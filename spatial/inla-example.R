library(INLA)
library(dplyr)

# Load processed data, merge it with spatial locations, rename 
# columns.  The bake-in is the same as the main data merge example
# and hopefully can be baked-out.

ke42d = readRDS('kenya-output/ke42.rds') %>%
  dplyr::mutate(country = substr(v000, 1, 2), phase = substr(v000, 3, 3), year = v007,
    method = method, unmet = unmettot, dhs_cluster = v001) %>%
  dplyr::select(country, phase, year, method, unmettot, dhs_cluster) %>%
  dplyr::group_by(dhs_cluster, phase, year) %>% 
  dplyr::summarise(
    not_using = sum(method == 0), modern = sum(method == 1), 
    traditional = sum(method == 2), exclude = sum(method == 9),
    unmet_need = sum(unmettot == 1), unknown = sum(is.na(method)), 
    total = n()
  ) %>% dplyr::left_join(
    y = maptools::readShapeSpatial('kenya-dhs/KEGE43FL')@data %>% 
      dplyr::select(DHSCLUST, CCFIPS, ADM1FIPS, ADM1FIPSNA, LATNUM, LONGNUM, DATUM) %>%
      dplyr::rename(dhs_cluster = DHSCLUST, country_fips_code = CCFIPS, 
        adm_1_fips_code = ADM1FIPS, adm_1_fips_name = ADM1FIPSNA, 
        latitude = LATNUM, longitude = LONGNUM, datum = DATUM) %>%
      dplyr::mutate(longitude = dplyr::if_else(longitude == 0, as.numeric(NA), longitude), 
        latitude = if_else(latitude == 0, as.numeric(NA), latitude)),
    by = c('dhs_cluster')
  ) %>% ungroup() %>% dplyr::mutate(all = (modern + traditional) )

# This is just the bounding box based on the data:
boundaries = sapply( X = ke42d[,c('longitude', 'latitude')], 
  FUN = function(x) c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))) %>%
  `rownames<-`(c('min','max'))

# This is the spatial data file.  I should use this to get boundaries of
# the country so that the mesh can be bounded.
# FIXME: NOT HERE YET

# Create a mesh for the modeling to happen on.  This is straightforward
# because at the country scale we can use a fine mesh and INLA will
# regularize it based on priors/model.  These numbers allow for smaller
# triangles and a finer mesh around highly-sampled areas.
mesh = inla.mesh.create.helper(
  points=ke42d[,c('longitude', 'latitude')], 
  cutoff=0.05, 
  offset=c(0.3, 0.6), 
  max.edge=c(0.4, 1.5)
)

# Create a lattice for predictions to go on. 
# FIXME: Currently this is an irregular mesh, I need to make a REGULAR
#        mesh based on the contour of the country from the shapefile
#        import above.... that code is in the splines spatial manipulation
#        file.
projection_mesh = inla.mesh.projector(
  mesh = mesh, loc = mesh$loc)

# I don't understand these priors but they're something generic 
# implemented in one of the INLA vignettes.  Spatial patterns in
# the estimated latent field show up with these so the next step is
# to ... understand them better and see if it's possible to get more
# structure to show up around the cities.
sigma0 = 1
range0 = 0.5
kappa0 = sqrt(8)/range0
tau0 = 1/(sqrt(4*pi)*kappa0*sigma0)

# This is creating the spatial strucutre from the priors.  I don't
# understand the details very well here although the background
# materials here are good so it won't be an issue.
# Among others use:
#  - https://arxiv.org/pdf/1802.06350.pdf
#  - http://www.maths.ed.ac.uk/~flindgre/rinla/isbaspde.pdf

spde = inla.spde2.matern(
  mesh = mesh,
  B.tau = cbind(log(tau0),1,0),
  B.kappa = cbind(log(kappa0),0,1),
  theta.prior.mean = c(0,0),
  theta.prior.prec = 1
)

# A is a matrix that projects from an m-vector of m mesh-point
# effects to an n-vector of n observation locations.  So A
# is n rows by m columns.  Locations are measurement locations
# so in our case cluster locations and index is observation index
# here we have no replicates although a final model will and that
# will allow us to estimate time-constant spatial effects.
A = inla.spde.make.A(
  mesh = mesh, 
  loc = as.matrix(ke42d[,c('longitude', 'latitude')]), 
  index = 1:nrow(ke42d), 
)

# A_predictive is a matrix that projects form an m-vector of m
# mesh-points to a p-vector of p lattice points (these are the 
# regularly spaced points we want predictions on s.t. we can
# make nice raster plots of the surface we are estimating.
# This depends on the projection_mesh locations (from above).
A_predictive = inla.spde.make.A(
  mesh = mesh,
  loc = projection_mesh$loc)

# A 'stack' is an INLA 'data' object that can be used both for
# dependent variables, covariates, spatial locations, etc... 
# it also holds the projection matrix A and validates it against
# the covariates we want to apply.  
# 
# The model we are shooting for here is a spatial effect plus
# an intercept.
data_stack = inla.stack(
  data = list(all = ke42d$all), 
  A = list(A), 
  effects = list(c(
    inla.spde.make.index("spatial", spde$n.spde),
    list(intercept = rep(1, spde$n.spde))
  )), 
  tag = 'estimation'
)

# This prediction stack is going to have the response imputed
# based on the model during fitting.  It holds the predictive
# projection matrix (A_predictive) and the same covariates as
# available (just the spatial ones).  It can have fewer covariates
# that the data model if they are not used for prediciton, or
# so it seems from the doc.
prediction_stack = inla.stack(
  data = list(all = rep(NA)), 
  A = list(A_predictive),
  effects = list(c(
    inla.spde.make.index("spatial", spde$n.spde),
    list(intercept = rep(1, spde$n.spde))
  )),
  tag = 'prediction'
)

# The nice thing about INLA and 'stack' objects is that
# R-INLA has a lot of helper functions for manipulating
# them.  Here we combine the two so that predictions are
# made during the estimation step.
stack = inla.stack(data_stack, prediction_stack)

# Estimation step, the model here is just an intercept 
# plus spatial model (matern covariance function, priors
# from above.  The family is 'logistic' but I can break
# out components of 'cpr' and use the 'binomial' family
# to get the right link function in prediction for free.
#
# - Hm... don't recall exactly why the control.predictor "A"
#   is required.  
# - The link control.predictor is required only
#   because the logistic family assumes an identity link...
#   because it's the wrong family. 
# - The 'compute' control.predictor makes sure posterior
#   predictors are computed (so that the NA's in the 
#   prediction stack are filled in.
inla_m = inla(
  formula = all ~ 1 + f(spatial, model = spde), 
  family = 'binomial',
  Ntrials = c(ke42d[['total']], rep(mean(ke42d[['total']]), 1511)),
  data = inla.stack.data(stack, spde = spde), 
  control.family = list(
    link = 'logit'
  ),
  control.predictor = list(
    A = inla.stack.A(stack),
    link = 1,
    compute = TRUE
  )
)

# This is the non-transformed estimated spatial field.  It ignores
# the intercept and it ignores the transform.
inla_field = inla.spde2.result(
  inla = inla_m, 
  name = 'spatial', 
  spde = spde
)

# We project this field onto a grid and show it.... hey, 
# there's _some_ kind of spatial pattern.
grid = inla.mesh.projector(mesh, dims=c(200, 200))
grid_projection = inla.mesh.project(grid, inla_field$summary.values$mean) %>% 
  data.frame(check.names=FALSE) %>%
  dplyr::mutate(latitude = grid$y) %>% 
  tidyr::gather(longitude_index, value, -latitude) %>%
  dplyr::mutate(longitude = grid$x[as.numeric(longitude_index)]) %>%
  dplyr::filter(!is.na(value))

pl_field <- ggplot(
  data=grid_projection, 
  aes(x=longitude, y=latitude, fill=value)
) + geom_raster()


# This gets the stack indexes for the filled-in NA's and 
# uses them to pull out NA values only for the prediction points
prediction_index <- inla.stack.index(stack, 'prediction')$data
prediction_values <- inla_m$summary.fitted.values$mean
predictions_only <- prediction_values[prediction_index]

# Re-project prediction points onto a regular lattice, 
# then munge.
prediction_grid_projection = inla.mesh.project(grid, predictions_only) %>% 
  data.frame(check.names=FALSE) %>%
  dplyr::mutate(latitude = grid$y) %>% 
  tidyr::gather(longitude_index, value, -latitude) %>%
  dplyr::mutate(longitude = grid$x[as.numeric(longitude_index)]) %>%
  dplyr::filter(!is.na(value))

# Grab estimated uncertainty
uncertainty_index <- inla.stack.index(stack, 'prediction')$data
uncertainty_values <- inla_m$summary.fitted.values$sd
uncertainty_only <- uncertainty_values[uncertainty_index]

# Re-project prediction points onto a regular lattice, 
# then munge.
uncertainty_grid_projection = inla.mesh.project(grid, uncertainty_only) %>% 
  data.frame(check.names=FALSE) %>%
  dplyr::mutate(latitude = grid$y) %>% 
  tidyr::gather(longitude_index, uncertainty, -latitude) %>%
  dplyr::mutate(longitude = grid$x[as.numeric(longitude_index)]) %>%
  dplyr::filter(!is.na(uncertainty))

grid_projection <- prediction_grid_projection %>% dplyr::select(-longitude_index) %>%
  dplyr::left_join(y = uncertainty_grid_projection %>% dplyr::select(-longitude_index))

# Get spatial data for Kenya, filter out older divisions, tranform
# to a data frame so I can use it with ggplot2.
kenya_geojson_url = "https://api.dhsprogram.com/rest/dhs/geometry/KE?f=geojson"
download.file(url = kenya_geojson_url, destfile='/tmp/admin-2-kenya.geojson')
kenya_map <- geojsonio::geojson_read('/tmp/admin-2-kenya.geojson', what='sp')


filtering_data_url <- paste0("https://api.dhsprogram.com/rest/",
  "dhs/data/KE,SV_BACK_W_NUM?f=csv&breakdown=subnational&f=csv&perpage=3000")

filtering_data <- read.csv(file=filtering_data_url, stringsAsFactors=FALSE) %>%
  dplyr::filter(grepl('^\\.\\.', x=CharacteristicLabel)) %>%
  dplyr::select(RegionId) %>% unlist

kenya_map <- kenya_map %>% 
  subset(CountryName == "Kenya" & RegionID %in% filtering_data) %>%
  ggplot2::fortify()

# Plot spatial estimates with overlay of Kenya divisions
# and data.
pallette <- c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')

pl_prediction_field <- ggplot() + geom_raster(
  data = grid_projection, 
  aes(x=longitude, y=latitude, fill=value, alpha = 1 - uncertainty)
) + geom_path(
  data = kenya_map,
  aes(x=long, y=lat, group = paste(group, piece)), 
  colour='black'
) + geom_point(
  data = ke42d,
  aes(x=longitude, y=latitude, colour = all/total)
) + theme_minimal() +
    scale_x_continuous("longitude") +
    scale_y_continuous("latitude") +
    scale_fill_gradientn("CPR, est.", 
      colours = pallette, values = c(0, 0.25, 0.5, 0.75, 1.0)
    ) +
    scale_colour_gradientn("CPR, observed.",
      colours = pallette, values = c(0, 0.25, 0.5, 0.75, 1.0),
      guide = "none"
    ) +
    ggtitle("Kenya, survey 4-2, estimated CPR") 













