library(INLA)
library(dplyr)

## Load processed data, merge it with spatial locations, rename 
#  columns.
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
  ) %>% ungroup() %>% dplyr::mutate(cpr = (modern + traditional) / total)

boundaries = sapply( X = ke42d[,c('longitude', 'latitude')], 
  FUN = function(x) c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))) %>%
  `rownames<-`(c('min','max'))

mesh = inla.mesh.create.helper(
  points=ke42d[,c('longitude', 'latitude')], 
  cutoff=0.05, 
  offset=c(0.3, 0.6), 
  max.edge=c(0.4, 0.5)
)

sigma0 = 1
range0 = 0.5
kappa0 = sqrt(8)/range0
tau0 = 1/(sqrt(4*pi)*kappa0*sigma0)

spde = inla.spde2.matern(
  mesh = mesh,
  B.tau = cbind(log(tau0),1,0),
  B.kappa = cbind(log(kappa0),0,1),
  theta.prior.mean = c(0,0),
  theta.prior.prec = 1
)

A = inla.spde.make.A(
  mesh = mesh, 
  loc = as.matrix(ke42d[,c('longitude', 'latitude')]), 
  index = 1:nrow(ke42d), 
  repl = rep(1, nrow(ke42d))
)

spde = inla.spde2.matern(
  mesh = mesh,
  B.tau = cbind(log(tau0),1,0),
  B.kappa = cbind(log(kappa0),0,1),
  theta.prior.mean = c(0,0),
  theta.prior.prec = 1
)


stk = inla.stack(
  data = list(resp = ke42d$cpr), 
  A = list(A,1), 
  effects = list(
    i = 1:spde$n.spde, 
    m = rep(1:nrow(ke42d))
  ), 
  tag = 'est'
)

inla_m = inla(
  formula = ke42d$cpr ~ 0 + f(i, model=spde_model), 
  family='logistic', 
  data=inla.stack.data(stk), 
  control.predictor=list(A=inla.stack.A(stk))
)

inla_field = inla.spde2.result(
  inla = inla_m, 
  name = 'i', 
  spde = spde
)


grid = inla.mesh.projector(mesh, dims=c(200, 200))

grid_projection = inla.mesh.project(grid, inla_field$summary.values$mean)














