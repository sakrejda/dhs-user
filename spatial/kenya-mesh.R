# Assuming subdirectories are:
# 1) 'gps' for county shapefiles and for cluster location data.
# 2) 'survey' for individual recode survey files

## Some of these libraries are hard to install.  That sucks (for you).
libs <- c('maptools', 'ggplot2', 'dplyr', 
          'magrittr', 'ggrepel', 'INLA',
          'rmapshaper', 'geojson', 'geojsonio',
          'rgdal', 'spdep')
for (l in libs) library(l, character.only=TRUE)

spatial_data <- 'gps'
survey_data <- 'survey'


# We need country-level data (using from GADM here) so that 
# we can have a spatial mesh that respects the country bounaries
# without distortion.  The country-level map needs to be simplified
# to avoid tiny triangles at the boundaries, and we cut out
# islands for the same reason.

country_shape <- readRDS(file.path(spatial_data, 'KEN_adm0.rds'))
country_json <- geojsonio::geojson_json(country_shape) %>%
  rmapshaper::ms_filter_islands(min_area = 0.02) %>%
  rmapshaper::ms_simplify(keep = 0.008) %>% 
  rmapshaper::ms_filter_islands(min_area = 0.2, min_vertices=100)
geojsonio::geojson_write(input = country_json, file = 'kenya-country.geojson')

country_shape <- 'kenya-country.geojson' %>% 
  geojsonio::geojson_read(method='local', what = 'sp') 


# In the INLA mesher we need to pass the simplified country boundary 
# as a inla.mesh.segment object
country_boundaries <-
  inla.mesh.2d(boundary=list(country_shape), cutoff = 0.2, max.edge = .8) %>%
  inla.mesh.boundary()

saveRDS(country_boundaries, file = 'kenya-country-mesh-boundary.rds')


# We need county-level shapes and labels, including places to put labels
# in multiple formats, the objects at the county level are:
#   1) county_shapes - SpatialPolygonsDataFrame for county data
#   5) county_json - same data, geojson format.
#   3) county_shapes_df - ggplot-friendly version of county shapes
#   4) county_data_df - basic county-level data for merging,
#        includes centroids.
county_shapes <- rgdal::readOGR(spatial_data, 'kenya-counties')
county_shapes@data <- county_shapes@data %>% 
  dplyr::rename(id = OBJECTID, area = AREA, perimeter = PERIMETER,
                county_3_id = COUNTY3_, county_3_id_2 = COUNTY3_ID) %>%
  dplyr::mutate(county_name = as.character(COUNTY))

county_shapes_df <- ggplot2::fortify(county_shapes) %>%
  dplyr::rename(longitude = long, latitude = lat)

county_json <- geojsonio::geojson_json(county_shapes) %>%
  rmapshaper::ms_simplify()

county_centroids = rgeos::gCentroid(county_shapes, byid=TRUE, id=county_shapes@data$county_name)
county_centroids_df = data.frame(
  county_name = rownames(county_centroids@coords), 
  longitude = county_centroids@coords[,1],
  latitude = county_centroids@coords[,2])

county_data_df = county_shapes@data %>% 
  dplyr::left_join(county_centroids_df, by='county_name') %>%
  dplyr::arrange(latitude, longitude) %>% 
  dplyr::mutate(county_idx = 1:n())

saveRDS(county_data_df, file = 'kenya-county-data.rds')

# We locate the clusters to the county map s.t. we can fit county
# effects and apply them to cluster.
clusters <- readRDS('gps/merged-points.rds') %>% do.call(rbind, .) %>%
  dplyr::mutate(row_id = 1:n())
obs_locations = clusters[,c('longitude', 'latitude')] %>% as.matrix
rownames(obs_locations) <- clusters[['row_id']]
misplaced_locations <- apply(obs_locations, 1, function(x) any(is.na(x)))
sp_locations = SpatialPointsDataFrame(
  coords = obs_locations[!misplaced_locations,], 
  proj4string = sp::CRS(proj4string(county_shapes)),
  data = dplyr::select(clusters, row_id, cluster_id, stratum, year, 
    country, stratum) %>% `[`(!misplaced_locations, )
)

located_clusters = sp::over(sp_locations, county_shapes) %>%
  dplyr::mutate(row_id = sp_locations@data[['row_id']]) %>%
  dplyr::select(row_id, id, county_name, area, perimeter) %>%
  dplyr::right_join(y = clusters, by = c('row_id')) %>%
  dplyr::left_join(
    y = dplyr::select(county_data_df, county_idx, county_name),
    by = c('county_name')
  )

saveRDS(located_clusters, file = 'kenya-located-clusters.rds')

# The mesh  _also_ needs to respect where population
# is so we us the country boundary as the inner boundary and the
# sampling points to figure out where to concentrate triangles.
kenya_mesh <- inla.mesh.2d(
  loc = obs_locations, 
  interior = country_boundaries,  ## here's where the country boundary comes in.
  cutoff = 0.1, 
  max.edge=c(0.6)
)

saveRDS(kenya_mesh, file = 'kenya-mesh.rds')

# Quick visualization of the mesh:  plot(kenya_mesh, asp=1)

# Extract the mesh into ggplot-compatible data frame:
idx = cbind(kenya_mesh$graph$tv[, c(1:3, 1), drop=FALSE], NA)
x = kenya_mesh$loc[t(idx), 1]
y = kenya_mesh$loc[t(idx), 2]
kenya_triangles_df <- data.frame(x=x, y=y, group=1:length(x))

saveRDS(kenya_mesh, file = 'kenya-mesh-df.rds')

# The county-level map with labels confirms (vs. online maps) 
# that the right county-level data goes with the right shape:

pl_county_map <- ggplot() + geom_polygon(
  data = county_shapes_df, 
  aes(x=longitude, y=latitude, group=group), 
  fill='white', colour='black'
) + geom_label_repel(
  data = county_data_df, 
  aes(x=longitude, y=latitude, label=county_name), 
  fill='white',
) + theme_minimal() + coord_fixed()


## Visualize the mesh to see that it's doing what we want
#  _with_ county labels here.
pl_county_map_w_mesh <- ggplot() + geom_polygon(
  data = county_shapes_df, 
  aes(x=longitude, y=latitude, group=group), 
  fill='white', colour='black'
) + geom_path(
  data = kenya_triangles_df, 
  aes(x=x, y=y), 
  alpha = 0.1
) + geom_label_repel(
  data = county_data_df, 
  aes(x=longitude, y=latitude, label=county_name), 
  fill='white'
) + theme_minimal() + coord_fixed()

# Without county labels
pl_county_map_w_mesh_no_label <- ggplot() + geom_polygon(
  data = county_shapes_df, 
  aes(x=longitude, y=latitude, group=group), 
  fill='white', colour='black'
) + geom_path(
  data = kenya_triangles_df, 
  aes(x=x, y=y), 
  alpha = 0.1
) + theme_minimal() + coord_fixed()

# Print out:
pdf(file='kenya-map-with-mesh.pdf', height=12, width=12); print(pl_county_map_w_mesh); dev.off()
pdf(file='kenya-map-with-mesh-no-label.pdf', height=12, width=12); print(pl_county_map_w_mesh_no_label); dev.off()

# Now that it looks good, collect the goods.  Specifically, 
# 'connections' contains a matrix with one connection per
# row from node connections[,1] to connections[,2] and each
# connection appears only once so this is in effect an
# undirected graph describing the neighborhood as required for
# implementing ICAR in Stan.
connection_list = kenya_mesh$graph$vv %>% Matrix::tril() %>% 
  apply(1, function(x) which(x == 1))
connections = mapply(FUN = function(i, j) if(length(j) != 0) cbind(i, j) else NULL,
  i = 1:length(connection_list), j = connection_list) %>%
  do.call(what = rbind, args = .)

saveRDS(connections, file = 'kenya-mesh-connections.rds')

# Now we need to create a projection from the mesh points to the
# cluster locations:
kenya_mesh_to_clusters = inla.spde.make.A(kenya_mesh, obs_locations)
irrelevant_mesh_points = which(colSums(kenya_mesh_to_clusters) == 0) # could drop?

saveRDS(kenya_mesh_to_clusters, file = 'project-kenya-mesh-to-clusters.rds')

# We also want county-level connections list for the county-level 
# ICAR model.

county_neighbors <- spdep::poly2nb(
  pl = county_shapes, 
  row.names = county_shapes@data[['county_name']],
  snap = 0.01)

saveRDS(county_neighbors, file = 'kenya-county-neighborhoods.rds')

names(county_neighbors) <- attr(county_neighbors, 'region.id')
named_county_neighbors <- lapply(county_neighbors, function(x) x <- names(county_neighbors)[x])

county_connections =  mapply(FUN = function(i, j) if(length(j) != 0) cbind(i, j) else NULL,
    i = 1:length(county_neighbors), j = county_neighbors) %>%
  do.call(what = rbind, args = .) %>% 
  apply(1, sort) %>% t %>% `[`(order(.[,1]),) %>% 
  data.frame %>% unique

saveRDS(county_connections, file = 'kenya-county_connections.rds')


