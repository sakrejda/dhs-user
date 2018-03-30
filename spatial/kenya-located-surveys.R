
surveys <- readRDS('standardized-survey-data.rds')
located_clusters <- readRDS('kenya-located-clusters.rds')

located_surveys <- dplyr::left_join(
  x = surveys, 
  y = located_clusters %>% dplyr::select(cluster_id, year, county_name, 
    area, perimeter, longitude, latitude, stratum, county_idx)
)


