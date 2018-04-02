
surveys <- readRDS('standardized-survey-data.rds')
located_clusters <- readRDS('kenya-located-clusters.rds')

located_surveys <- dplyr::left_join(
  x = surveys, 
  y = located_clusters %>% dplyr::select(cluster_id, year, county_name, 
    area, perimeter, longitude, latitude, stratum, county_idx)
)

saveRDS(located_surveys, file='located-surveys.rds')

model_data <- list(
  n_obs = nrow(located_surveys),
  n_levels = 2,  # cluster, county  


