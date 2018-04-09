
n_forecast_years = 15

surveys <- readRDS('standardized-survey-data.rds')
located_clusters <- readRDS('kenya-located-clusters.rds')

located_surveys <- dplyr::left_join(
  x = surveys, 
  y = located_clusters %>% dplyr::select(cluster_id, year, country, county_name, 
    area, perimeter, longitude, latitude, stratum, county_idx),
  by = c('country', 'cluster_id', 'year')                                
) %>% ungroup() %>% dplyr::mutate(
  cluster_idx = factor(paste(year, cluster_id)) %>% as.numeric,
  household_type_idx = factor(household_type) %>% as.numeric,
  year_idx = factor(year, levels = min(year):(max(year) + n_forecast_years)) %>% as.numeric,
  county_idx = ifelse(is.na(county_idx), max(county_idx, na.rm=TRUE) + 1, county_idx),
  survey_year_idx = factor(year) %>% as.numeric
) 

saveRDS(located_surveys, file='located-surveys.rds')

unit_summaries <- located_surveys %>% 
  dplyr::group_by(county_idx, year_idx) %>% 
  dplyr::summarise()                                          

saveRDS(unit_summaries, file = 'sampled-unit-summaries.rds')

idx <- list()
l <- vector(mode='numeric', length = nrow(unit_summaries))
for (i in 1:nrow(unit_summaries)) {
  idx[[i]] = located_surveys %>% dplyr::mutate(idx = 1:n()) %>%
    dplyr::filter(
      county_idx == unit_summaries[['county_idx']][i],
      year_idx == unit_summaries[['year_idx']][i]
    ) %>% dplyr::select(idx) %>% unlist(use.names=FALSE)
  l[i] = length(idx[[i]])
}

ends <- cumsum(l)
starts <- cumsum(l) - l + 1

stan_env <- new.env()
with(data = stan_env, expr = {
  n_obs = nrow(located_surveys)

  n_years = max(located_surveys[['year']]) - min(located_surveys[['year']]) + 1
  n_forecast_years = n_forecast_years
  year = min(located_surveys[['year']]):(max(located_surveys[['year']]) + n_forecast_years)

  n_weighted = nrow(located_surveys)
  weighted = 1:nrow(located_surveys)

  n_yes = located_surveys[['cu_modern']]
  n_asked = located_surveys[['n']]
  weights = located_surveys[['weight']]

  cluster_idx = located_surveys[['cluster_idx']]
  county_idx = located_surveys[['county_idx']]
  hh_type_idx = located_surveys[['household_type_idx']]
  year_idx = located_surveys[['year_idx']]
  survey_year_idx = located_surveys[['survey_year_idx']]

  n_sampled = nrow(unit_summaries)
  sampled_unit_idx = unit_summaries[['county_idx']]
  sampled_year_idx = unit_summaries[['year_idx']]
  sampled_start_idx = starts
  sampled_stop_idx = ends
  sampled_cross_idx = do.call(what=c, args=idx)
})


saveRDS(stan_env, file = 'stan-data-env.rds')
rstan::stan_rdump(list = ls(stan_env), file = 'stan-data.rdump', envir = stan_env)


