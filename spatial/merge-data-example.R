
# Load data from two surveys (KEIR42 and KEIR52), as processed by
# Phillip's code (more or less our edits of it).  Merge it with
# spatial location from two spatial files (KEGE43 and KEGE52)
# based on cluster id.  Rename columns.
#
#
# Baked in assumptions:
# 
# - dhs_cluster == v001
# - spatial data names: DHSCLUST, CCFIPS, ADM1FIPS, ADM1FIPSNA, LATNUM, LONGNUM,
#                     DATUM
# - longitude == 0 => NA
# - latitude == 0 => NA
# - spatial data file must match survey data file (makes sense, it's 
#   a table for dhs_cluter => (longitude, latitude)      
#
# - one data set (.rds) and one spatial file (__GE____FL) per survey
# - data sets from multiple years: a) are combined using rbind; and 
#   b) have a column indicating year ("year")
#
# - spatial layer is _not_ tied to survey or year, and it is currently 
#   pulled from DHS API.
#
#   TODO: This code will run after the initial import steps so the baked-in
#         assumptions can be removed.  Inputs (for a single survey) will be a 
#         data file and a spatial file, they will be merged with standard
#         columns.  Not much code left here once the assumptions are
#         ... baked out and moved upstream to the data import code.
#
 
 
ke42d <- readRDS('kenya-output/ke42.rds') %>%
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
  )

ke52d <- readRDS('kenya-output/ke52.rds') %>% 
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
    y =  maptools::readShapeSpatial('kenya-dhs/KEGE52FL')@data %>% 
      dplyr::select(DHSCLUST, CCFIPS, ADM1FIPS, ADM1FIPSNA, LATNUM, LONGNUM, DATUM) %>%
      dplyr::rename(dhs_cluster = DHSCLUST, country_fips_code = CCFIPS, 
        adm_1_fips_code = ADM1FIPS, adm_1_fips_name = ADM1FIPSNA, 
        latitude = LATNUM, longitude = LONGNUM, datum = DATUM) %>%
      dplyr::mutate(longitude = dplyr::if_else(longitude == 0, as.numeric(NA), longitude), 
        latitude = if_else(latitude == 0, as.numeric(NA), latitude)),
    by = c('dhs_cluster')
  )

data <- rbind(ke42d, ke52d)
n <- colnames(data)
attributes(data) <- NULL
names(data) <- n
data <- data.frame(data)
saveRDS(data, file='kenya-data.rds')








