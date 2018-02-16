## Load processed data, merge it with spatial locations, rename 
#  columns.
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


## Need to get map data, filter out older divisions, etc...
kenya_geojson_url = "https://api.dhsprogram.com/rest/dhs/geometry/KE?f=geojson"
download.file(url = kenya_geojson_url, destfile='/tmp/admin-2-kenya.geojson')
kenya_map <- geojsonio::geojson_read('/tmp/admin-2-kenya.geojson', what='sp')


filtering_data_url <- paste0("https://api.dhsprogram.com/rest/",
  "dhs/data/KE,SV_BACK_W_NUM?f=csv&breakdown=subnational&f=csv&perpage=3000")

filtering_data <- read.csv(file=filtering_data_url, stringsAsFactors=FALSE) %>%
  dplyr::filter(grepl('^\\.\\.', x=CharacteristicLabel)) %>% 
  dplyr::select(RegionId) %>% unlist

kenya_map_2014 <- kenya_map %>% subset(CountryName == "Kenya" & RegionID %in% filtering_data)


## Map plot with data by year.
pl <- ggplot() + geom_polygon(
  data = fortify(kenya_map_2014), 
  aes(x = long, y = lat, group = group), 
  colour = 'black', fill = 'white'
) + coord_cartesian() + geom_jitter(
  data = data, 
  aes(x = longitude, y = latitude, colour = (traditional + modern)/total)
) + facet_wrap( ~ year) + 
    theme_minimal() +
    coord_fixed(ratio = 1, xlim = c(33, 42.5), ylim = c(-5, 5.5)) +
    scale_colour_continuous("CPR", limits=c(0,1)) +
    scale_x_continuous("longitude") +
    scale_y_continuous("latitude")

pdf(file='/tmp/keyna-dhs-w-map.pdf', width=32, height=9)
print(pl); dev.off()

# Not using leaflet at the moment.
#library(leaflet)
#palette <- colorNumeric("viridis", NULL)
#
#regions <- unique(kenya_map_2014@data$RegionID)
#
#for (region in regions) {
#  map <- leaflet(
#    data = kenya_map_2014 %>% subset(RegionID != 'KEDHS2014416004'),
#  ) %>% 
#    addTiles() %>% 
#    addPolygons(stroke=TRUE, smoothFactor=0.3, 
#      fillOpacity=~ifelse(RegionID == region, 1.0, 0.1), 
#      fillColor=~palette(Value), 
#      label = ~substr(RegionID, 10, 15), weight=0.8, ) %>%
#    addLegend(pal = palette, values = ~Value, opacity = 1.0,
#    labFormat = labelFormat(transform = function(x) round(x, 2)))
#  print(region)
#  print(map)
#  readline()
#}

## Create spatial grid for spline model knots:

# Sparse points will need to be augmented by additional points in high 
# population density areas, but the model could fit based on these now.
sparse_points <- knot_grid(kenya_map_2014, spacing = 1.8, padding = list(longitude = c(1,1), latitude=c(2,1)))

dense_points <- knot_grid(kenya_map_2014, spacing = .1, padding = list(longitude = c(1,1), latitude=c(2,1)))
dense_weights <- knot_filter(as.matrix(pts[,c('longitude', 'latitude')]), as.matrix(data[,c('longitude', 'latitude')]), scale=0.5)

sparsed_points <- dense_points[sample(x=1:nrow(dense_points), size=50, replace=FALSE, prob=dense_weights),]

grid_points <- rbind(sparsed_points, sparse_points)
dev.off(); plot(grid_points$longitude, grid_points$latitude); plot(kenya_map_2014, add=TRUE)








