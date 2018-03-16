
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








