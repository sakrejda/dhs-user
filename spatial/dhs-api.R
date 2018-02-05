
kenya_geojson_url = url("https://api.dhsprogram.com/rest/dhs/geometry/KE?f=geojson")
download.file(url = kenya_geojson_url, destfile='/tmp/admin-2-kenya.geojson')

kenya_map <- geojsonio::geojson_read('/tmp/admin-2-kenya.geojson', what='sp')


kenya_2014_subregion_cpr_url <- paste0("https://api.dhsprogram.com/rest/dhs/data", 
  "?breakdown=subnational&indicatorIds=FP_CUSM_W_ANY&surveyIds=KE2014DHS&",
  "lang=en&returnGeometry=false&f=csv")

kenya_2014_subregion_cpr <- read.csv(file=kenya_2014_subregion_cpr_url,
  stringsAsFactors=FALSE)
 
kenya_map_2014 <- subset(kenya_map, CountryName == "Kenya" & SurveyId == "KE2014DHS")
kenya_map_2014@data <- dplyr::left_join(x = kenya_map_2014@data, 
  y = dplyr::select(kenya_2014_subregion_cpr, "RegionId", "Value"), 
  by = c(RegionID = 'RegionId')) 



library(leaflet)
palette <- colorNumeric("viridis", NULL)

regions <- unique(kenya_map_2014@data$RegionID)

for (region in regions) {
  map <- leaflet(
    data = kenya_map_2014 %>% subset(RegionID != 'KEDHS2014416004'),
  ) %>% 
    addTiles() %>% 
    addPolygons(stroke=TRUE, smoothFactor=0.3, 
      fillOpacity=~ifelse(RegionID == region, 1.0, 0.1), 
      fillColor=~palette(Value), 
      label = ~substr(RegionID, 10, 15), weight=0.8, ) %>%
    addLegend(pal = palette, values = ~Value, opacity = 1.0,
    labFormat = labelFormat(transform = function(x) round(x, 2)))
  print(region)
  print(map)
  readline()
}



