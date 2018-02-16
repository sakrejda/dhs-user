
bounding_box <- function(data) with(data = data, expr = list(
  longitude = c(min(longitude, na.rm = TRUE), max(longitude, na.rm = TRUE)), 
  latitude = c(min(latitude, na.rm = TRUE), max(latitude, na.rm = TRUE))
))

grid <- function(box, spacing, padding=NULL) {
  if (!is.null(padding)) {
    box[['longitude']][1] = box[['longitude']][1] - padding[['longitude']][1]
    box[['longitude']][2] = box[['longitude']][2] + padding[['longitude']][2]
    box[['latitude']][1] = box[['latitude']][1] - padding[['latitude']][1]
    box[['latitude']][2] = box[['latitude']][2] + padding[['latitude']][2]
  }
  grid <- expand.grid(
    longitude = seq(from = box[['longitude']][1], to = box[['longitude']][2], by=spacing),
    latitude = seq(from = box[['latitude']][1], to = box[['latitude']][2], by=spacing)
  )
  return(grid)
}

knot_grid <- function(map, spacing, padding) { 
  box <- map %>% fortify %>% dplyr::rename(
    longitude = long, latitude = lat) %>% bounding_box
  grid <- grid(box, spacing, padding)
  return(grid)
}

knot_filter <- function(grid, points, scale) {
  weights <- apply(X = grid, MARGIN = 1, FUN = function(grid_pt, filter_pts, scale) {
    d <- mvtnorm::dmvnorm(x=filter_pts, mean=grid_pt, sigma=diag(c(scale,scale)))
    return(sum(d, na.rm=TRUE))
  }, filter_pts = points, scale = scale)
  return(weights)
}




