
## Create spatial grid for spline model knots:

# Sparse points will need to be augmented by additional points in high 
# population density areas, but the model could fit based on these now.
sparse_points <- knot_grid(kenya_map_2014, spacing = 1.8, padding = list(longitude = c(1,1), latitude=c(2,1)))

dense_points <- knot_grid(kenya_map_2014, spacing = .1, padding = list(longitude = c(1,1), latitude=c(2,1)))
dense_weights <- knot_filter(as.matrix(pts[,c('longitude', 'latitude')]), as.matrix(data[,c('longitude', 'latitude')]), scale=0.5)

sparsed_points <- dense_points[sample(x=1:nrow(dense_points), size=50, replace=FALSE, prob=dense_weights),]

grid_points <- rbind(sparsed_points, sparse_points)
dev.off(); plot(grid_points$longitude, grid_points$latitude); plot(kenya_map_2014, add=TRUE)








