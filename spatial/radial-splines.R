



basis_function = function(r, h) {
  o <- vector(mode = 'numeric', length = length(r))
  if (length(r) > 1 && length(h) == 1) 
    h = rep(h, length(r))
  for (i in 1:length(o)) {
    if (r[i] <= h[i]) {
      o[i] = (h[i]^3 + 3 * h[i]^2 * (h[i] - r[i]) + 
        3 * h[i] * (h[i] - r[i])^2 - 3 * (h[i] - r[i])^3)/(4*h[i]^2)
    } else if (r[i] > h[i] && r[i] < (2 * h[i])) {
      o[i] = (2 * h[i] - r[i])^3 / (4 * h[i]^2)
    }
  }
  return(o)
}

basis = function(x, p, b, h) {
  o = matrix(data = NA, nrow = length(x), ncol = length(p))
  for (i in 1:length(p)) {
    o[,i] = basis_function(abs(x - p[i]), h[i])
  }
  o = apply(o, 1, function(x) sum(b * x))
  return(o)
}

## 2d check
grid <- expand.grid(x = seq(-40, 40, 1), y=seq(-40, 40, 1), U=0)
line <- seq(from = -40, to = 40, by = 0.01)
line_U <- vector(mode = 'numeric', length = length(line))
h = 0.95
for ( i in 1:nrow(grid)) for( j in 1:length(line)) {
  r = sqrt((grid[['x']][i] - line[j])^2 + (grid[['y']][i] - line[j])^2)
  if (r > 2 * h) 
    next
  line_U[j] = line_U[j] + basis_function(r, h)
}

lines(line, line_U, type = 'l')


line <- seq(from = -40, to = 40, by = 0.01)
line_U <- vector(mode = 'numeric', length = length(line))
h = 0.95
for ( i in 1:nrow(grid)) for( j in 1:length(line)) {
  r = sqrt((grid[['x']][i] - line[j])^2 + (grid[['y']][i] - 0)^2)
  if (r > 2 * h) 
    next
  line_U[j] = line_U[j] + basis_function(r, h)
}

plot(line, line_U, type = 'l')




