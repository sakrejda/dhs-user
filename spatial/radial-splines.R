



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
  o = apply(o, 1, function(x) sum( (b/sum(x)) * x)/sum(x))
  return(o)
}




