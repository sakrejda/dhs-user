library(magrittr)
set.seed(100)

n_knots_m <- 10
n_knots_p <- 18
n_knots_in_grid <- n_knots_m * n_knots_p

knot_points_m <- seq(from=0, to=100, length.out=n_knots_m)
knot_points_p <- seq(from=0, to=180, length.out=n_knots_p)

knot_mu <- 0
knot_sd <- 2
knot_value_grid <- matrix(data=plogis(rnorm(n_knots_in_grid, knot_mu, knot_sd)),
  nrow = n_knots_m, ncol = n_knots_p)
knot_value_long <- knot_value_grid %>% data.frame %>% 
  `colnames<-`(paste0(1:n_knots_p)) %>% 
  dplyr::mutate(M=as.numeric(1:n_knots_m)) %>% 
  tidyr::gather(P, value, -M) %>%
  dplyr::mutate(P=as.numeric(P)) %>% 
  dplyr::mutate(m=knot_points_m[M], p=knot_points_p[P]) %>%
  dplyr::arrange(P, M)

measure_points_m <- seq(from=0, to=100, length.out=200)
measure_points_p <- seq(from=0, to=180, length.out=200)
measurements_long <- expand.grid(m=measure_points_m, p=measure_points_p)

bs_m <- splines::bs(x=measurements_long$m, knots=knot_points_m, degree=5, 
  Boundary.knots=c(knot_points_m[1] - diff(knot_points_m)[1], 
                   knot_points_m[n_knots_m] + diff(knot_points_m)[n_knots_m-1]))
bs_p <- splines::bs(x=measurements_long$p, knots=knot_points_p, degree=5, 
  Boundary.knots=c(knot_points_p[1] - diff(knot_points_p)[1], 
                   knot_points_p[n_knots_p] + diff(knot_points_p)[n_knots_p-1]))

is <- vector(mode='numeric')
js <- vector(mode='numeric')
xs <- vector(mode='numeric')

for (m in 1:n_knots_m) {
  for (p in 1:n_knots_p) {
    j <- (m - 1) * n_knots_p + p
    y <- bs_m[,m] * bs_p[,p]
    is_ <- which(y > 1e-8)
    js_ <- rep(j, length(is_))
    xs_ <- y[is_]
    is <- c(is, is_)
    js <- c(js, js_)
    xs <- c(xs, xs_)
  }
}

bs_p_m <- Matrix::sparseMatrix(i=is, j=js, x=xs, 
  dims=c(nrow(measurements_long), n_knots_in_grid))

measurements_long <- measurements_long %>% 
  dplyr::mutate(observation = as.vector(bs_p_m %*% knot_value_long[,'value']),
                prediction = as.vector(bs_p_m %*% MatrixModels:::lm.fit.sparse(bs_p_m, observation)))

library(ggplot2)

picks <- sample(1:nrow(measurements_long), 200)
pl <- ggplot(data=measurements_long) +
geom_raster(
  aes(x=m, y=p, fill=observation)) + 
geom_point(
  data=measurements_long[picks,],
  aes(x=m, y=p, colour=prediction), size=2.5, shape=15)  +
geom_point(
  data=measurements_long[picks,],
  aes(x=m, y=p), colour='black', size=2.5, shape=0) 


stan_data <- rstan::extract_sparse_parts(bs_p_m)
stan_data[['N']] <- nrow(measurements_long)
stan_data[['y']] <- measurements_long[['observation']] + rnorm(n=stan_data[['N']], 0, 1)
#stan_data[['m']] <- measurements_long[['m']]
#stan_data[['p']] <- measurements_long[['p']]
stan_data[['n_w']] <- length(stan_data[['w']])
stan_data[['n_v']] <- length(stan_data[['v']])
stan_data[['n_u']] <- length(stan_data[['u']])

stan_data[['n_knots_m']] <- n_knots_m
stan_data[['n_knots_p']] <- n_knots_p

stan_data[['knot_points_m']] <- knot_points_m
stan_data[['knot_points_p']] <- knot_points_p

rstan::stan_rdump(list=names(stan_data), file='data.rdump', 
  envir=list2env(stan_data))

#m <- rstan::stan_model(file='sparse-lm.stan')
#s <- rstan::sampling(m, data=stan_data, chains=1, iter=100, warmup=200,
#  thin=1, seed=12, verbose=TRUE)

s <- rstan::read_stan_csv(csvfiles='output.csv') %>% rstan::extract(permuted=TRUE)
a <- apply(s$knot_weights, 2, mean)
plot(a, MatrixModels:::lm.fit.sparse(bs_p_m, measurements_long$observation), ylim=c(0,2), xlim=c(0,2))




