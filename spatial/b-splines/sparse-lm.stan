data {
  int N;
  int M;
  int y[N];
  int n[N];

  int n_w;
  vector[n_w] w;

  int n_v;
  int v[n_v];

  int n_u;
  int u[n_u];


}

parameters {
  vector[M] knot_weights;
  real<lower=0> sigma;
}

model {
  sigma ~ gamma(4, 10);
  knot_weights ~ normal(0, 1);

  y ~ binomial(n, csr_matrix_times_vector(N, M, w, v, u, knot_weights));
}



