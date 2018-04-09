/* REV-01
 *
 */

functions {
  int cast_to_int(real x, int magnitude) {
    real precision = sqrt(machine_precision());
    int y = 0;
    real x_copy = x;
    int x_sign = 1;

    if (x < 0.0) {
      x_sign = -1;
      x_copy = -x;
    } 
    
    for (j in 0:magnitude) {
      if (j > x_copy) 
        break;
      y = j;
      if ((x_copy - j) < precision)
        break;
      if (j == magnitude) reject("Cast to int failed.")
    }
    if (x_sign < 0) {
      return -y;
    } else {
      return y;
    }
  }

  int casting_min(vector x) {
    return cast_to_int(min(x), 10000);
  }

  int casting_max(vector x) {
    return cast_to_int(max(x), 10000);
  }

  int[] seq_array(int n) {
    int y[n];
    for (i in 1:n) {
      y[i] = i;
    }
    return y;
  }

  /* Returns an array of all permutations of pairs of a and b. */
  int[,] all_permutations(int[] a, int[] b) {
    int N = num_elements(a);
    int M = num_elements(b);
    int Z[2, N * M];
    int i = 0;
    for (n in 1:N) {
      for (m in 1:M) {
        i = i + 1;
        Z[1,i] = n; Z[2,i] = m;
      }
    }
    return Z;
  }

  vector scale(vector x, real mu, real sigma) {
    return (x - mu) / sigma;
  }

  int[] group_sizes(int[,] group) {
    int n_levels = dims(group)[1];
    int sizes[n_levels];
    for (i in 1:n_levels)
      sizes[i] = max(group[i]);
    return sizes;
  }

  vector recenter_1L(real intercept, vector offsets_1, int[] reindex_1, real sd
  ) {
    return (intercept + offsets_1 * sd)[reindex_1];
  }

  vector recenter_2L(real intercept, vector offsets_1, int[] reindex_1, 
    vector offsets_2, int[] reindex_2, real[] sd
  ) {
    return ((intercept + offsets_1 * sd[1])[reindex_1] + 
                         offsets_2 * sd[2])[reindex_2];
  }     

  vector recenter_3L(real intercept, vector offsets_1, int[] reindex_1, 
    vector offsets_2, int[] reindex_2, 
    vector offsets_3, int[] reindex_3, real[] sd
  ) {
    return (((intercept + offsets_1 * sd[1])[reindex_1] + 
                          offsets_2 * sd[2])[reindex_2] +
                          offsets_3 * sd[3])[reindex_3];
  }     

  /* This implements the transformation from P/R/Z to an array of four vectors, 
   * one for each proportion. */
  vector[] PRZ_to_proportions(vector P, vector R, vector Z) {
    int N = num_elements(P);
    vector[N] proportions[4];
    vector[N] ones = rep_vector(1.0, N);
    proportions[1] = (ones - R) .* P;
    proportions[2] = R .* P;
    proportions[3] = (ones - P) .* Z;
    proportions[4] = (ones - P) .* (ones - Z);
    return proportions;
  }

  /* This implements the observation model for mixed data on modern method use,
   * traditional method use, unmet need, and non-need.  It returns the resulting
   * log-density.  
   */
  real observation_MTUT(vector P, vector R, vector Z, 
    int[] idx_MTXX, int[] idx_XXUX, int[] idx_XXXT,
    vector[] ratio_MTXX, vector ratio_XXUX, vector ratio_XXXT,
    real corr_MTXX, vector[] se_MTXX, vector se_XXUX, vector se_XXXT
  ) {
    real ld = 0.0;
    int n_obs = num_elements(P);
    int n_MTXX = num_elements(idx_MTXX);
    int n_XXUX = num_elements(idx_XXUX);
    int n_XXXT = num_elements(idx_XXXT);
    vector[n_obs] proportions[4] = PRZ_to_proportions(P, R, Z);

    // Observation model for MT__ data
    { 
      vector[2] q_ratio_MTXX[n_MTXX];
      for(i in 1:n_MTXX) {
        int j = idx_MTXX[i];
        matrix[2,2] Sigma = diag_matrix(se_MTXX[i] .* se_MTXX[i]);
        Sigma[1,2] = se_MTXX[i][1] * corr_MTXX * se_MTXX[i][2];
        Sigma[2,1] = Sigma[1,2];
        q_ratio_MTXX[i][1] = log(proportions[1, j] / (proportions[3, j] + proportions[4, j]));
        q_ratio_MTXX[i][2] = log(proportions[2, j] / (proportions[3, j] + proportions[4, j]));
        ld += multi_normal_cholesky_lpdf(ratio_MTXX[i] | q_ratio_MTXX[i], Sigma);
      } 
    }
  
    // Observation model for __U_ data
    {
      vector[n_XXUX] q_ratio_XXUX;
      for ( i in 1:n_XXUX ) {
        int j = idx_XXUX[i];
        q_ratio_XXUX[i] = log(proportions[3, j] / (proportions[3, j] + proportions[4, j]));
      } 
      ld += normal_lpdf(ratio_XXUX | q_ratio_XXUX, se_XXUX);
    }
  
    // Observation model for ___T data
    {
      vector[n_XXXT] q_ratio_XXXT;
      for ( i in 1:n_XXXT ) {
        int j = idx_XXXT[i];
        q_ratio_XXXT[i] = log(proportions[1, j] / (1.0 - proportions[1, j]));
      } 
    
      ld += normal_lpdf(ratio_XXXT | q_ratio_XXXT, se_XXXT);
    }
    return ld;
  }

  vector skipping_ar_nc(int[] head, int[] tail, int[] pivot, vector time,
    vector pivot_values, vector chained_values, real ro, real tau
  ) {
    int n_chained = num_elements(head);
    int n = n_chained + num_elements(pivot);
    vector[n] z;
    z[pivot] = pivot_values * tau * sqrt(1 - ro^2);
    
    for (i in 1:n_chained) {
      real delta = time[head[i]] - time[tail[i]];
      z[head[i]] = pow(ro, delta) * z[tail[i]]
        + chained_values[i] * tau * sqrt(
          (1 - ro ^ (2 * delta)) / (1 - ro ^ 2));
    }
    return z;
  }



  // Return the number of zero entries in array `x`
  int count_zeros(int[] x) {
    int count = 0;
    for (i in 1:num_elements(x)) {
      if (x[i] == 0)
        count = count + 1;
    }
    return count;
  }

  // Return the input array but with zero entries
  // deleted.
  int[] drop_zeros(int[] x) {
    int J = num_elements(x) - count_zeros(x);
    int y[J];
    int j = 0;
    for (i in 1:num_elements(x)) {
      if (x[i] != 0) {
        j = j + 1;
        y[j] = x[i];
      }
    }
    return y;
  }

  // For a grouping index and a regular index, 
  // for each level of the grouping index, pick
  // the minimum regular index.
  int[] group_min(int[] group_idx, int[] idx) {
    int K = max(group_idx);
    int y[K] = rep_array(0, K);
    for ( i in 1:num_elements(idx)) { 
      y[group_idx[i]] = min(y[group_idx[i]], idx[i]);
    }
    return y;
  }

  // For a grouping index and a group, return positional
  // index to all members of the group.
  int[] group_member(int[] group_idx, int group) {
    int K = max(group_idx);
    int N = num_elements(group_idx);
    int y[N] = seq_array(N);
    for (i in 1:N) {
      if (group_idx[i] != group)
        y[i] = 0;
      else 
        y[i] = i;
    } 
    return drop_zeros(y);
  }

  // For a grouping index and a group, count the number
  // of times the group appears in the index.
  int count_members(int[] group_idx, int group) {
    int N = num_elements(group_idx);
    int y = 0;
    for (i in 1:N) {
      if (group_idx[i] == group)
        y = y + 1;
    } 
    return y;
  }

  // For two arrays a and b, return a concatenated array.
  int[] concatenate_array(int[] a, int[] b) {
    int N = num_elements(a) + num_elements(b);
    int y[N];
    for (i in 1:num_elements(a)) {
      y[i] = a[i];
    }
    for (i in 1:num_elements(b)) {
      y[num_elements(a) + i] = b[i];
    }
    return y;
  }

  // In index 'index' look up a target values 'target'
  // and return their position in the index. A position
  // of '0' indicates the target is not present.
  int[] lookup(int[] index, int[] target) {
    int N = num_elements(target);
    int y[N] = rep_array(0, N);
    for (i in 1:N) {
      for (j in 1:num_elements(index)) {
        if (index[j] == target[i])
          y[i] = j;
      }
    }
    return y;
  }

  // Return array indicating if an entry in a 
  // time index is prior to a pivot (1), at the pivot (0), 
  // or after the pivot (-1).
  int[] pivot_time(int[] idx, int pivot) {
    int N = num_elements(idx);
    int y[N];
    for (i in 1:N) {
      if (idx[i] < pivot)
        y[i] = 1;
      else if (idx[i] > pivot)
        y[i] = -1;
      else if (idx[i] == pivot)
        y[i] = 0;
    }
    return y;
  }

  // Return an array that is the sum of two arrays, element-wise.
  int[] add_array(int[] a, int[] b) {
    int N = num_elements(a);
    int y[N];
    if (N != num_elements(b)) {
      reject("add_array function takes arrays of matching length.");
    } else {
      for (i in 1:N) { 
        y[i] = a[i] + b[i];
      }
    }
    return y;
  }

  // For each entry in the group index return a pivot value which 
  // is the mean time_idx value cast to an `int`.
  int[] pick_pivots(int[] group_idx, int[] time_idx) {
    int N = num_elements(group_idx);
    int K = max(group_idx);
    int T = max(time_idx);
    int pivots[K];
    vector[K] real_pivots = rep_vector(0.0, K);
    int n[K] = rep_array(0, K);
    for (i in 1:N) {
      real_pivots[group_idx[i]] = pivots[group_idx[i]] + time_idx[i];
      n[group_idx[i]] = n[group_idx[i]] + 1;
    }
    for (k in 1:K)
      pivots[k] = cast_to_int(real_pivots[k] / n[k], 10000);
    return pivots;
  }

  // For a grouping index and pivots, pick all entries
  // that are not pivots
  int[] pick_heads(int[] group_idx, int[] time_idx) { 
    int pivots[max(group_idx)] = pick_pivots(group_idx, time_idx); 
    int N = num_elements(group_idx);
    int heads[N] = seq_array(N);
    for (i in 1:N) {
      if (i == pivots[group_idx[i]])
        heads[i] = 0;
    }
    return drop_zeros(heads);
  }

  // For a group index, time index, and pivots array, return
  // an array of preceding observations by group.
  int[] pick_tails(int[] group_idx, int[] time_idx) {
    int N = num_elements(group_idx);
    int K = max(group_idx);
    int pivots[max(group_idx)] = pick_pivots(group_idx, time_idx); 
    int tails[N] = rep_array(0, N);
    int a = 1;
    int b = 0;
    for (k in 1:K) {
      int group_size = count_members(group_idx, k);
      int member_idx[group_size] = group_member(group_idx, k);
      int t_idx[group_size] = time_idx[member_idx];
      int first_time = min(t_idx);
      int final_time = max(t_idx);
      int order[group_size] = sort_indices_asc(t_idx);
      int diff[group_size] = pivot_time(order, pivots[k]);
      int tails_idx[group_size] = lookup(member_idx, add_array(order, diff));
      b = b + num_elements(tails_idx);
      tails[a:b] = tails_idx;
      a = a + num_elements(tails_idx);
    }
    return drop_zeros(tails);
  }

  // For a square structure 'A' return only entries picked
  // out by A[y][x]
  vector flatten(vector[] A, int[] x, int[] y) {
    vector[num_elements(x)] z;
    for (i in 1:num_elements(z)) {
      z[i] = A[y[i]][x[i]];
    }
    return z;
  }

  /* This implements the 2013 logistic model. */
  vector APT(vector asymptote, vector pace, vector timing, vector time, vector epsilon
  ) {
    return inv_logit(logit(asymptote .* inv_logit(pace .* (time - timing))) + epsilon);
  }

  /* This implements the 'rate-level' model. */
  vector RLAPT(vector asymptote, vector pace, vector epsilon, 
    int[] head, int[] tail, int[] pivots, vector pivot_values
  ) {
    int n_pivots = num_elements(pivots);
    int n_chained = num_elements(head);
    vector[n_pivots + n_chained] x;
    if (n_chained != num_elements(tail))
      reject("Number of head and tail observations does not match.");
    x[pivots] = pivot_values;
    for (i in 1:n_chained) {
      if (x[tail[i]] > asymptote[head[i]])
        x[head[i]] = inv_logit(logit(x[tail[i]]) + epsilon[tail[i]]);
      else 
        x[head[i]] = inv_logit(
          logit(asymptote[head[i]] * inv_logit(logit(
            x[tail[i]]/asymptote[tail[i]]) + pace[tail[i]])) + epsilon[tail[i]]
        );
    }   
    return x;
  }
 

  /* This implements a single step for a asymptote-pace-timing scaled logistic
   * model on the rates (Rate Asymptote Pace Timing, thus RAPT) model on
   * the level change from time s to time t.  This is the integral
   * of the rate model.
   */
  vector RAPT(vector s, vector t, vector asymptote, vector pace, vector timing) {
    return asymptote .* (t - s + inv(pace) .* ( 
        log(exp(pace .* (s - timing)) + 1) 
      - log(exp(pace .* (t - timing)) + 1)
    ));
  }

  /* This implements a single step for the Integrated Ornstein-Uhlenbeck (IOU)
   * process from OU value u(s) and IOU value v(s) to values u(t) and s(t)
   * where xi is a 2-vector of standard normal variables (a-priori) 
   * kappa is an autocorrelation parameter and sigma is a scale parameter applied
   * to the noise process.
   */
  vector IOU(real s, real t, real u, real v, real kappa, real sigma, vector xi) {
    real delta_t = t - s;
    real mu = exp(-kappa * delta_t);
    real one_m_mu_sq = 1 - mu^2;
    real var_u = sigma * inv((2 * kappa) * one_m_mu_sq);
    real var_v = sigma * inv(kappa^3) * (
      delta_t * kappa - 2 * (1 - mu) + 0.5 * one_m_mu_sq);
    real cov_uv = sigma * inv(2 * kappa^2) * one_m_mu_sq;
    vector[2] uv;
    if (num_elements(xi) != 2)
      reject("IOU parameter 'xi' must be a 2-vector.");
    uv[1] = mu * u + sqrt(var_u) * xi[1];
    uv[2] = v + u * inv(kappa) * (1 - mu) 
      + sqrt(var_v - cov_uv * cov_uv * inv(var_u)) * xi[2]
      + cov_uv * inv(var_u) * xi[1];
    return uv;
  }

}



/* Chained observations are ones that have a model
 * depending on other observations or their parameters
 * these indexing vectors **MUST** be ordered such that
 * dependencies always appear before their dependents.
 *
 * If the data were in an K by T matrix, group would
 * index along the K dimension.  There are n_levels levels
 * of indexing along this dimension.  
 *
 */

data {
  int n_obs;
  int n_yes[n_obs];
  int n_asked[n_obs];
  vector[n_obs] weights; // normalize to sum to 1 per group.

  int cluster_idx[n_obs];
  int county_idx[n_obs];
  int hh_type_idx[n_obs];
  int year_idx[n_obs];
  int survey_year_idx[n_obs];

  int n_sampled;
  int sampled_unit_idx[n_sampled];
  int sampled_year_idx[n_sampled];

  int sampled_start_idx[n_sampled];
  int sampled_stop_idx[n_sampled];
  int sampled_cross_idx[n_obs];

  int n_years;
  int n_forecast_years;
  vector[n_years + n_forecast_years] year;

}

transformed data {
  int n_clusters = max(cluster_idx);
  int n_counties = max(county_idx);

  vector[n_sampled] sampled_year = year[sampled_year_idx];
  int min_year = casting_min(year);
  int max_year = casting_max(year);
  real year_center = mean(sampled_year);
  real year_scale = sd(sampled_year);

  int n_survey_years = num_elements(sampled_year_idx);

  vector[n_sampled] sampled_timing = scale(sampled_year, year_center, 2 * year_scale);

  int n_predictions = n_counties * (n_years + n_forecast_years);
  int gen_unit_year_idx[2, n_predictions] = all_permutations(
    seq_array(n_counties),
    seq_array(n_years + n_forecast_years)
  );
  int gen_unit_idx[n_predictions] = gen_unit_year_idx[1,];
  int gen_year_idx[n_predictions] = gen_unit_year_idx[2,];
  vector[n_predictions] gen_year = year[gen_year_idx];
  vector[n_predictions] gen_timing = scale(gen_year, year_center, 2 * year_scale);

}

/* REV-01
 *
 * This is the REV-06 of the full FPET model but cut down to model only
 * modern contraceptive use in Kenya.
 *
 * The target model here is the 'rate-level model' from Kahill et al.
 * but without the bias corrections in the observation model since that 
 * component is in flux.  This is the optimized version so priors are
 * simplified, the hierarchical model is non-centered, and the AR
 * model is non-centered.
 *
 * Daniel Simpson argues that zero avoiding priors in data-sparse
 * settings such as this can inflate estimates of variances and cause
 * the data to be even less meaningful than it already is.
 * 
 * Version features:
 *  - AR country x year, non-centered
 *  - HM non-centered
 *  - Generated quantities for prediction
 * 
 */

// [[fp::functions::rev-01]]

// [[fp::data::rev-02]]

parameters {
  // Observation parameters
  vector[n_obs] theta_star;

  // AR process parameters
  vector[n_counties] epsilon_star[n_years + n_forecast_years];
  real<lower=0> tau_epsilon;
  real<lower=0, upper=1> ro_epsilon;

  // Proportion calculations;
  real P_tilde_w;
  real<lower=0> kappa_p_c;
  vector[n_counties] P_tilde_c_star;

  real omega_w;
  real<lower=0> kappa_omega_c;
  vector[n_counties] omega_c_star;

  real Omega_w;
  real<lower=0> kappa_Omega_c;
  vector[n_counties] Omega_c_star;

  real<lower=0> sigma;
}

transformed parameters {
  vector[n_obs] theta = inv_logit(theta_star);
  vector[n_sampled] design_theta;

  vector[n_counties] epsilon[n_years + n_forecast_years];
  vector[n_sampled] P;

  // observation / weighting component
  for (i in 1:n_sampled) {
    int n_county_clusters = sampled_stop_idx[i] - sampled_start_idx[i] + 1;
    int idxs[n_county_clusters] = sampled_cross_idx[sampled_start_idx[i]:sampled_stop_idx[i]];
    design_theta[i] = sum(weights[idxs] .* theta[idxs])/sum(weights[idxs]);
  }


  // AR component
  epsilon[1] = epsilon_star[1];
  for (t in 2:(n_years + n_forecast_years)) {
    epsilon[t] = ro_epsilon * epsilon[t-1] + epsilon_star[t] * tau_epsilon;
  }

  // Structural + AR 
  P = APT(
    inv_logit(recenter_1L(P_tilde_w, P_tilde_c_star, sampled_unit_idx, kappa_p_c)),
    exp(recenter_1L(omega_w - 1.0, omega_c_star, sampled_unit_idx, kappa_omega_c)),
    recenter_1L(Omega_w, Omega_c_star, sampled_unit_idx, kappa_Omega_c),
    sampled_timing, 
    rep_vector(0.0, n_sampled) 
  ); 

}

model {
  // observation
  theta_star ~ normal(0, 1.5);

  // AR error process
  ro_epsilon ~ beta(3,3);  
  tau_epsilon ~ normal(0, 0.5);
  epsilon_star[1] ~ normal(0, tau_epsilon * sqrt(1.0 - ro_epsilon^2));
  for (t in 2:(n_years + n_forecast_years)) {
    epsilon_star[t] ~ normal(0,1);
  }

  // Proportions hierarchical priors
  // Ratio of CPR versus non-use
  // Asymptote
  P_tilde_w ~ normal(0, 0.5);
  P_tilde_c_star ~ normal(0, 1);
  kappa_p_c ~ normal(0,0.5);

  // Pacing
  omega_w ~ normal(0, 0.5);
  omega_c_star ~ normal(0, 1);
  kappa_omega_c ~ normal(0, 0.5);

  // Timing
  Omega_w ~ normal(0, 1.0);
  Omega_c_star ~ normal(0, 1.0);
  kappa_Omega_c ~ normal(0, 1.0);


  // Observation model:
  sigma ~ normal(0, 0.2);

  n_yes ~ binomial(n_asked, theta);
  design_theta ~ normal(P, sigma);
}

generated quantities {
  // Structural + AR 
  vector[n_predictions] gen_P = APT(
    inv_logit(recenter_1L(P_tilde_w, P_tilde_c_star, gen_unit_idx, kappa_p_c)),
    exp(recenter_1L(omega_w - 1.0, omega_c_star, gen_unit_idx, kappa_omega_c)),
    recenter_1L(Omega_w, Omega_c_star, gen_unit_idx, kappa_Omega_c),
    gen_timing, 
    flatten(epsilon, gen_unit_idx, gen_year_idx)
  ); 
}
