data {
  int<lower=0> N;
  int<lower=0> K;
  real<lower=0> cc_sd;
  vector[3] alpha;
  vector[2] h[N];  // array of size N containing vectors of size 2
  vector[2] b[N];
  vector[2] d[N];
  vector[2] o[K,N];
  // vector[2] o3[N];
}

// transformed data {
//   real lambda[3];
//   lambda = {1.0/3,1.0/3,1.0/3};
// }

parameters {
  simplex[K] theta[K];
  simplex[3] lambda;
  // real psi[K];
}

model {
  // priors
  // target+= dirichlet_lpdf(lambda | alpha);
  // forward algorithm
  {
  real acc[K];
  real gamma[N, K];
  for (k in 1:K)
    gamma[1, k] = normal_lpdf(d[1] |  o[k,1]*lambda[1] + h[1]*lambda[2] + b[1]*lambda[3], cc_sd);
  for (t in 2:N) {
    for (k in 1:K) {
      for (j in 1:K)
        acc[j] = gamma[t-1, j] + log(theta[j, k])
          + normal_lpdf(d[t] |  o[k,t]*lambda[1] + h[t]*lambda[2] + b[t]*lambda[3], cc_sd);
      gamma[t, k] = log_sum_exp(acc);
    }
  }
  target += log_sum_exp(gamma[N]);
  }
}

generated quantities {
  int<lower=1,upper=K> y_star[N];
  real log_p_y_star;
  {
    int back_ptr[N, K];
    real best_logp[N, K];
    real best_total_logp;
    for (k in 1:K)
      best_logp[1, k] = normal_lpdf(d[1] |  o[k,1]*lambda[1] + h[1]*lambda[2] + b[1]*lambda[3], cc_sd);
    for (t in 2:N) {
      for (k in 1:K) {
        best_logp[t, k] = negative_infinity();
        for (j in 1:K) {
          real logp;
          logp = best_logp[t-1, j] + log(theta[j, k])
            + normal_lpdf(d[t] |  o[k,t]*lambda[1] + h[t]*lambda[2] + b[t]*lambda[3], cc_sd);
          if (logp > best_logp[t, k]) {
            back_ptr[t, k] = j;
            best_logp[t, k] = logp;
          }
        }
      }
    }
    log_p_y_star = max(best_logp[N]);
    for (k in 1:K)
      if (best_logp[N, k] == log_p_y_star)
        y_star[N] = k;
    for (t in 1:(N - 1))
      y_star[N - t] = back_ptr[N - t + 1, y_star[N - t + 1]];
  }
}
