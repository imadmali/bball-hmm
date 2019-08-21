// simple hmm example with bad fit (1 output; 2 states)
data {
  int<lower=0> N;
  int<lower=0> K;
  real y[N];
}

parameters {
  simplex[K] theta[K];
  real psi[K];
  // positive_ordered[K] psi;
}

model {
  // priors
  target+= normal_lpdf(psi[1] | 3, 1);
  target+= normal_lpdf(psi[2] | 10, 1);
  // forward algorithm
  {
  real acc[K];
  real gamma[N, K];
  for (k in 1:K)
    gamma[1, k] = normal_lpdf(y[1] | psi[k], 1);
  for (t in 2:N) {
    for (k in 1:K) {
      for (j in 1:K)
        acc[j] = gamma[t-1, j] + log(theta[j, k]) + normal_lpdf(y[t] | psi[k], 1);
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
    for (k in 1:K)
      best_logp[1, k] = normal_lpdf(y[1] | psi[k], 1);
    for (t in 2:N) {
      for (k in 1:K) {
        best_logp[t, k] = negative_infinity();
        for (j in 1:K) {
          real logp;
          logp = best_logp[t-1, j] + log(theta[j, k]) + normal_lpdf(y[t] | psi[k], 1);
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
