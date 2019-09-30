// defensive model 0.b (lambda as parameter)
data {
  int<lower=0> N;
  int<lower=0> K;
  real<lower=0> tau;
  vector[3] alpha;
  vector[2] h;
  vector[2] b[N];  // array of size N containing vectors of size 2
  vector[2] d[N];
  vector[2] o[K,N];   // array of size K,N containing vectors of size 2
}

parameters {
  simplex[K] theta[K];
  simplex[3] lambda;
}

model {
  // priors
  target+= dirichlet_lpdf(lambda | alpha);
  // forward algorithm
  {
  real acc[K];
  real gamma[N, K];
  for (k in 1:K)
    gamma[1, k] = normal_lpdf(d[1] |  o[k,1]*lambda[1] + h*lambda[2] + b[1]*lambda[3], tau);
  for (t in 2:N) {
    for (k in 1:K) {
      for (j in 1:K)
        acc[j] = gamma[t-1, j] + log(theta[j, k])
          + normal_lpdf(d[t] |  o[k,t]*lambda[1] + h*lambda[2] + b[t]*lambda[3], tau);
      gamma[t, k] = log_sum_exp(acc);
    }
  }
  target += log_sum_exp(gamma[N]);
  }
}

generated quantities {
  int<lower=1,upper=K> z_star[N];
  real log_p_z_star;
  {
    int back_ptr[N, K];
    real best_logp[N, K];
    for (k in 1:K)
      best_logp[1, k] = normal_lpdf(d[1] |  o[k,1]*lambda[1] + h*lambda[2] + b[1]*lambda[3], tau);
    for (t in 2:N) {
      for (k in 1:K) {
        best_logp[t, k] = negative_infinity();
        for (j in 1:K) {
          real logp;
          logp = best_logp[t-1, j] + log(theta[j, k])
            + normal_lpdf(d[t] |  o[k,t]*lambda[1] + h*lambda[2] + b[t]*lambda[3], tau);
          if (logp > best_logp[t, k]) {
            back_ptr[t, k] = j;
            best_logp[t, k] = logp;
          }
        }
      }
    }
    log_p_z_star = max(best_logp[N]);
    for (k in 1:K)
      if (best_logp[N, k] == log_p_z_star)
        z_star[N] = k;
    for (t in 1:(N - 1))
      z_star[N - t] = back_ptr[N - t + 1, z_star[N - t + 1]];
  }
}
