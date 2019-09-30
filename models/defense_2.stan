// defensive model 2 given transition matrix (assignment for all 5 defenders)
data {
  int<lower=0> N;       // number of time steps
  int<lower=0> K;       // number of offensive players
  int<lower=0> I;       // number of defenders
  real<lower=0> tau;    // scale parameter associated with convex combination
  vector[3] alpha;      // prior on convex combination parameters
  vector[K] theta[K];   // given transition matrix
  vector[2] h;          // location of hoop
  vector[2] b[N];       // location of ball (array of size N containing vectors of size 2)
  vector[2] d[I,N];     // location of defenders (array of size I,K containing vectors of size 2)
  vector[2] o[K,N];     // location of offensive players (array of size K,N containing vectors of size 2)
}

parameters {
  simplex[3] lambda;
}

model {
  // priors
  target+= dirichlet_lpdf(lambda | alpha);
  // forward algorithm
  for (i in 1:I) {
    real acc[K];
    real gamma[N, K];
    for (k in 1:K)
      gamma[1, k] = normal_lpdf(d[i,1] |  o[k,1]*lambda[1] + h*lambda[2] + b[1]*lambda[3], tau);
    for (t in 2:N) {
      for (k in 1:K) {
        for (j in 1:K)
          acc[j] = gamma[t-1, j] + log(theta[j, k])
            + normal_lpdf(d[i,t] |  o[k,t]*lambda[1] + h*lambda[2] + b[t]*lambda[3], tau);
        gamma[t, k] = log_sum_exp(acc);
      }
    }
    target += log_sum_exp(gamma[N]);
  }
}

generated quantities {
  int<lower=1,upper=K> z_star[I,N];
  real log_p_z_star;
  for (i in 1:I) {
    int back_ptr[N, K];
    real best_logp[N, K];
    for (k in 1:K)
      best_logp[1, k] = normal_lpdf(d[i,1] |  o[k,1]*lambda[1] + h*lambda[2] + b[1]*lambda[3], tau);
    for (t in 2:N) {
      for (k in 1:K) {
        best_logp[t, k] = negative_infinity();
        for (j in 1:K) {
          real logp;
          logp = best_logp[t-1, j] + log(theta[j, k])
            + normal_lpdf(d[i,t] |  o[k,t]*lambda[1] + h*lambda[2] + b[t]*lambda[3], tau);
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
        z_star[i,N] = k;
    for (t in 1:(N - 1))
      z_star[i, N - t] = back_ptr[N - t + 1, z_star[i, N - t + 1]];
  }
}
