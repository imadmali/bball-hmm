functions {
  int swap_state(int state) {
    return(state == 1 ? 2 : 1);
  }
}
data {
  int<lower=1> K;              // number of states (1 = none, 2 = drive)
  // int<lower=1> V;  // num words
  // int<lower=0> T;  // num supervised items
  int<lower=1> T_unsup;  // length of process
  // int<lower=1,upper=V> w[T]; // words
  // int<lower=1,upper=K> z[T]; // categories
  real<lower=0> u[T_unsup];    // 1/speed
  real<lower=0> v[T_unsup];    // hoop distance
  matrix<lower=0>[K,K] alpha;  // transit prior
  // vector<lower=0>[V] beta;   // emit prior
}
parameters {
  simplex[K] theta[K];          // transit probs
  // enforce an ordering: phi[1] <= phi[2]
  positive_ordered[K] phi;      // emission prob for 1/speed
  positive_ordered[K] lambda;   // emission prob for hoop dist
}
model {
  // priors (break symmetry)
  theta[1] ~ dirichlet(alpha[1,]');
  theta[2] ~ dirichlet(alpha[2,]');
  phi[1] ~ normal(0,1);
  phi[2] ~ normal(3,1);
  lambda[1] ~ normal(0,1);
  lambda[2] ~ normal(3,1);
  {
    // forward algorithm computes log p(u|...)
    real acc[K];
    real gamma[T_unsup,K];
    for (k in 1:K)
      // gamma[1,k] = (log(phi[k]) - phi[k] * u[1]) + (log(lambda[swap_state(k)]) - lambda[swap_state(k)] * v[1]);
      gamma[1,k] = (log(phi[k]) - phi[k] * u[1]) + (log(lambda[k]) - lambda[k] * v[1]);
    for (t in 2:T_unsup) {
      for (k in 1:K) {
        for (j in 1:K)
          //acc[j] = gamma[t-1,j] + log(theta[j,k]) + (log(phi[k]) - phi[k] * u[t]) + //(log(lambda[swap_state(k)]) - lambda[swap_state(k)] * v[t]);
          acc[j] = gamma[t-1,j] + log(theta[j,k]) + (log(phi[k]) - phi[k] * u[t]) + (log(lambda[k]) - lambda[k] * v[t]);
        gamma[t,k] = log_sum_exp(acc);
      }
    }
    target+= log_sum_exp(gamma[T_unsup]);
  }
}
generated quantities {
  int<lower=1,upper=K> y_star[T_unsup];
  real log_p_y_star;
  {
    // Viterbi algorithm
    int back_ptr[T_unsup,K];
    real best_logp[T_unsup,K];
    for (k in 1:K)
      // best_logp[1,K] = (log(phi[k]) - phi[k] * u[1]) +
      //                  (log(lambda[swap_state(k)]) - lambda[swap_state(k)] * v[1]);
      best_logp[1,K] = (log(phi[k]) - phi[k] * u[1]) +
                       (log(lambda[k]) - lambda[k] * v[1]);
    for (t in 2:T_unsup) {
      for (k in 1:K) {
        best_logp[t,k] = negative_infinity();
        for (j in 1:K) {
          real logp;
          // logp = best_logp[t-1,j] + log(theta[j,k]) +
          //        (log(phi[k]) - phi[k] * u[t]) +
          //        (log(lambda[swap_state(k)]) - lambda[swap_state(k)] * v[t]);
          logp = best_logp[t-1,j] + log(theta[j,k]) +
                 (log(phi[k]) - phi[k] * u[t]) +
                 (log(lambda[k]) - lambda[k] * v[t]);
          if (logp > best_logp[t,k]) {
            back_ptr[t,k] = j;
            best_logp[t,k] = logp;
          }
        }
      }
    }
    log_p_y_star = max(best_logp[T_unsup]);
    for (k in 1:K)
      if (best_logp[T_unsup,k] == log_p_y_star)
        y_star[T_unsup] = k;
    for (t in 1:(T_unsup - 1))
      y_star[T_unsup - t] = back_ptr[T_unsup - t + 1, y_star[T_unsup - t + 1]];
  }
}
