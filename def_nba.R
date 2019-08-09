rstan_options(auto_write = TRUE)

# question: which player is the defender guarding?
N <- 20
tau <- 0.01
h <- cbind(rep(0,N),
           rep(-1,N))
b <- cbind(rnorm(N, 0, tau),
           rnorm(N, 1.5, tau))
o1 <- cbind(rnorm(N, -1, tau),
           rnorm(N, 0.5, tau))
o2 <- cbind(rnorm(N, 1, tau),
            rnorm(N, 0.5, tau))
o3 <- cbind(rnorm(N, 0.5, tau),
            rnorm(N, 1, tau))
d1 <- cbind(seq(-1,1,length.out =N),
            rep(0, N))

plot(h, xlim = c(-2,2), ylim = c(-2,2), pch = 19)
points(d1, col = "#ff6688", pch = 0)
points(b)
points(o1, pch = 3)
points(o2, pch = 3)
points(o3, pch = 3)

# h <- h*10
# b <- b*10
# o1 <- o1*10
# o2 <- o2*10
# d1 <- d1*10
# 
# plot(h, xlim = c(-2,2)*10, ylim = c(-2,2)*10, pch = 19)
# points(d1, col = "#ff6688", pch = 0)
# points(b)
# points(o1, pch = 3)
# points(o2, pch = 3)

K <- 3
o <- array(c(o1,o2,o3), dim = c(N,2,K))
o <- aperm(o, c(3,1,2))
stan_data <- list(N = N,
                  K = K,
                  cc_sd = 0.001,
                  alpha = c(3,3,3),
                  h = h,
                  b = b,
                  d = d1,
                  o = o)

fit <- stan("models/def_model1.stan", data = stan_data, chains = 4, iter = 1e3)
# mcmc_trace(as.array(fit), regex_pars = "lambda")
# mcmc_trace(as.array(fit), pars = "y_star[1]")
# mcmc_trace(as.array(fit), pars = "y_star[20]")
# mcmc_trace(as.array(fit), regex_pars = "^lambda")

samples <- as.matrix(fit)
lambda <- samples[,grep("^^lambda", colnames(samples))]
lambda <- colMeans(lambda)
lambda
y_star <- samples[,grep("^y_star", colnames(samples))]
apply(y_star, 2, table)
colMeans(y_star)
# plot(colMeans(y_star))

# cbind(o1[1,],h[1,],b[1,]) %*% lambda

plot(h, xlim = c(-2,2), ylim = c(-2,2), pch = 19)
points(d1, col = "#ff6688", pch = colMeans(y_star))
points(b)
points(o1, pch = 3)
points(o2, pch = 3)
points(o3, pch = 3)

