# create fake basketball possession with 1 defender (defense_example.RDS)

source("../graphics.R")

N <- 20
tau <- 0.01
h <- c(0,-1)
K <- 5
# sample positions
b <- cbind(rnorm(N, 0, tau),
           rnorm(N, 1.5, tau))
o1 <- cbind(rnorm(N, -1, tau),
            rnorm(N, 0.5, tau))
o2 <- cbind(rnorm(N, 1, tau),
            rnorm(N, 0.5, tau))
o3 <- cbind(rnorm(N, 0.5, tau),
            rnorm(N, 1, tau))
o4 <- cbind(rnorm(N, -0.1, tau),
            rnorm(N, 1.5, tau))
o5 <- cbind(rnorm(N, -0.5, tau),
            rnorm(N, -0.7, tau))
d <- cbind(seq(-1,1,length.out=N),
           -0.6*seq(-1.5,1.5,length.out=N)^2+0.5)
# convert offensive positions into an array of shape (K,N,2)
o <- array(c(o1,o2,o3,o4,o5), dim = c(N,2,K))
o <- aperm(o, c(3,1,2))
# store as list
dat <- list(N = N,
            K = K,
            tau = tau,
            h = h,
            b = b,
            o = o,
            d = d)
saveRDS(dat, "defense_example.RDS")

# plot
plt_defense_example(dat, main = "Defense Example")
points(d[,1], d[,2], col = "#ff6688", pch = 0)
