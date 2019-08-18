# HMM example data (hmm_example.RDS)

# transition matrix (2 states)
theta <- rbind(c(0.8,0.2),c(0.1,0.9))
# emmission vector (2 states)
psi <- c(3,9)
# timesteps
N <- 100

# hidden states
z <- 1
for (i in 2:N)
  z[i] <- sample(1:2, 1, replace = TRUE, prob = theta[z[i-1],])
# observations
y <- c()
for (i in 1:N)
  y[i] <- rnorm(1, psi[z[i]], 1)

# visualization
par(mfrow=c(2,1))
plot(z, type="l",
     main = "Latent States",
     ylab = "State Value",
     xlab = "Time")
plot(y, type = "l",
     main = "Observed Output",
     ylab = "Observation Value",
     xlab = "Time")
y_plt <- y
y_plt[z==1] <- NA
lines(y_plt, col = "red")
dev.off()

saveRDS(list(z=z,y=y), "hmm_example.RDS")
