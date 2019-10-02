library(rstan)
rstan_options(auto_write = TRUE)

hmm_data <- readRDS("data/hmm_example.RDS")

stan_data <- list(N = length(hmm_data$y),
                  K = 2,
                  y = hmm_data$y)

hmm_fit <- stan("models/hmm_example_bad.stan", data = stan_data, iter = 1e3, chains = 4)
saveRDS(list(fit = hmm_fit, data = stan_data), "results/hmm_example_bad.RDS")

samples <- as.matrix(hmm_fit)

psi_indx <- grep("^mu\\[", colnames(samples))
theta_indx <- grep("^theta\\[", colnames(samples))
z_star_indx <- grep("^z_star\\[", colnames(samples))

traceplot(hmm_fit, pars = "theta")
traceplot(hmm_fit, pars = "mu")

colMeans(samples[,theta_indx])
colMeans(samples[,psi_indx])

z_star <- colMeans(samples[,z_star_indx])

# visualization
pdf("media/hmm_example_bad.pdf", width = 12, height = 9)
par(mfrow=c(2,1))
plot(hmm_data$z, type="l",
     main = "Latent States",
     ylab = "State Value",
     xlab = "Time")
points(z_star, cex = 0.5)
legend("bottomright", c("Actual","Predicted"), pch = c(NA,1), lty = c(1,NA), cex = 0.8)
plot(hmm_data$y, type = "l",
     main = "Observed Output",
     ylab = "Observation Value",
     xlab = "Time")
y_plt <- hmm_data$y
y_plt[hmm_data$z==1] <- NA
lines(y_plt, lwd = 3)
legend("bottomright", c("State 1","State 2"), lty = c(1,1), lwd = c(1,3), cex = 0.8)
dev.off()
