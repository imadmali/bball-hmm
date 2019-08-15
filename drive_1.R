library(rstan)
library(bayesplot)
library(animation)

source("graphics.R")
rstan_options(auto_write = TRUE)

drive <- readRDS("data/evt140_0021500411.RDS")

lavine_speed <- drive$game$lavine_speed
lavine_dist <- drive$game$lavine_dist

na_indx <- which(is.na(lavine_speed))

lavine_speed <- lavine_speed[-1]
lavine_dist <- lavine_dist[-1]

stan_data <- list(N = length(lavine_speed),
                  K = 2,
                  u = log(1/lavine_speed),
                  v = log(lavine_dist),
                  alpha = rbind(c(4,2),c(2,4)),
                  tau = 1,
                  rho = 1)

fit <- stan("models/drive_1.stan", data = stan_data, chains = 4, iter = 1e3)
saveRDS(list(fit = fit, data = stan_data), "results/drive_m1.RDS")

# mcmc_trace(as.array(fit), regex_pars = "^theta|^phi|^lambda|^y_star\\[1\\]")

samples <- as.matrix(fit)
y_star <- samples[,grep("^y_star", colnames(samples))]
y_star <- colMeans(y_star)

ani.options(ani.width=600, ani.height=900, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:nrow(drive$game)) {
    layout(matrix(1:4, ncol = 1), heights = c(1,1,1,2))
    plot(lavine_dist, type = "l", ylab = "Distance From Hoop", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    plot(lavine_speed, type = "l", ylab = "1/Speed", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    plot(round(y_star), type = "l", pch = 1, cex = 0.5, ylab = "State", xlab = "Time (25hz)",
         ylim = c(0.5, 2.5), yaxt = "n")
    axis(2, c(1,2), c("Drive", "None"), las = 2)
    abline(v = i, col = "red", lwd = 2)
    plot_fullcourt()
    text(1,48, paste0("Q",drive$game$quarter[i]," | GC: ",drive$game$game_clock[i]), pos=4, cex=1.5)
    plot_shot(drive, loop = i, static = F)
  }
}, video.name = paste0("media/drive_event_140_m1",".mp4"))
