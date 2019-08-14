library(rstan)
library(bayesplot)
library(animation)

source("graphics.R")
rstan_options(auto_write = TRUE)

drive <- readRDS("data/evt140_0021500411.RDS")

lavine_speed <- drive$game$lavine_speed
lavine_dist <- drive$game$lavine_dist

na_indx <- which(is.na(lavine_speed))

# lavine_speed <- log(1/lavine_speed[-1])  # improper because using exp in model
lavine_speed <- lavine_speed[-1]
lavine_dist <- lavine_dist[-1]

stan_data <- list(N = length(lavine_speed),
                  K = 2,
                  u = lavine_speed,
                  v = 1/lavine_dist,
                  alpha = rbind(c(6,2),c(2,6)))

fit <- stan("models/drive.stan", data = stan_data, chains = 4, iter = 1e3)

samples <- as.matrix(fit)
y_star <- samples[,grep("^y_star", colnames(samples))]
y_star <- colMeans(y_star)
y_star
mcmc_trace(as.array(fit), regex_pars = "^theta|^phi|^lambda")

ani.options(ani.width=600, ani.height=900, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:nrow(drive$game)) {
    layout(matrix(1:4, ncol = 1), heights = c(1,1,1,2))
    plot(lavine_dist, type = "l", ylab = "Distance From Hoop", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    plot(lavine_speed, type = "l", ylab = "1/Speed", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    plot(y_star, pch = 20, cex = 0.5, ylab = "State", xlab = "Time (25hz)",
         ylim = c(0.5, 2.5), yaxt = "n")
    axis(2, c(1,2), c("None", "Drive"), las = 2)
    abline(v = i, col = "red", lwd = 2)
    plot_fullcourt()
    text(1,48, paste0("Q",drive$game$quarter[i]," | GC: ",drive$game$game_clock[i]), pos=4, cex=1.5)
    plot_shot(drive, loop = i, static = F)
  }
}, video.name = paste0("media/drive_event_140_hmm",".mp4"))
