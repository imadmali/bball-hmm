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

lavine_speed_smooth <- rep(lavine_speed[2],2)
for (i in 3:length(lavine_speed))
  lavine_speed_smooth[i] <- mean(lavine_speed[(i-2):i], na.rm=TRUE)

stan_data <- list(N = length(lavine_speed_smooth),
                  K = 2,
                  u = log(1/lavine_speed_smooth),
                  v = log(lavine_dist),
                  alpha = rbind(c(4,2),c(2,4)),
                  tau = 0.1,
                  rho = 0.1)

fit <- stan("models/drive_1.stan", data = stan_data, chains = 4, iter = 1e3)
saveRDS(list(fit = fit, data = stan_data), "results/drive_1.RDS")

# mcmc_trace(as.array(fit), regex_pars = "^theta|^phi|^lambda|^z_star\\[1\\]")

samples <- as.matrix(fit)
z_star <- samples[,grep("^z_star", colnames(samples))]
z_star <- colMeans(z_star)

pdf("media/drive_1.pdf", width = 9, height = 12)
par(mfrow = c(3,1))
plot(lavine_dist, type = "l",
     main = "Distance from Hoop",
     xlab = "Time (25hz)", ylab = "Distance from Hoop")
plot(lavine_speed_smooth, type = "l",
     main = "Smooth Speed",
     xlab = "Time (25hz)", ylab = "Speed")
plot(round(z_star), type = "l", pch = 1, cex = 0.5,
     main = "Hidden States",
     ylab = "State", xlab = "Time (25hz)",
     ylim = c(0.5, 2.5), yaxt = "n")
axis(2, c(1,2), c("Drive", "None"), las = 2)
dev.off()

ani.options(ani.width=600, ani.height=900, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:nrow(drive$game)) {
    layout(matrix(1:4, ncol = 1), heights = c(1,1,1,2))
    plot(lavine_dist, type = "l", ylab = "Distance From Hoop", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    plot(lavine_speed_smooth, type = "l", ylab = "1/Speed", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    plot(round(z_star), type = "l", pch = 1, cex = 0.5, ylab = "State", xlab = "Time (25hz)",
         ylim = c(0.5, 2.5), yaxt = "n")
    axis(2, c(1,2), c("Drive", "None"), las = 2)
    abline(v = i, col = "red", lwd = 2)
    plot_fullcourt()
    text(1,48, paste0("Q",drive$game$quarter[i]," | GC: ",drive$game$game_clock[i]), pos=4, cex=1.5)
    plot_shot(drive, loop = i, static = F)
  }
}, video.name = paste0("media/drive_1",".mp4"))
