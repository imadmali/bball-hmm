library(rstan)
library(bayesplot)
library(animation)

source("graphics.R")
rstan_options(auto_write = TRUE)

evt_id <- "evt244"

defense_low_res <- readRDS(paste0("data/defense_", evt_id, "_low_res.RDS"))
defense <- readRDS(paste0("data/defense_", evt_id, ".RDS"))

alpha <- c(10,10,10)  # priors on convex combination parameter
defense_low_res$alpha <- alpha
defense_low_res$K <- 5
defense_low_res$I <- 5
defense_low_res$tau <- 0.1
defense_low_res$theta <- rbind(rep(0.2,5),
                               rep(0.2,5),
                               rep(0.2,5),
                               rep(0.2,5),
                               rep(0.2,5))
  
fit <- stan("models/defense_2.stan", data = defense_low_res, chains = 4, iter = 1e3, cores = 4)
saveRDS(list(fit = fit, data = defense_low_res), paste0("results/defense_2_", evt_id, "_low_res.RDS"))

# mcmc_trace(as.array(fit), regex_pars = "^theta|^lambda")
print(fit, pars = "z_star", include = FALSE, probs = c(0.05,0.95))

samples <- as.matrix(fit)
z_star <- list()
for (i in 1:5) {
  def_z_star <- rep(NA, defense$N)
  def_z_star[defense_low_res$indxs] <- colMeans(samples[,grep(paste0("^z_star\\[",i), colnames(samples))])
  def_z_star <- zoo::na.locf(def_z_star)
  z_star[[i]] <- def_z_star
}

#' @param indx integer time step
#' @param I integer value between 1 to 5 (defender index)
#' @param state integer value between 1 to 5 of inferred state (offensive player index).
#' @return line segment to be overlayed on a plot
def_assign <- function(indx, I, state) {
  segments(d[I,indx,1],d[I,indx,2],
           o[state,indx,1],o[state,indx,2])  
}

pt <- readRDS(paste0("data/", evt_id, "_0021500411.RDS"))

d <- defense$d
o <- defense$o

# plot video
ani.options(ani.width=900, ani.height=600, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:defense$N) {
    plot_fullcourt()
    text(1,48, paste0("Q",pt$game$quarter[i]," | GC: ",pt$game$game_clock[i]), pos=4, cex=1.5)
    plot_shot(pt, loop = i, static = F)
    for (j in 1:5)
      def_assign(i, j, round(z_star[[j]][i]))
  }
}, video.name = paste0("media/defense_2_", evt_id, "_low_res",".mp4"))
