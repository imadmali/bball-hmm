library(rstan)
library(bayesplot)
library(animation)

source("graphics.R")
rstan_options(auto_write = TRUE)

defense <- readRDS("data/defense.RDS")
alpha <- c(3,3,3)  # priors on convex combination parameter
defense$alpha <- alpha
defense$K <- 5
defense$I <- 5
defense$tau <- 0.1

fit <- stan("models/defense_1.stan", data = defense, chains = 4, iter = 1e3, cores = 4)
saveRDS(list(fit = fit, data = defense), "results/defense_1.RDS")

# mcmc_trace(as.array(fit), regex_pars = "^theta|^lambda")

samples <- as.matrix(fit)
y_star <- list()
for (i in 1:5) {
  def_y_star <- samples[,grep(paste0("^y_star\\[",i), colnames(samples))]
  y_star[[i]] <- colMeans(def_y_star)
}

#' @param indx integer time step
#' @param I integer value between 1 to 5 (defender index)
#' @param state integer value between 1 to 5 of inferred state (offensive player index).
#' @return line segment to be overlayed on a plot
def_assign <- function(indx, I, state) {
  segments(d[I,indx,1],d[I,indx,2],
           o[state,indx,1],o[state,indx,2])  
}

drive <- readRDS("data/evt140_0021500411.RDS")
d <- defense$d
o <- defense$o

# plot fig
pdf("media/defense_1.pdf", width = 12, height = 8)
plot_fullcourt()
plot_shot(drive, loop = 1, static = F)
def_assign(1, 1, y_star[[1]][1])
def_assign(1, 2, y_star[[2]][1])
def_assign(1, 3, y_star[[3]][1])
def_assign(1, 4, y_star[[4]][1])
def_assign(1, 5, y_star[[5]][1])
dev.off()

# plot video
ani.options(ani.width=900, ani.height=600, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:defense$N) {
    plot_fullcourt()
    text(1,48, paste0("Q",drive$game$quarter[i]," | GC: ",drive$game$game_clock[i]), pos=4, cex=1.5)
    plot_shot(drive, loop = i, static = F)
    for (j in 1:5)
      def_assign(i, j, round(y_star[[j]][1]))
  }
}, video.name = paste0("media/defense_event_140_m1",".mp4"))
