library(rstan)
library(dplyr)
source("graphics.R")
rstan_options(auto_write = TRUE)

defense_example <- readRDS("data/defense_example.RDS")
alpha <- c(3,3,3)  # priors on convex combination parameter
defense_example$alpha <- alpha
fit <- stan("models/defense_0b.stan", data = defense_example, chains = 4, iter = 1e3)
saveRDS(list(fit = fit, data = defense_example), "results/defense_0b.RDS")

samples <- as.matrix(fit)
z_star <- samples[,grep("^z_star", colnames(samples))]
z_star <- colMeans(z_star)

lambda <- samples[,grep("^lambda", colnames(samples))]
lambda <- colMeans(lambda)

list2env(defense_example, .GlobalEnv)

# compute convex combination given lambda estimate
mu1 <- t(rbind(o[1,1,],h,b[1,])) %*% lambda %>% t %>% c
mu2 <- t(rbind(o[2,1,],h,b[1,])) %*% lambda %>% t %>% c
mu3 <- t(rbind(o[3,1,],h,b[1,])) %*% lambda %>% t %>% c
mu4 <- t(rbind(o[4,1,],h,b[1,])) %*% lambda %>% t %>% c
mu5 <- t(rbind(o[5,1,],h,b[1,])) %*% lambda %>% t %>% c

# plot
pdf("media/defense_0b.pdf", height = 8, width = 8)
plt_defense_example(defense_example, main = expression(paste("Defense Example: ", Lambda, " Estimated")))
lambda_txt <- sprintf("%.2f", round(lambda,2), collapse=",")
lambda_txt <- paste(lambda_txt, collapse = ",")
text(-2,1.9, bquote(paste(Lambda %~~% phantom(), "[", .(lambda_txt),"]")), pos = 4)
text(d[,1], d[,2], labels = paste(z_star), col = "#ff668890")
text(mu1[1], mu1[2], expression(mu[1]), cex = 0.8)
text(mu2[1], mu2[2], expression(mu[2]), cex = 0.8)
text(mu3[1], mu3[2], expression(mu[3]), cex = 0.8)
text(mu4[1], mu4[2], expression(mu[4]), cex = 0.8)
text(mu5[1], mu5[2], expression(mu[5]), cex = 0.8)
dev.off()

# determine which convex combination each defensive position is closest to
dist_mat <- list(o1 = apply(d, 1, function(x){dist(rbind(mu1,x))}),
                 o2 = apply(d, 1, function(x){dist(rbind(mu2,x))}),
                 o3 = apply(d, 1, function(x){dist(rbind(mu3,x))}),
                 o4 = apply(d, 1, function(x){dist(rbind(mu4,x))}),
                 o5 = apply(d, 1, function(x){dist(rbind(mu5,x))}))
dist_mat <- data.frame(dist_mat)
dist_mat$state <- c(apply(dist_mat, 1, which.min))

rbind(hmm = unname(z_star),
      dist = dist_mat$state)
