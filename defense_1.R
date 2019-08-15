library(rstan)
rstan_options(auto_write = TRUE)

defense_example <- readRDS("data/defense_example.RDS")
alpha <- c(3,3,3)  # priors on convex combination parameter
defense_example$alpha <- alpha
fit <- stan("models/defense_1.stan", data = defense_example, chains = 4, iter = 1e3)

samples <- as.matrix(fit)
y_star <- samples[,grep("^y_star", colnames(samples))]
y_star <- colMeans(y_star)

list2env(defense_example, .GlobalEnv)

mu1 <- t(rbind(o[1,1,],h,b[1,])) %*% lambda %>% t %>% c
mu2 <- t(rbind(o[2,1,],h,b[1,])) %*% lambda %>% t %>% c
mu3 <- t(rbind(o[3,1,],h,b[1,])) %*% lambda %>% t %>% c
mu4 <- t(rbind(o[4,1,],h,b[1,])) %*% lambda %>% t %>% c
mu5 <- t(rbind(o[5,1,],h,b[1,])) %*% lambda %>% t %>% c

plot(h[1], h[2], xlim = c(-2,2), ylim = c(-2,2), pch = 19,
     main = expression(paste("Defense Example: ", Lambda, " Fixed")),
     xlab = "x coordinate", ylab = "y coordinate")
text(d[,1], d[,2], labels = paste(y_star), col = "#ff668890")
points(b)
points(o[1,,], pch = 3)
points(o[2,,], pch = 3)
points(o[3,,], pch = 3)
points(o[4,,], pch = 3)
points(o[5,,], pch = 3)
text(d[1,1], d[1,2], "defender start", pos = 2, cex = 0.8)
text(d[N,1], d[N,2], "defender end", pos = 4, cex = 0.8)
text(h[1], h[2], "hoop", pos = 1, cex = 0.8)
text(b[1,1], b[1,2], "ball", pos = 4, cex = 0.8)
text(o[1,1,1], o[1,1,2], "o1", pos = 4, cex = 0.8)
text(o[2,1,1], o[2,1,2], "o2", pos = 4, cex = 0.8)
text(o[3,1,1], o[3,1,2], "o3", pos = 4, cex = 0.8)
text(o[4,1,1], o[4,1,2], "o4", pos = 2, cex = 0.8)
text(o[5,1,1], o[5,1,2], "o5", pos = 4, cex = 0.8)
text(mu1[1], mu1[2], expression(mu[1]), cex = 0.8)
text(mu2[1], mu2[2], expression(mu[2]), cex = 0.8)
text(mu3[1], mu3[2], expression(mu[3]), cex = 0.8)
text(mu4[1], mu4[2], expression(mu[4]), cex = 0.8)
text(mu5[1], mu5[2], expression(mu[5]), cex = 0.8)

# determine which convex combination each defensive position is closest to
dist_mat <- list(o1 = apply(d, 1, function(x){dist(rbind(mu1,x))}),
                 o2 = apply(d, 1, function(x){dist(rbind(mu2,x))}),
                 o3 = apply(d, 1, function(x){dist(rbind(mu3,x))}),
                 o4 = apply(d, 1, function(x){dist(rbind(mu4,x))}),
                 o5 = apply(d, 1, function(x){dist(rbind(mu5,x))}))
dist_mat <- data.frame(dist_mat)
dist_mat$state <- c(apply(dist_mat, 1, which.min))

rbind(hmm = unname(colMeans(y_star)),
      dist = dist_mat$state)
