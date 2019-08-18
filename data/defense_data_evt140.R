# reshape pt data evt140 for defense hmm in stan
# full data: defense_evt140.RDS
# low res: defense_evt140_low_res.RDS

pt <- readRDS("evt140_0021500411.RDS")

game <- pt$game

# indx to reduce resolution
indxs <- seq(1, nrow(game), by = 10)
if (tail(indxs,1) != nrow(game))
  indxs <- append(indxs, nrow(game))

h <- c(94-5.25, 25)

b <- cbind(game$x, game$y)

o1 <- cbind(game$a1_x, game$a1_y)
o2 <- cbind(game$a2_x, game$a2_y)
o3 <- cbind(game$a3_x, game$a3_y)
o4 <- cbind(game$a4_x, game$a4_y)
o5 <- cbind(game$a5_x, game$a5_y)

d1 <- cbind(game$h1_x, game$h1_y)
d2 <- cbind(game$h2_x, game$h2_y)
d3 <- cbind(game$h3_x, game$h3_y)
d4 <- cbind(game$h4_x, game$h4_y)
d5 <- cbind(game$h5_x, game$h5_y)

N <- nrow(game)
o <- array(c(o1,o2,o3,o4,o5), dim = c(N,2,5))
d <- array(c(d1,d2,d3,d4,d5), dim = c(N,2,5))

o <- aperm(o, c(3,1,2))
d <- aperm(d, c(3,1,2))

stan_data <- list(N = N,
                  h = h,
                  b = b,
                  d = d,
                  o = o)

stan_data_low_res <- list(N = length(indxs),
                          h = h,
                          b = b[indxs,],
                          d = d[,indxs,],
                          o = o[,indxs,],
                          indxs = indxs)

saveRDS(stan_data, "defense_evt140.RDS")
saveRDS(stan_data_low_res, "defense_evt140_low_res.RDS")
