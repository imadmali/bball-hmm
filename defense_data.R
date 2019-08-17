library(animation)
source("graphics.R")

# reshape pt data for defense hmm in stan

drive <- readRDS("data/evt140_0021500411.RDS")
game <- drive$game[1:100,]

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

saveRDS(stan_data, "data/defense.RDS")

# plot
ani.options(ani.width=900, ani.height=600, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:N) {
    plot_fullcourt()
    text(1,48, paste0("Q",drive$game$quarter[i]," | GC: ",drive$game$game_clock[i]), pos=4, cex=1.5)
    plot_shot(drive, loop = i, static = F)
  }
}, video.name = paste0("media/event_140_sample",".mp4"))
