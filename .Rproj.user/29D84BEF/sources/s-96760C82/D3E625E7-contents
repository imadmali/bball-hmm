library(dplyr)
library(animation)

source("graphics.R")

pt <- readRDS("data/0021500411.RDS")

drive <- pt
drive$game <-  pt$game %>%
  filter(event_id == 140)

lavine_coords <- drive$game %>%
  select(game_clock, unknown1, a5_ent, a5_x, a5_y) %>%
  mutate(wc_diff = c(NA, tail(unknown1/1e3,-1) - head(unknown1/1e3,-1)))

# construct player speed for event
lavine_speed <- c(NA)
for (i in 2:nrow(lavine_coords)) {
  d_mat <- lavine_coords[(i-1):i, c(4,5)]
  lavine_speed[i] <- dist(d_mat) / lavine_coords$wc_diff[i]
}

# construct player distance from hoop for event
right_hoop <- c((94 - 5.25), 25)
lavine_dist <- c()
for (i in 1:nrow(lavine_coords)) {
  d_mat <- rbind(lavine_coords[i,c(4,5)],
                 right_hoop)
  lavine_dist[i] <- dist(d_mat)
}

drive$game$lavine_speed <- lavine_speed
drive$game$lavine_dist <- lavine_dist

saveRDS(drive, "data/evt140_0021500411.RDS")

# create animation
ani.options(ani.width=600, ani.height=900, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:nrow(drive$game)) {
    # layout(matrix(1:4, ncol = 1), heights = c(1,1,1,2))
    layout(matrix(1:3, ncol = 1), heights = c(1,1,2))
    plot(lavine_dist[-1], type = "l", ylab = "Distance From Hoop", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    plot(lavine_speed, type = "l", ylab = "Speed", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    # plot(round(states), pch = 20, cex = 0.5, ylab = "State", xlab = "Time (25hz)",
    #      ylim = c(0.5, 2.5), yaxt = "n")
    # axis(2, c(1,2), c("None", "Drive"), las = 2)
    # abline(v = i, col = "red", lwd = 2)
    plot_fullcourt()
    text(1,1, paste0("Q",drive$game$quarter[i]," | GC: ",drive$game$game_clock[i]), pos=4, cex=0.8)
    plot_shot(drive, loop = i, static = F)
  }
}, video.name = paste0("media/drive_event_model_140",".mp4"))

# looks like lavine drives between index 300:400
# during this time his distance from the hoop decreases and his speed increases 
