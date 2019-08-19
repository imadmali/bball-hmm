# create evt140_0021500411.RDS data file with LaVine speed/dist metrics

library(dplyr)
library(animation)
source("../graphics.R")

pt <- readRDS("0021500411.RDS")

drive <- pt
drive$game <- pt$game %>%
  filter(event_id == 140)

# drop time after the event takes place (i.e. when the game clock stops)
gc_stop_indx <- which(diff(drive$game$game_clock) == 0)
drive$game <- drive$game %>%
  filter(row_number() < head(gc_stop_indx,1))

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

saveRDS(drive, "evt140_0021500411.RDS")

# create animation with variables
ani.options(ani.width=900, ani.height=600, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:nrow(drive$game)) {
    plot_fullcourt()
    text(1,48, paste0("Q",drive$game$quarter[i]," | GC: ",drive$game$game_clock[i]), pos=4, cex=1.5)
    plot_shot(drive, loop = i, static = F)
  }
}, video.name = paste0("../media/event_140",".mp4"))

# create animation with speed/dist variables
ani.options(ani.width=600, ani.height=700, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:nrow(drive$game)) {
    layout(matrix(1:3, ncol = 1), heights = c(1,1,2))
    plot(lavine_dist[-1], type = "l", ylab = "Distance From Hoop", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    plot(lavine_speed[-1], type = "l", ylab = "Speed", xlab = "Time (25hz)")
    abline(v = i, col = "red", lwd = 2)
    plot_fullcourt()
    text(1,48, paste0("Q",drive$game$quarter[i]," | GC: ",drive$game$game_clock[i]), pos=4, cex=1.5)
    plot_shot(drive, loop = i, static = F)
  }
}, video.name = paste0("../media/event_140_stats",".mp4"))

# looks like lavine drives between index 300:400
# during this time his distance from the hoop decreases and his speed increases 
