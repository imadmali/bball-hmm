# create evt244_0021500411.RDS data file

library(dplyr)
library(animation)
source("../graphics.R")

pt <- readRDS("0021500411.RDS")

pt$game <- pt$game %>%
  filter(event_id == 244)

# remove time steps where game clock has stopped
gc_stop_indx <- which(diff(pt$game$game_clock) == 0)
pt$game <- pt$game[-gc_stop_indx,]

saveRDS(pt, "evt244_0021500411.RDS")

ani.options(ani.width=900, ani.height=600, interval= 0.05, autobrowse = FALSE, ani.dev = "png", ani.type = "png")
saveVideo({
  for (i in 1:nrow(pt$game)) {
    plot_fullcourt()
    text(1,48, paste0("Q",pt$game$quarter[i]," | GC: ",pt$game$game_clock[i]), pos=4, cex=1.5)
    plot_shot(pt, loop = i, static = F)
  }
}, video.name = paste0("../media/event_244",".mp4"))