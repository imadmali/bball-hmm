
drive <- pt
drive$game <-  pt$game %>%
  filter(event_id == 140)

lavine_coords <- drive$game %>%
  select(game_clock, unknown1, a5_ent, a5_x, a5_y) %>%
  mutate(wc_diff = c(NA, tail(unknown1/1e3,-1) - head(unknown1/1e3,-1)))

lavine_speed <- c()
for (i in 2:nrow(lavine_coords)) {
  d_mat <- lavine_coords[(i-1):i, c(4,5)]
  lavine_speed[i-1] <- dist(d_mat) / lavine_coords$wc_diff[i]
}

right_hoop <- c((94 - 5.25), 25)
lavine_dist <- c()
for (i in 1:nrow(lavine_coords)) {
  d_mat <- rbind(lavine_coords[i,c(4,5)],
                 right_hoop)
  lavine_dist[i] <- dist(d_mat)
}
