#' @param defense_data The data object saved to data/defense_example.rDS
plt_defense_example <- function(defense_data, ...) {
  list2env(defense_data, environment())
  plot(h[1], h[2], xlim = c(-2,2), ylim = c(-2,2), pch = 19,
       xlab = "x coordinate", ylab = "y coordinate", ...)
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
}

circle <- function(x, y, r, from=0, to=2*pi, lines=FALSE, ...) {
  theta <- seq(from, to, length=100)
  if (lines)
    lines(x + r * cos(theta), y + r * sin(theta), ...)
  else polygon(x + r * cos(theta), y + r * sin(theta), ...)
}

fullcourt_lines <- function() {
  rect(0, 0, 94, 50)
  points(c(5.25, 94 - 5.25), c(25, 25), cex = 2)
  # circle(94 - 5.25, 25, 9/12, ..., col = "#808080")
  segments(47, 0, 47, 50)
  circle(47, 25, 8)
  circle(47, 25, 2, col = "lightgray")
  theta1 <- acos((25 - 35 / 12) / 23.75)
  circle(5.25, 25, 23.75, -pi / 2 + theta1, pi / 2 - theta1, TRUE)
  circle(94 - 5.25, 25, 23.75, pi / 2 + theta1, 3 * pi / 2 - theta1, TRUE)
  segments(0, 35/12, 5.25 + 23.75 * sin(theta1), 35 / 12)
  segments(0, 50 - 35 / 12, 5.25 + 23.75 * sin(theta1), 50 - 35 / 12)
  segments(94, 35 / 12, 94 - 5.25 - 23.75 * sin(theta1), 35 / 12)
  segments(94, 50 - 35 / 12, 94 - 5.25 - 23.75 * sin(theta1), 50 - 35 / 12)
  circle(19, 25, 6, -pi/2, pi/2, TRUE)
  circle(19, 25, 6, pi/2, 3 * pi/2, TRUE, lty = 2)
  circle(94 - 19, 25, 6, pi/2, 3 * pi/2, TRUE)
  circle(94 - 19, 25, 6, -pi/2, pi/2, TRUE, lty = 2)
  circle(5.25, 25, 4, -pi/2, pi/2, TRUE)
  circle(94 - 5.25, 25, 4, pi/2, 3 * pi/2, TRUE)
  rect(0, 17, 19, 33, border = "gray")
  rect(94, 17, 94 - 19, 33, border = "gray")
}

plot_fullcourt <- function() {
  # court is (0,94) x (0,50)
  plot(0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
       xlim = c(0,94), ylim = c(0,50))
  fullcourt_lines()
}

jersey <- function(playerid, player_table) {
  return(player_table$jersey[which(player_table$playerid == playerid)])
}

plot_shot <- function(obj, loop = 1, static = FALSE) {
  # browser()
  obj_game <- obj$game
  home <- obj$players$home
  visitor <- obj$players$visitor
  home_abb <-  obj$players$home$abb[1]
  visitor_abb <- obj$players$visitor$abb[1]
  # stuff for gif
  past <- 0
  if (static) {
    future <- nrow(obj_game)
  }
  else {
    future <- loop
    if (loop > 10)
      past <- future - 10
    else
      past <- 0
  }
  # else {
  #   future <- past + loop
  # }
  # transparent colors
  home_col <- "#00834890"
  visitor_col <- "#002B5C90"
  # trajectory
  lines(obj_game$x[past:future], obj_game$y[past:future], col = "#ffa50020", lwd = 3)
  lines(obj_game$a1_x[past:future], obj_game$a1_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$a2_x[past:future], obj_game$a2_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$a3_x[past:future], obj_game$a3_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$a4_x[past:future], obj_game$a4_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$a5_x[past:future], obj_game$a5_y[past:future], col = visitor_col, lwd = 3)
  lines(obj_game$h1_x[past:future], obj_game$h1_y[past:future], col = home_col, lwd = 3)
  lines(obj_game$h2_x[past:future], obj_game$h2_y[past:future], col = home_col, lwd = 3)
  lines(obj_game$h3_x[past:future], obj_game$h3_y[past:future], col = home_col, lwd = 3)
  lines(obj_game$h4_x[past:future], obj_game$h4_y[past:future], col = home_col, lwd = 3)
  lines(obj_game$h5_x[past:future], obj_game$h5_y[past:future], col = home_col, lwd = 3)
  # ending position
  points(obj_game$a1_x[future], obj_game$a1_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$a2_x[future], obj_game$a2_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$a3_x[future], obj_game$a3_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$a4_x[future], obj_game$a4_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$a5_x[future], obj_game$a5_y[future], col = visitor_col, pch = 20, cex = 6)
  points(obj_game$h1_x[future], obj_game$h1_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$h2_x[future], obj_game$h2_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$h3_x[future], obj_game$h3_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$h4_x[future], obj_game$h4_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$h5_x[future], obj_game$h5_y[future], col = home_col, pch = 20, cex = 6)
  points(obj_game$x[future], obj_game$y[future], col = "#ffa500", cex = 3.5)
  circle(obj_game$x[future], obj_game$y[future], 9.55/2/12, col = "#ffa500", border = NA)
  # player identifier
  text(obj_game$a1_x[future], obj_game$a1_y[future], jersey(obj$game$a1_ent[future], visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$a2_x[future], obj_game$a2_y[future], jersey(obj$game$a2_ent[future], visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$a3_x[future], obj_game$a3_y[future], jersey(obj$game$a3_ent[future], visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$a4_x[future], obj_game$a4_y[future], jersey(obj$game$a4_ent[future], visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$a5_x[future], obj_game$a5_y[future], jersey(obj$game$a5_ent[future], visitor), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h1_x[future], obj_game$h1_y[future], jersey(obj$game$h1_ent[future], home), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h2_x[future], obj_game$h2_y[future], jersey(obj$game$h2_ent[future], home), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h3_x[future], obj_game$h3_y[future], jersey(obj$game$h3_ent[future], home), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h4_x[future], obj_game$h4_y[future], jersey(obj$game$h4_ent[future], home), col = "#ffffff", cex = 1, font = 2)
  text(obj_game$h5_x[future], obj_game$h5_y[future], jersey(obj$game$h5_ent[future], home), col = "#ffffff", cex = 1, font = 2)
}
