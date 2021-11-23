
library(dplyr)
library(ggplot2)



# helper function to get points on an ellipse -----------------------------
# Note: angles must be specified in units of pi.
get_ellipsePointsDat <- function(center_x, center_y, radius_x, radius_y,
                                 angle_min, angle_max) {
  ellipsis_dat <- data.frame(
    x = center_x + radius_x * cos(seq(angle_min, angle_max, length.out = 500)),
    y = center_y + radius_y * sin(seq(angle_min, angle_max, length.out = 500))
  )
  
  return(ellipsis_dat)
}



# define data.frame objects for drawing all lines -------------------------
mainRedLines_dat <- data.frame(
  x     = c(0,2.15, 0,0,    0,1.1),
  y     = c(0,0,    0,1.15, 0,1.1),
  group = c(1,1,    2,2,    3,3)
)

furtherRedLines_dat <- data.frame(
  x     = c(0,.6, 0,.2, .4,1.4, .8,1.8, 1.2,2, 1.6,2),
  y     = c(.4,1, .8,1, 0,1,    0,1,    0,.8,  0,.4),
  group = c(4,4,  5,5,  6,6,    7,7,    8,8,   9,9)
)

blueLines_dat <- data.frame(
  x     = rep(c(0,2),   times = 2),
  y     = rep(c(.4,.8), each = 2),
  group = rep(1:2,      each = 2)
)

greenLines_dat <- data.frame(
  x     = rep(c(.4,.8,1.2,1.6,2), each = 2),
  y     = rep(c(0,1),             times = 5),
  group = rep(1:5,                each = 2)
)

A_dat <- data.frame(
  x     = c(0,.4,  0,.15, .15,.3, .3,.49,  .5,.4, .5,.6, .49,.51, .51,.6),
  y     = c(.4,.8, .4,.4, .4,.55, .55,.55, .8,.8, .8,.6, .55,.51, .51,.6),
  group = c(1,1,   8,8,   7,7,    6,6,     2,2,   3,3,   5,5,     4,4),
  type  = 1 # "1" for defining the main area of the polygon
) %>% arrange(desc(group))

A_hole_dat <- data.frame(
  x     = c(.35,.47, .35,.44, .47,.44),
  y     = c(.6,.6,   .6,.695, .6,.695),
  group = c(1,1,     2,2,     3,3),
  type  = 2 # "2" for defining a hole in the polygon
) %>% arrange(y)

P_dat <- data.frame(
  x     = c(.8,.8, .95,.8, .8,1.05, .95,.95, 1.05,.95),
  y     = c(.1,.7, .1,.1,  .7,.7,   .4,.1,   .4,.4),
  group = c(1,1,   5,5,    2,2,     4,4,     3,3)
) %>% arrange(group)

C_dat <- data.frame(
  x     = c(1.9,1.9, 1.9,1.9, 1.8,1.9, 1.8,1.9, 1.9,1.85, 1.9,1.85) - .07,
  y     = c(.4,.32,  0,.08,   .4,.4,   0,0,     .32,.32,  .08,.08),
  group = c(2,2,     5,5,     1,1,     4,4,     3,3,      6,6)
) %>% arrange(group)

P_outerCircle_dat <- get_ellipsePointsDat(center_x  = 1.05,   center_y  = .55,
                                          radius_x  = .25,    radius_y  = .15,
                                          angle_min = -.5*pi, angle_max = .5*pi)

P_hole_dat <- get_ellipsePointsDat(center_x  = 1.02,    center_y  = .55,
                                   radius_x  = .18,    radius_y  = .09,
                                   angle_min = -.5*pi, angle_max = .5*pi) %>% 
  filter(y <= x - 0.4) %>% 
  dplyr::bind_rows(data.frame(x = .92, y = .52)) %>% 
  dplyr::bind_rows(data.frame(x = .92, y = .46)) %>% 
  dplyr::bind_rows(data.frame(x = .92, y = .46), .)

C_outerCircle_dat <- get_ellipsePointsDat(center_x  = 1.8-.07,   center_y  = .2,
                                          radius_x  = .4,    radius_y  = .2,
                                          angle_min = .5*pi, angle_max = 1.5*pi) %>% 
  filter(y <= x - 1.2) %>% 
  arrange(y)

C_innerCircle_dat <- get_ellipsePointsDat(center_x  = 1.85-.07,  center_y  = .2,
                                          radius_x  = .28,    radius_y  = .12,
                                          angle_min = .5*pi, angle_max = 1.5*pi)

A_poly_dat <- A_dat %>% dplyr::bind_rows(A_hole_dat)

P_poly_dat <- P_dat[P_dat$group <= 2,] %>% 
  dplyr::bind_rows(P_outerCircle_dat) %>%
  dplyr::bind_rows(P_dat[P_dat$group > 2,]) %>% 
  mutate(type = 1) # "1" for defining the main area of the polygon
P_poly_dat <- P_hole_dat %>% 
  mutate(type = 2) %>% # "2" for defining a hole in the polygon
  dplyr::bind_rows(P_poly_dat, .)

C_poly_dat <- C_dat[C_dat$group <= 3,] %>% 
  dplyr::bind_rows(C_innerCircle_dat) %>%
  dplyr::bind_rows(C_dat[C_dat$group > 3,]) %>% 
  dplyr::bind_rows(C_outerCircle_dat)



# main plot ---------------------------------------------------------------
ggplot() +
  geom_line(data = mainRedLines_dat,    aes(x, y, group = group), size = 1.2, col = "firebrick3", arrow = arrow(angle = 15, ends = "last", type = "closed")) +
  geom_line(data = furtherRedLines_dat, aes(x, y, group = group), lty = 2, size = 1, col = "firebrick3") +
  geom_line(data = blueLines_dat,       aes(x, y, group = group), lty = 2, size = 1, col = "dodgerblue3") +
  geom_line(data = greenLines_dat,      aes(x, y, group = group), lty = 2, size = 1, col = "chartreuse4") +
  geom_line(data = A_dat, aes(x, y, group = group), size = 2, lineend = "square") +
  geom_line(data = P_dat, aes(x, y, group = group), size = 2, lineend = "square") +
  geom_line(data = C_dat, aes(x, y, group = group), size = 2, lineend = "square") +
  geom_line(data = A_hole_dat, aes(x, y, group = group), size = 2, lineend = "square") +
  geom_path(data = P_outerCircle_dat, aes(x, y), size = 2, lineend = "square") +
  geom_path(data = P_innerCircle_dat, aes(x, y), size = 2, lineend = "square") +
  geom_path(data = C_outerCircle_dat, aes(x, y), size = 2, lineend = "square") +
  geom_path(data = C_innerCircle_dat, aes(x, y), size = 2, lineend = "square") +
  geom_polygon(data = A_poly_dat, aes(x, y, subgroup = type)) +
  geom_polygon(data = P_poly_dat, aes(x, y, subgroup = type)) +
  geom_polygon(data = C_poly_dat, aes(x, y)) +
  theme(axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.background = element_blank(),
        legend.position  = "none",
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.grid       = element_blank())
ggsave("main_plot.pdf", width = 7, height = 5)
