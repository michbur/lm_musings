# influence(m)

# cooks.distance(m2)

# library(car)
# leveragePlots(m)

source("functions.R")

# easy example ----------------------------------
x <- 1L:10
y <- 1.5*x + 0.5 + rnorm(10)

dat <- get_borders(x, y)

plot_borders(dat)

# harder example ----------------------------------
x <- 1L:10
y <- 1.5*x + 0.5 + rnorm(10)*10

dat <- get_borders(x, y)

plot_borders(dat)

# ideal example ----------------------------------
x <- 1L:10
y <- 1.5*x + 0.5 + rnorm(10)
y[2] <- y[2] + 20
dat <- get_borders(x, y)

plot_borders(dat)

# there are no ideal example. the distance between border and the point always depends on its position in the regression.

# AS dat ----------------------------

as_dat <- read.csv("as_dat.csv")
  
as_dat_res <- get_borders(as_dat[[1]], as_dat[[2]], 0.1)

plot_borders(as_dat_res)
