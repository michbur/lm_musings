# influence(m)

# cooks.distance(m2)

# library(car)
# leveragePlots(m)

source("functions.R")

# easy example ----------------------------------
x <- 1L:10
y <- 1.5*x + 0.5 + rnorm(10)

dat <- get_borders(x, y)

plot(dat[["x"]], dat[["borders"]], col = "red")
points(dat[["x"]], dat[["y"]])
abline(lm(y ~ x))


# harder example ----------------------------------
x <- 1L:10
y <- 1.5*x + 0.5 + rnorm(10)*10

sim_dat_res <- get_borders(x, y)

plot(dat[["x"]], dat[["borders"]], col = "red")
points(dat[["x"]], dat[["y"]])
abline(lm(y ~ x))

# AS dat

as_dat <- read.csv("as_dat.csv")
  
as_dat_res <- get_borders(as_dat[[1]], as_dat[[2]], 1)

plot(as_dat_res[["x"]], as_dat_res[["borders"]], col = "red")
points(as_dat_res[["x"]], as_dat_res[["y"]])
abline(lm(y ~ x))

ggplot(as_dat_res, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  geom_point(aes(x = x, y = y_border, color = factor(side))) +
  geom_polygon(aes(x = x, y = y_border, fill = factor(side)))
