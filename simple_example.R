# influence(m)

# cooks.distance(m2)

# library(car)
# leveragePlots(m)

source("functions.R")

x <- 1L:10
y <- 1.5*x + 0.5 + rnorm(10)

sim_dat_res <- get_borders(x, y)
plot_borders(sim_dat_res)

as_dat <- read.csv("as_dat.csv")
  
as_dat_res <- get_borders(as_dat[[1]], as_dat[[2]], 50)

mat <- cbind(1, as_dat[[1]])

borderize_single(mat, as_dat[[2]], 2, 50)


plot_borders(as_dat_res)

x <- as_dat[[1]]
y <- as_dat[[2]]
m <- lm(y ~ x)
plot(x, y)
abline(m)

y2 <- as_dat[[2]]
y2[1] <- as_dat_res[["y_border"]][1]
plot(x, y2)
m2 <- lm(y2 ~ x)
abline(m2)
