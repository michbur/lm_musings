x <- 1L:10
y <- 1.5*x + 0.5 + rnorm(10)
y[2] <- y[2] +520

plot(x, y)
m <- lm(y~x)
abline(m)
inf <- influence(m)
inf_dat <- data.frame(hat = inf[["hat"]], sigma = inf[["sigma"]], wt.res = inf[["wt.res"]])

plot(x, inf_dat[["hat"]])
plot(x, inf_dat[["wt.res"]])
plot(x, inf_dat[["sigma"]])
