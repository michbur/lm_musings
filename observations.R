x <- 1L:11
y <- 1.5*x + 0.5 + rnorm(11)*10

y2 <- y
y2[6] <- y[6] + 2000000

lm(y2 ~ x)

y2[6] <- y[6] + 2000000

lm(y2 ~ x)

# if the number of points is unequal, the middle point does not seem to have any influence
