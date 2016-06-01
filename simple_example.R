x <- 1L:10
y <- 1.5*x + 0.5 + rnorm(10)

plot(x, y)
m <- lm(y ~ x)
abline(m)

y2 <- y
y2[10] <- y[10] - 50 

plot(x, y2)
m2 <- lm(y2 ~ x)
abline(m2)

# influence(m)

# cooks.distance(m2)

# library(car)
# leveragePlots(m)

x <- x
y <- y

lm(y ~ x)
mat <- model.matrix(m)
lm.fit(mat, y)
# coefficients


coef_a <- .Call(stats:::C_Cdqrls, mat, y, 1e-07, FALSE)[["coefficients"]][2]
coef_tmp <- coef_a

# which way (positive or negative) changes the a?
y_id <- 1
y_multiplier <- 10
while(coef_tmp > 0) {
  y_tmp <- y
  y_tmp[y_id] <- y_multiplier*y[y_id]
  coef_tmp <- .Call(stats:::C_Cdqrls, mat, y_tmp, 1e-07, FALSE)[["coefficients"]][2]
  y_multiplier <- y_multiplier + 1
  print(coef_tmp)
}
