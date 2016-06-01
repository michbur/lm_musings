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

lm.fit(mat, y)
# coefficients


borderize_single <- function(mat, y, y_id, step) {
  y_tmp <- y
  y_tmp[y_id] <- 2*y[y_id]
  coef_tmp <- .Call(stats:::C_Cdqrls, mat, y_tmp, 1e-07, FALSE)[["coefficients"]][2] 
  coef_raw <- .Call(stats:::C_Cdqrls, mat, y, 1e-07, FALSE)[["coefficients"]][2]
  
  step_multiplier <- 1
  
  if(coef_raw - coef_tmp > 0) {
    top = 1
    while(coef_tmp > 0) {
      y_tmp <- y
      y_tmp[y_id] <- y_tmp[y_id] + step*step_multiplier
      coef_tmp <- .Call(stats:::C_Cdqrls, mat, y_tmp, 1e-07, FALSE)[["coefficients"]][2]
      step_multiplier <- step_multiplier + 1
    }
  } else {
    top = 0
    while(coef_tmp > 0) {
      y_tmp <- y
      y_tmp[y_id] <- y_tmp[y_id] - step*step_multiplier
      coef_tmp <- .Call(stats:::C_Cdqrls, mat, y_tmp, 1e-07, FALSE)[["coefficients"]][2]
      step_multiplier <- step_multiplier + 1
    }
  }
  
  c(y_border = y_tmp[y_id], top = top)
}


borderize <- function(mat, y, step)
  t(sapply(1L:length(y), function(i) { 
    borderize_single(mat = mat, y = y, y_id = i, step = step)
  }))



get_borders <- function(x, y, step = 50) {
  mat <- model.matrix(m)
  borders <- borderize(mat, y, step)
  data.frame(x = x, y = y, borders)
}

plot_borders <- function(border_df)
  ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  geom_point(aes(x = x, y = y_border, color = factor(top))) +
  geom_polygon(aes(x = x, y = y_border, fill = factor(top)))

read.csv("as_dat.csv")
  