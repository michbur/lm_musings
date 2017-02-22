library(ggplot2)
library(pbapply)

comp_shift_single <- function(x, y, y_id) {
  
  p_val <- summary(lm(y ~ x))[["coefficients"]][2, 4]
  single_step <- y[y_id]/100
  y_tmp <- y
  multiplier <- 1
  res <- c(NA, NA)
  
  while(p_val < 0.05) {
    y_tmp[y_id] <- y_tmp[y_id] - single_step*multiplier
    p_val <- summary(lm(y_tmp ~ x))[["coefficients"]][2, 4]
    multiplier <- multiplier*1.01
  }
  
  res[1] <- y_tmp[y_id]
  
  p_val <- summary(lm(y ~ x))[["coefficients"]][2, 4]
  y_tmp <- y
  multiplier <- 1
  
  while(p_val < 0.05) {
    y_tmp[y_id] <- y_tmp[y_id] + single_step*multiplier
    p_val <- summary(lm(y_tmp ~ x))[["coefficients"]][2, 4]
    multiplier <- multiplier*1.01
  }
  
  res[2] <- y_tmp[y_id]
  
  res
}

comp_shift <- function(x, y) {
  max_shifts <- do.call(rbind, pblapply(1L:length(y), function(i) comp_shift_single(x, y, i)))
  max_shifts <- t(apply(max_shifts, 1, sort))
  colnames(max_shifts) <- c("y_min", "y_max")
  cbind(x = x, y = y, max_shifts)
}

as_dat <- read.csv("as_dat.csv")
x <- as_dat[[1]]
y <- as_dat[[2]]

# get minimum shifts that alter significance for each point
shifts_as <- data.frame(comp_shift(x, y))

fit <- lm(y ~ x)
pred <- predict(fit, newdata = data.frame(x = x), interval = "confidence", level = 0.99)
pred_df <- cbind(shifts_as, pred)

# choose points whose shifts altering the significance are in the uncertainty surrounding 
# the predicted y-value of a single sampled point 
points_ids <- which(pred_df[["y_min"]] > pred_df[["lwr"]] | pred_df[["y_max"]] < pred_df[["upr"]])

levels_pred <- data.frame(level = c(0.01, 0.05, 1L:9/10, 0.95, 0.99),
                          shifted =  sapply(c(0.01, 0.05, 1L:9/10, 0.95, 0.99), function(i) {
                            borders <- predict(fit, newdata = data.frame(x = x[points_ids]), 
                                               interval = "confidence", level = i)[, -1]
                            
                            any(pred_df[["y_min"]][points_ids] > borders[, "lwr"] | 
                                  pred_df[["y_max"]][points_ids] < borders[, "upr"])
                          }))

# the p-value is shifted even for the lowest level 0.01, so I'd give this linear model 
# 0.01 "shift measure". Value of 1 equals to a good model, the closest we are to zero,
# the more problematics model is.

ggplot(pred_df, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(x = x, y = y_min)) +
  geom_line(aes(x = x, y = y_max)) +
  geom_line(aes(x = x, y = lwr), color = "red") +
  geom_line(aes(x = x, y = upr), color = "red") +
  coord_cartesian(ylim = c(-25, 25))

# I had to trim the plot, but you see that the main problem with the significance is hidden in the
# last point
