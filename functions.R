require(ggplot2)

check_side <- function(y, y_id) {
  half <- ifelse(length(y) %% 2 == 0, length(y)/2, (length(y) - 1)/2)
  if(y_id <= half) {
    "left"
  } else {
    if(length(y) %% 2 == 0 | y_id > half + 1) {
      "right"
    } else {
      "middle"
    }
  }
} 

# check_side(1L:10, 5)
# check_side(1L:11, 6)


# x <- 1L:10
# y <- 1.5*x + 0.5 + rnorm(10)
# mat <- cbind('(Intercept)' = 1, x)

borderize_single <- function(mat, y, y_id, step) {
  
  pos <- check_side(y, y_id)
  
  coef_raw <- .Call(stats:::C_Cdqrls, mat, y, 1e-07, FALSE)[["coefficients"]][2]
  
  step_multiplier <- 1
  
  if(pos != "middle") {
    diff_or_sum <- if((coef_raw > 0 & pos == "left") | (coef_raw < 0 & pos == "right")) {
      1
    } else {
      -1
    }
    
    coef_tmp <- coef_raw
    
    while(sign(coef_tmp) == sign(coef_raw)) {
      y_tmp <- y
      y_tmp[y_id] <- y_tmp[y_id] + step*step_multiplier*diff_or_sum
      coef_tmp <- .Call(stats:::C_Cdqrls, mat, y_tmp, 1e-07, FALSE)[["coefficients"]][2]
      step_multiplier <- step_multiplier + 1
    }
    
  } else {
    y_tmp <- y
    y_tmp[y_id] <- y_tmp[y_id]
  }
  
  c(y_border = y_tmp[y_id], side = ifelse(pos == "middle", 0, ifelse(pos == "left", -1, 1)))
}

#borderize_single(mat, y, 6, 1)

borderize <- function(mat, y, step)
  sapply(1L:length(y), function(i) {
    borderize_single(mat = mat, y = y, y_id = i, step = step)
  })

#borderize(mat, y, 1)

get_borders <- function(x, y, step = 50) {
  mat <- model.matrix(lm(y ~ x))
  borders <- borderize(mat, y, step)
  data.frame(x = x, y = y, t(borders))
}

#dat <- get_borders(x, y, 1)

plot_borders <- function(dat)
  ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  geom_point(aes(x = x, y = y_border, color = factor(side))) +
  geom_line(aes(x = x, y = y_border, color = factor(side)), linetype = "dashed")

