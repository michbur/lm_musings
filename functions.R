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

check_side(1L:10, 5)

borderize_single <- function(mat, y, y_id, step) {
  
  pos <- check_side(y, y_id)
  
  coef_raw <- .Call(stats:::C_Cdqrls, mat, y, 1e-07, FALSE)[["coefficients"]][2]
  
  step_multiplier <- 1
  
  if((coef_raw > 0 & pos == "left") | (coef_raw < 0 & pos == "right")) {
    top = 1
    while((coef_raw > 0 & pos == "left") | (coef_raw < 0 & pos == "right")) {
      y_tmp <- y
      y_tmp[y_id] <- y_tmp[y_id] + step*step_multiplier
      coef_tmp <- .Call(stats:::C_Cdqrls, mat, y_tmp, 1e-07, FALSE)[["coefficients"]][2]
      step_multiplier <- step_multiplier + 1
    }
  } 
  
  if((coef_raw < 0 & pos == "left") | (coef_raw > 0 & pos == "right")) {
    top = -1
    while((coef_raw > 0 & pos == "left") | (coef_raw < 0 & pos == "right")) {
      y_tmp <- y
      y_tmp[y_id] <- y_tmp[y_id] - step*step_multiplier
      coef_tmp <- .Call(stats:::C_Cdqrls, mat, y_tmp, 1e-07, FALSE)[["coefficients"]][2]
      step_multiplier <- step_multiplier + 1
    }
  }
  
  if(pos == "middle") {
    y_tmp <- y
    top <- 0
  }
  
  c(y_adj = y_tmp, top = top)
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
  ggplot(border_df, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  geom_point(aes(x = x, y = y_border, color = factor(top))) +
  geom_polygon(aes(x = x, y = y_border, fill = factor(top)))
