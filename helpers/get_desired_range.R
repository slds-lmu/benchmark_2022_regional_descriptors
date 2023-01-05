get_desired_range = function(data_name, pred, x_interest) {
  pred_x_interest = pred$predict(x_interest)
  f_hat_data = pred$predict(pred$data$get.x())
  if (pred$task == "classification") {
    desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.max)]
    if (data_name %in% c("diabetes", "tic_tac_toe")) {
      desired_range = c(0.5, 1)
    }  else if (data_name %in% c("cmc", "vehicle")) {
      sdf_hat = 1/2*sd(f_hat_data[[desired_class]])
      desired_range = c(pred_x_interest[[desired_class]] - sdf_hat, pred_x_interest[[desired_class]] + sdf_hat)
      desired_range = c(max(min(desired_range), 0), min(max(desired_range), 1))
    }
  } else {
    sdf_hat = 1/2*sd(f_hat_data[[1]])
    desired_range = c(pred_x_interest[[1]] - sdf_hat, pred_x_interest[[1]] + sdf_hat)
    desired_class = NULL
  }
  return(list(desired_class, desired_range))
}

