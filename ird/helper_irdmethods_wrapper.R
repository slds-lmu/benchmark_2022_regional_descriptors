# MAIRE
maire_wrapper = function(data, job, instance, ...) {
  browser()
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)


  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]

  # REGRESSION: desired_class = NULL

  desired_range = if (pred_x_interest[[desired_class]] < 0.5) {
    c(0, 0.5 - sqrt(.Machine$double.eps))
  }  else {
    c(0.5 + sqrt(.Machine$double.eps), 1)
  }

  maire = Maire$new(pred, num_of_iterations = 100L, quiet = FALSE)
  box = maire$find_box(x_interest = x_interest, desired_class = desired_class,
    desired_range = desired_range)

  # return(box)

  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 30L)
  boxpost = post$find_box(x_interest = x_interest, desired_class = desired_class,
    desired_range = desired_range, box_init = box$box)

  return(orig = box, postproc = boxpost)
}

# MaxBox
maxbox_wrapper = function(data, job, instance, ...) {
  browser()
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)

  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]

  # REGRESSION: desired_class = NULL

  desired_range = if (pred_x_interest[[desired_class]] < 0.5) {
    c(0, 0.5 - sqrt(.Machine$double.eps))
  }  else {
    c(0.5 + sqrt(.Machine$double.eps), 1)
  }

  method = MaxBox$new(pred, quiet = FALSE)
  box = method$find_box(x_interest = x_interest, desired_class = desired_class,
    desired_range = desired_range)

  # return(box)

  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 30L)
  boxpost = post$find_box(x_interest = x_interest, desired_class = desired_class,
    desired_range = desired_range, box_init = box$box)

  return(orig = box, postproc = boxpost)

}

# PRIM
prim_wrapper = function(data, job, instance, ...) {
  browser()
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)


  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]

  # REGRESSION: desired_class = NULL
  desired_range = if (pred_x_interest[[desired_class]] < 0.5) {
    c(0, 0.5 - sqrt(.Machine$double.eps))
  }  else {
    c(0.5 + sqrt(.Machine$double.eps), 1)
  }

  method = Prim$new(pred)
  box = method$find_box(x_interest = x_interest, desired_class = desired_class,
    desired_range = desired_range)

  # return(box)

  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 30L)
  boxpost = post$find_box(x_interest = x_interest, desired_class = desired_class,
    desired_range = desired_range, box_init = box$box)

  return(orig = box, postproc = boxpost)
}



# Function to create results table
get_cf_table = function(cfactuals_obj, this_job) {
  dt_standard = data.table()
  if (nrow(cfactuals_obj$data) > 0L) {
    cfactuals = cfactuals_obj$evaluate()
    cfactuals_sets = cfactuals_obj$evaluate_set()
    dt_standard = cbind(cfactuals, "job.id" = this_job$id)
    attr(dt_standard, "evalsets") = cfactuals_sets
  }
  return(dt_standard)
}

