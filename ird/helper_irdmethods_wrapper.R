# MAIRE
maire_wrapper = function(data, job, instance, ...) {

  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)

  desired = get_desired_range(data_name = job$prob.name, pred = pred, x_interest = x_interest)
  desired_class = desired[[1]]
  desired_range = desired[[2]]

  pred$class = desired_class

  maire = Maire$new(pred, num_of_iterations = 100L, convergence = TRUE, quiet = FALSE)
  # maire = Maire$new(pred, num_of_iterations = 100L, convergence = FALSE, quiet = FALSE)
  box = maire$find_box(x_interest = x_interest, desired_range = desired_range)

  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 100L)
  boxpost = post$find_box(x_interest = x_interest,
    desired_range = desired_range, box_init = box$box)

  return(list(orig = box, postproc = boxpost))
}

# MaxBox
maxbox_wrapper = function(data, job, instance, ...) {
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)

  desired = get_desired_range(data_name, pred, x_interest)
  desired_class = desired[[1]]
  desired_range = desired[[2]]

  pred$class = desired_class

  method = MaxBox$new(pred, quiet = FALSE)
  box = method$find_box(x_interest = x_interest, desired_range = desired_range)

  # return(box)

  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 100L)
  boxpost = post$find_box(x_interest = x_interest,
    desired_range = desired_range, box_init = box$box)

  return(list(orig = box, postproc = boxpost))

}

# PRIM
prim_wrapper = function(data, job, instance, ...) {
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)

  desired = get_desired_range(data_name, pred, x_interest)
  desired_class = desired[[1]]
  desired_range = desired[[2]]

  pred$class = desired_class

  method = Prim$new(pred)
  box = method$find_box(x_interest = x_interest, desired_range = desired_range)

  # return(box)

  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 100L)
  boxpost = post$find_box(x_interest = x_interest,
    desired_range = desired_range, box_init = box$box)

  return(list(orig = box, postproc = boxpost))
}

# Anchors
anchors_wrapper = function(data, job, instance, ...) {
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)

  desired = get_desired_range(data_name, pred, x_interest)
  desired_class = desired[[1]]
  desired_range = desired[[2]]

  pred$class = desired_class

  method = Anchor$new(pred)
  box = method$find_box(x_interest = x_interest, desired_range = desired_range)

  # return(box)

  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 100L)
  boxpost = post$find_box(x_interest = x_interest,
    desired_range = desired_range, box_init = box$box)

  return(list(orig = box, postproc = boxpost))
}



# # Function to create results table
# get_cf_table = function(cfactuals_obj, this_job) {
#   dt_standard = data.table()
#   if (nrow(cfactuals_obj$data) > 0L) {
#     cfactuals = cfactuals_obj$evaluate()
#     cfactuals_sets = cfactuals_obj$evaluate_set()
#     dt_standard = cbind(cfactuals, "job.id" = this_job$id)
#     attr(dt_standard, "evalsets") = cfactuals_sets
#   }
#   return(dt_standard)
# }

