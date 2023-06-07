# MAIRE
maire_wrapper = function(data, job, instance, ...) {
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)

  desired = get_desired_range(data_name = job$prob.name, pred = pred, x_interest = x_interest)
  desired_class = desired[[1]]
  desired_range = desired[[2]]
  pred$class = desired_class

  # Get data (either training or sampled in largest box)
  filenam = paste0(paste(job$prob.name, arg_list$model_name, arg_list$id_x_interest, sep = "_"), ".rds")
  obsdata = readRDS(file.path("dataeval/prod", paste0("data_inbox_", arg_list$datastrategy), filenam))
  box_largest = readRDS(file.path("dataeval/prod/largest_box", filenam))

  # Run method
  start_time = Sys.time()
  method = Maire$new(pred, num_of_iterations = 100L, convergence = TRUE, quiet = TRUE)
  # method = Maire$new(pred, num_of_iterations = 100L, convergence = FALSE, quiet = FALSE)
  box = method$find_box(x_interest = x_interest, desired_range = desired_range, obsdata = obsdata, box_largest = box_largest)
  end_time = Sys.time()

  # save info on runtime and calls to fhat
  attr(box, "runtime") = as.numeric(end_time - start_time)
  attr(box, "calls_fhat") = method$calls_fhat

  # Postprocessing
  start_time = Sys.time()
  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 100L, quiet = TRUE)
  boxpost = post$find_box(x_interest = x_interest,
    desired_range = desired_range, box_init = box$box, box_largest = box_largest)
  end_time = Sys.time()

  # save info on runtime and calls to fhat
  attr(boxpost, "runtime") = as.numeric(end_time - start_time)
  attr(boxpost, "calls_fhat") = post$calls_fhat

  return(list(orig = box, postproc = boxpost))
}

# MaxBox
maxbox_wrapper = function(data, job, instance, ...) {
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)

  desired = get_desired_range(data_name = job$prob.name, pred = pred, x_interest = x_interest)
  desired_class = desired[[1]]
  desired_range = desired[[2]]

  pred$class = desired_class

  # Get data (either training or sampled in largest box)
  filenam = paste0(paste(job$prob.name, arg_list$model_name, arg_list$id_x_interest, sep = "_"), ".rds")
  obsdata = readRDS(file.path("dataeval/prod", paste0("data_inbox_", arg_list$datastrategy), filenam))
  box_largest = readRDS(file.path("dataeval/prod/largest_box", filenam))

   # Run method
  start_time = Sys.time()
  method = MaxBox$new(pred, efficient = TRUE, quiet = TRUE)
  box = method$find_box(x_interest = x_interest, desired_range = desired_range, obsdata = obsdata, box_largest = box_largest)
  end_time = Sys.time()

  # save info on runtime and calls to fhat
  attr(box, "runtime") = as.numeric(end_time - start_time)
  attr(box, "calls_fhat") = method$calls_fhat

  # Postprocessing
  start_time = Sys.time()
  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 100L, quiet = TRUE)
  boxpost = post$find_box(x_interest = x_interest,
    desired_range = desired_range, box_init = box$box, box_largest = box_largest)
  end_time = Sys.time()

  # save info on runtime and calls to fhat
  attr(boxpost, "runtime") = as.numeric(end_time - start_time)
  attr(boxpost, "calls_fhat") = post$calls_fhat

  return(list(orig = box, postproc = boxpost))
}

# PRIM
prim_wrapper = function(data, job, instance, ...) {
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)

  desired = get_desired_range(data_name = job$prob.name, pred = pred, x_interest = x_interest)
  desired_class = desired[[1]]
  desired_range = desired[[2]]

  pred$class = desired_class

  # Get data (either training or sampled in largest box)
  filenam = paste0(paste(job$prob.name, arg_list$model_name, arg_list$id_x_interest, sep = "_"), ".rds")
  obsdata = readRDS(file.path("dataeval/prod", paste0("data_inbox_", arg_list$datastrategy), filenam))
  box_largest = readRDS(file.path("dataeval/prod/largest_box", filenam))

   # Run method
  start_time = Sys.time()
  method = Prim$new(pred, quiet = TRUE)
  box = method$find_box(x_interest = x_interest, desired_range = desired_range, obsdata = obsdata, box_largest = box_largest)
  end_time = Sys.time()

  # save info on runtime and calls to fhat
  attr(box, "runtime") = as.numeric(end_time - start_time)
  attr(box, "calls_fhat") = method$calls_fhat

  # Postprocessing
  start_time = Sys.time()
  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 100L, quiet = TRUE)
  boxpost = post$find_box(x_interest = x_interest,
    desired_range = desired_range, box_init = box$box, box_largest = box_largest)
  end_time = Sys.time()

  # save info on runtime and calls to fhat
  attr(boxpost, "runtime") = as.numeric(end_time - start_time)
  attr(boxpost, "calls_fhat") = post$calls_fhat

  return(list(orig = box, postproc = boxpost))
}

# Anchors
anchors_wrapper = function(data, job, instance, ...) {
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)

  desired = get_desired_range(data_name = job$prob.name, pred = pred, x_interest = x_interest)
  desired_class = desired[[1]]
  desired_range = desired[[2]]

  pred$class = desired_class

  # Get data (either training or sampled in largest box)
  filenam = paste0(paste(job$prob.name, arg_list$model_name, arg_list$id_x_interest, sep = "_"), ".rds")
  obsdata = readRDS(file.path("dataeval/prod", paste0("data_inbox_", arg_list$datastrategy), filenam))
  box_largest = readRDS(file.path("dataeval/prod/largest_box", filenam))

   # Run method
  start_time = Sys.time()
  method = Anchor$new(pred, quiet = FALSE)
  box = method$find_box(x_interest = x_interest, desired_range = desired_range, obsdata = obsdata, box_largest = box_largest)
  end_time = Sys.time()

  # save info on runtime and calls to fhat
  attr(box, "runtime") = as.numeric(end_time - start_time)
  attr(box, "calls_fhat") = method$calls_fhat

  # Postprocessing
  start_time = Sys.time()
  post = PostProcessing$new(pred, subbox_relsize = 0.1, evaluation_n = 100L, quiet = FALSE)
  boxpost = post$find_box(x_interest = x_interest,
    desired_range = desired_range, box_init = box$box, box_largest = box_largest)
  end_time = Sys.time()

  # save info on runtime and calls to fhat
  attr(boxpost, "runtime") = as.numeric(end_time - start_time)
  attr(boxpost, "calls_fhat") = post$calls_fhat

  return(list(orig = box, postproc = boxpost))
}

