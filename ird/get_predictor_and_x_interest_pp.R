get_predictor_and_x_interest_pp = function(arg_list, job, data) {
  library(batchtools)
   get_predictor(data, model_name = arg_list$model_name, data_name = job$prob.name, id_x_interest = arg_list$id_x_interest)

  # target_name = names(data)[ncol(data)]
  # is_keras = arg_list$model_name == "neural_network"
  #
  # # if (grepl("run_or_walk_info", job$prob.name, fixed = TRUE)) {
  # #   prob_name = "run_or_walk_info"
  # # } else {
  # prob_name = job$prob.name
  # # }
  #
  # if (prob_name %in% c("diabetes", "tic_tac_toe", "cmc", "vehicle")) {
  #   type = "prob"
  # } else {
  #   type = NULL
  # }
  #
  # if (is_keras) {
  #   if (TEST) {
  #     model_dir = file.path("models/test/keras", arg_list$model_name)
  #   } else {
  #     model_dir = file.path("models/prod/keras", arg_list$model_name)
  #   }
  #   path_pipeline = file.path(model_dir, paste0(prob_name, "_po.rds"))
  #   pipeline = readRDS(path_pipeline)
  #   model_path = file.path(model_dir, paste0(prob_name, "_model.hdf5"))
  #   this_model = load_model_hdf5(model_path)
  #   pred = Predictor$new(
  #     this_model, data = data, y = target_name,
  #     predict.function = function(model, newdata) {
  #       newdata = data.table::as.data.table(newdata)
  #       factor_cols = names(which(sapply(data, is.factor)))
  #       for (factor_col in factor_cols) {
  #         fact_col_data = data[[factor_col]]
  #         value = factor(newdata[[factor_col]], levels = levels(fact_col_data), ordered = is.ordered(fact_col_data))
  #         set(newdata, j = factor_col, value = value)
  #       }
  #       int_cols = names(which(sapply(data, is.integer)))
  #       if (length(int_cols) > 0L) {
  #         newdata[,(int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
  #       }
  #       newdata[, (target_name) := data[[target_name]][1]]
  #       newdata = pipeline$predict(as_task_classif(newdata, target = target_name))[[1]]$data()
  #       yhat = model %>% predict(as.matrix(newdata[, -1L]))
  #       yhat = data.table::as.data.table(yhat)
  #       names(yhat) = levels(data[[target_name]])
  #       yhat
  #     },
  #     type = "prob"
  #   )
  # } else {
  #   model_registry = loadRegistry("models/prod/registry", make.default = FALSE)
  #   model_job_params = unwrap(getJobPars(reg = model_registry))
  #   job_id = model_job_params[problem == prob_name & algorithm == arg_list$model_name]
  #   this_model = loadResult(id = job_id, reg = model_registry)
  #   if (is.list(this_model)) {
  #     pred = this_model[[arg_list$id_x_interest]]
  #   } else {
  #     pred = Predictor$new(this_model, data = data, y = target_name, type = type)
  #   }
  # }
  #
  # print(summary(pred$predict(data)))
  #
  # return(pred)
}



# TEST MODEL OUTPUT!!
if (FALSE) {
  arg_list = list(model_name = "neural_network")
  data = data_list[[1]]
  job = list(prob.name = "credit_g")
  get_predictor_and_x_interest_pp(arg_list = arg_list, job = job, data = data)
}
