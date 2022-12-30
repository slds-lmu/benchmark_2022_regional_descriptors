get_predictor_and_x_interest_pp = function(arg_list, job, data) {
  library(batchtools)
  target_name = names(data)[ncol(data)]
  is_keras = arg_list$model_name == "neural_network"

  # if (grepl("run_or_walk_info", job$prob.name, fixed = TRUE)) {
  #   prob_name = "run_or_walk_info"
  # } else {
  prob_name = job$prob.name
  # }

  if (is_keras) {
    if (TEST) {
      model_dir = file.path("models/test/keras", arg_list$model_name)
    } else {
      model_dir = file.path("models/prod/keras", arg_list$model_name)
    }
    path_pipeline = file.path(model_dir, paste0(prob_name, "_po.rds"))
    pipeline = readRDS(path_pipeline)
    model_path = file.path(model_dir, paste0(prob_name, "_model.hdf5"))
    this_model = load_model_hdf5(model_path)
    pred = Predictor$new(
      this_model, data = data, y = target_name,
      predict.function = function(model, newdata) {
        newdata = data.table::as.data.table(newdata)
        factor_cols = names(which(sapply(data, is.factor)))
        for (factor_col in factor_cols) {
          fact_col_data = data[[factor_col]]
          value = factor(newdata[[factor_col]], levels = levels(fact_col_data), ordered = is.ordered(fact_col_data))
          set(newdata, j = factor_col, value = value)
        }
        int_cols = names(which(sapply(data, is.integer)))
        if (length(int_cols) > 0L) {
          newdata[,(int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
        }
        newdata[, (target_name) := data[[target_name]][1]]
        newdata = pipeline$predict(as_task_classif(newdata, target = target_name))[[1]]$data()
        yhat = model %>% predict(as.matrix(newdata[, -1L]))
        yhat = data.table::as.data.table(yhat)
        names(yhat) = levels(data[[target_name]])
        yhat
      },
      type = "prob"
    )
  } else {
    model_registry = loadRegistry("models/prod/registry", make.default = FALSE)
    model_job_params = unwrap(getJobPars(reg = model_registry))
    job_id = model_job_params[problem == prob_name & algorithm == arg_list$model_name]
    this_model = loadResult(id = job_id, reg = model_registry)
    if (class(this_model)[1] == "constparty") {
      x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
      attr(this_model, "x_interest") = x_interest
      pred = Predictor$new(model = this_model, data = data, y = target_name,
        predict.function = function(model, newdata) {
          node_interest = predict(model, newdata = attr(model, "x_interest"), type = "node")
          prediction = ifelse(predict(model, newdata = newdata, type = "node") == node_interest, 1, 0)
      })
    } else {
      pred = Predictor$new(this_model, data = data, y = target_name, type = "prob" )
    }
    return(pred)
  }

  # summary(pred$predict(data))

  pred
}


# TEST MODEL OUTPUT!!
if (FALSE) {
  arg_list = list(model_name = "neural_network")
  data = data_list[[1]]
  job = list(prob.name = "credit_g")
  get_predictor_and_x_interest_pp(arg_list = arg_list, job = job, data = data)
}
