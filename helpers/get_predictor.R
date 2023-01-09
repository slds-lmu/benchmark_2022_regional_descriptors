get_predictor = function(data, model_name, data_name, id_x_interest) {

  target_name = names(data)[ncol(data)]
  is_keras = model_name == "neural_network"

  if (data_name %in% c("diabetes", "tic_tac_toe", "cmc", "vehicle")) {
    type = "prob"
    task = "classification"
  } else {
    type = NULL
    task = "regression"
  }

  if (is_keras) {
    if (TEST) {
      model_dir = file.path("models/test/keras", model_name)
    } else {
      model_dir = file.path("models/prod/keras", model_name)
    }
    path_pipeline = file.path(model_dir, paste0(data_name, "_po.rds"))
    pipeline = readRDS(path_pipeline)
    model_path = file.path(model_dir, paste0(data_name, "_model.hdf5"))
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
        setcolorder(newdata, target_name)
        if (class(pipeline)[1] == "Graph") {
          newdata = pipeline$predict(as_task_classif(newdata, target = target_name))[[1]]$data()
        }
        yhat = model %>% predict(as.matrix(newdata[, -1L]))
        yhat = data.table::as.data.table(yhat)
        names(yhat) = levels(data[[target_name]])
        yhat
      },
      type = "prob"
    )
    pred$task = task
  } else {
    model_registry = loadRegistry("models/prod/registry", make.default = FALSE)
    model_job_params = unwrap(getJobPars(reg = model_registry))
    job_id = model_job_params[problem == data_name & algorithm == model_name]
     this_model = loadResult(id = job_id, reg = model_registry)
    if (is.list(this_model)) {
      pred = this_model[[id_x_interest]]
    } else {
        pred = Predictor$new(this_model, data = data, y = target_name, type = type)
    }
  }

  print(summary(pred$predict(data)))

  return(pred)
}


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

