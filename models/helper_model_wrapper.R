ranger_wrapper = function(data, job, instance, ...) {

  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  y = data[[ncol(data)]]
  if (is.numeric(y) & length(unique(y)) > 3) {
   this_task = as_task_regr(data, target = names(data)[ncol(data)])
  } else {
    this_task = as_task_classif(data, target = names(data)[ncol(data)])
  }
  tc = readRDS(file.path("models/tuning_config.RDS"))[[this_task$task_type]]

  if (this_task$task_type == "classif") {
    # CLASSIFICATION
    if (job$prob.name %in% c("tic_tac_toe", "diabetes", "cmc")) {
      mod = po("scale") %>>%
        po("encode") %>>%
        po("smote") %>>%
        po(lrn("classif.ranger", predict_type = "prob"))
      mod$keep_results = TRUE

    } else {
      mod = lrn("classif.ranger", predict_type = "prob")
    }

    tune_ps = paradox::ParamSet$new(list(
      paradox::ParamDbl$new("classif.ranger.num.trees", lower = 1, upper = log(1000))
    ))
    tune_ps$trafo = function(x, param_set) {
      x$classif.ranger.num.trees = round(exp(x$classif.ranger.num.trees))
      x
    }

  } else {
    # REGRESSION
    mod = lrn("regr.ranger")
    tune_ps = paradox::ParamSet$new(list(
      paradox::ParamDbl$new("regr.ranger.num.trees", lower = 1, upper = log(1000))
    ))
    tune_ps$trafo = function(x, param_set) {
      x$regr.ranger.num.trees = round(exp(x$regr.ranger.num.trees))
      x
    }

  }
  mod = GraphLearner$new(mod)
  at = AutoTuner$new(mod, tc$resampling, tc$measure, tc$terminator, tc$tuner, tune_ps)
  at$train(this_task)

  print(summary(at$predict(this_task)$response))

  return(at)

}

lm_wrapper = function(data, job, instance, ...) {

  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  y = data[[ncol(data)]]
  if (is.numeric(y) & length(unique(y)) > 3) {
    this_task = as_task_regr(data, target = names(data)[ncol(data)])
  } else {
    this_task = as_task_classif(data, target = names(data)[ncol(data)])
  }

  if (this_task$task_type == "classif") {
    if (this_task$properties == "twoclass") {
      if (job$prob.name %in% c("tic_tac_toe", "diabetes")) {
        mod = po("scale") %>>%
          po("encode") %>>%
          po("smote") %>>%
          po(lrn("classif.log_reg", predict_type = "prob"))
      } else {
        mod = po(lrn("classif.log_reg", predict_type = "prob"))
      }
    } else if (this_task$properties == "multiclass") {
      mod = lrn("classif.multinom", predict_type = "prob")
    }
  } else if (this_task$task_type == "regr") {
    mod = lrn("regr.lm")
  }

  lr_learner = as_learner(mod)
  lr_learner$train(this_task)

  print(summary(lr_learner$predict(this_task)$response))

  lr_learner

}


nn_wrapper = function(data, job, instance, ...) {

  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  library(mlr3keras)
  reticulate::use_condaenv("mlr3keras")

  target_name = names(data)[ncol(data)]
  if (job$prob.name %in% c("tic_tac_toe", "diabetes", "cmc")) {
    pos = po("scale") %>>% po("encode") %>>% po("smote")
    data = pos$train(as_task_classif(data, target = target_name))[[1L]]$data()
  } else {
    pos = NULL
  }
  y = data[[target_name]]
  if (is.numeric(y) & length(unique(y)) > 3) {
    this_task = as_task_regr(data, target = target_name)
  } else {
    this_task = as_task_classif(data, target = target_name)
  }

  tc = readRDS(file.path("models/tuning_config.RDS"))[[this_task$task_type]]

  # Instantiate Learner
  lrn = LearnerClassifKerasFF$new()

  # Set Learner Hyperparams

  if (this_task$task_type == "classif") {
    # get_keras_model = function(units, learning_rate = 3*10^-4) {
    #   m = keras::keras_model_sequential()
    #   m = keras::layer_dense(m, units = units, activation = "relu")
    #   m = keras::layer_dense(m, units = 1L, activation = "sigmoid")
    #   keras::compile(m, optimizer = keras::optimizer_adam(learning_rate), loss = "categorical_crossentropy", metrics = "accuracy")
    #   m
    # }
    nn_learner = lrn("classif.kerasff", predict_type = "prob",
      epochs = 500L, callbacks = list(cb_es(monitor = "val_loss", patience = 5L)))
  } else {
    # get_keras_model = function(units, learning_rate = 3*10^-4) {
    #   m = keras::keras_model_sequential()
    #   m = keras::layer_dense(m, units = units, activation = "relu")
    #   m = keras::layer_dense(m, units = 1L, activation = "sigmoid")
    #   keras::compile(m, optimizer = keras::optimizer_adam(learning_rate), loss = "mean_squared_error", metrics = "mse")
    #   m
    # }
    nn_learner = lrn("regr.kerasff", epochs = 500L,
      callbacks = list(cb_es(monitor = "val_loss", patience = 5L)))
  }
  tune_ps = paradox::ParamSet$new(list(
    paradox::ParamInt$new("layer_units", lower = 1, upper = 20, tags = "train")
    # paradox::ParamInt$new("units", lower = 1, upper = 20, tags = "train"),
    # paradox::ParamDbl$new("learning_rate", lower = 10^-5, upper = 10^-1, tags = "train")
  ))
  # tune_ps$trafo = function(x, param_set) {
  #   x$model = get_keras_model(x$units, x$learning_rate)
  #   x$learning_rate = x$units = NULL
  #   return(x)
  # }

  at = AutoTuner$new(
    learner = nn_learner,
    resampling = tc$resampling,
    measure = tc$measure,
    search_space = tune_ps,
    terminator = tc$terminator,
    tuner = tc$tuner
  )
  at$train(this_task)
  rr = resample(this_task, at, tc$outer_resampling, store_models = TRUE)

  path = "models/prod/keras/neural_network"
  if (!dir.exists(dirname(path))) dir.create(dirname(path))
  if (!dir.exists(path)) dir.create(path)
  saveRDS(rr, file.path(path, paste0(job$prob.name, "_rr.rds")))
  saveRDS(pos, file.path(path, paste0(job$prob.name, "_po.rds")))
  at$learner$save(file.path(path, paste0(job$prob.name, "_model.hdf5")))

  print(summary(at$predict(this_task)$response))

  return(at)

}


hb_wrapper = function(data, job, instance, ...) {

  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)


  target_name = names(data)[ncol(data)]
  y = data[[target_name]]
  tree = rpart(as.formula(paste0(target_name, "~ .")), data = data)
  mod = as.party(tree)

  x_interests = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]]
  predictor_list = list()

  for (i in seq_len(nrow(x_interests))) {
    x_interest = x_interests[i,]
    attr(mod, "x_interest") = x_interest
    predictor_list[[i]] = Predictor$new(model = mod, data = data, y = target_name,
      predict.function = function(model, newdata) {
        node_interest = predict(model, newdata = attr(model, "x_interest"), type = "node")
        prediction = ifelse(predict(model, newdata = newdata, type = "node") == node_interest, 1, 0)
      })
  }

  return(predictor_list)
}


