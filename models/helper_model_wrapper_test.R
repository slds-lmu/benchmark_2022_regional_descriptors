ranger_wrapper = function(data, job, instance, ...) {
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  tc = readRDS(file.path("models/tuning_config.RDS"))
  this_task = as_task_classif(data, target = names(data)[ncol(data)])
  
  if (job$prob.name %in% c("diabetis", "tic_tac_toe", "credit_g")) {
    mod = po("classbalancing", adjust = "major", reference = "major", shuffle = FALSE, ratio = 1 / 2) %>>%
      po("scale") %>>%
      po("encode") %>>%
      po(lrn("classif.ranger", predict_type = "prob"))
  } else {
    mod = po("scale") %>>%
      po("encode") %>>%
      po(lrn("classif.ranger", predict_type = "prob"))
  }
  
  mod$keep_results = TRUE
  tune_ps = paradox::ParamSet$new(list(
    paradox::ParamDbl$new("classif.ranger.num.trees", lower = 1, upper = log(1000))
  ))
  tune_ps$trafo = function(x, param_set) {
    x$classif.ranger.num.trees = round(exp(x$classif.ranger.num.trees))
    x
  }
  mod = GraphLearner$new(mod)
  at = AutoTuner$new(mod, tc$resampling, tc$measure, tc$terminator, tc$tuner, tune_ps)
  at$train(this_task)
  at
}


lr_wrapper = function(data, job, instance, ...) {
  library(mlr3)
  library(mlr3learners)
  target_name = names(data)[ncol(data)] 
  this_task = as_task_classif(data, target = target_name)
  lr_learner = lrn("classif.log_reg")
  lr_learner$train(this_task)
  lr_learner
}