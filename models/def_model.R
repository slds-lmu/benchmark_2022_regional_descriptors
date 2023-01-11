folds = 5L # 5L
n_evals = 15L # 15L

tuning_config = list("classif" = list(
      resampling = rsmp("cv", folds = folds),
      outer_resampling = rsmp("cv", folds = folds),
      measure = msr("classif.ce"),
      tuner = tnr("random_search"),
      terminator = trm("evals", n_evals = n_evals)
    ),
  "regr" = list(
      resampling = rsmp("cv", folds = folds),
      outer_resampling = rsmp("cv", folds = folds),
      measure = msr("regr.mse"),
      tuner = tnr("random_search"),
      terminator = trm("evals", n_evals = n_evals)
    )
)

saveRDS(tuning_config, file.path("models/tuning_config.RDS"))


# DATA configurations
data_list <- readRDS(file.path("data/data_storage/data_list.RDS"))

# ALGO configurations
algos <- c(
  "ranger" = ranger_wrapper,
  "linear_model" = lm_wrapper,
  "neural_network" = nn_wrapper,
  "hyperbox" = hb_wrapper
)

# BATCHTOOLS configurations
n_cores <- 15L
registry_dir <- "models/prod/registry"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))
