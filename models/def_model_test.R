# TUNING configurations
tuning_config <- list(
  resampling = rsmp("cv", folds = 2L),
  outer_resampling = rsmp("cv", folds = 2L),
  measure = msr("classif.ce"),
  tuner = tnr("grid_search", resolution = 1L),
  terminator = trm("evals", n_evals = 1L)
)
saveRDS(tuning_config, file.path("models/tuning_config.RDS"))

# DATA configurations
data_list <- readRDS(file.path("data/data_storage_test/data_list.RDS"))

# ALGO configurations
algos <- c("ranger" = ranger_wrapper, "logistic_regression" = lr_wrapper)

# BATCHTOOLS configurations
n_cores <- 4L
registry_dir <- "models/test/registry_test"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))
