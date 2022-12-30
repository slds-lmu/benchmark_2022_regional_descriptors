library(mlr3)
library(batchtools)
library(mlr3tuning)

model_registry = loadRegistry("models/prod/registry", make.default = FALSE)
model_job_params = unwrap(getJobPars(reg = model_registry))  
data_list <- readRDS("data/data_storage/data_list.RDS")
if (!dir.exists("models/prod/resampling")) dir.create("models/prod/resampling")

for (i in model_job_params$job.id) {
  print(i)
  this_job <- model_job_params[job.id == i]
  if (this_job$algorithm %in% c("neural_network")) {
    rr <- readRDS(file.path("models/prod/keras", this_job$algorithm, paste0(this_job$problem, "_rr.rds")))
    save_dir <- file.path("models/prod/resampling", this_job$problem)
    if (!dir.exists(save_dir)) dir.create(save_dir)
    saveRDS(rr, file.path(save_dir, paste0(this_job$algorithm, "_rr.rds")))
  } else {
    this_data <- data_list[[this_job$problem]]
    this_task <- as_task_classif(this_data, names(this_data)[ncol(this_data)])
    this_at <- loadResult(i, model_registry)
    rr <- resample(this_task, this_at, rsmp("cv", folds = 5L), store_models = TRUE)
    save_dir <- file.path("models/prod/resampling", this_job$problem)
    if (!dir.exists(save_dir)) dir.create(save_dir)
    saveRDS(rr, file.path(save_dir, paste0(this_job$algorithm, "_rr.rds")))
  }
}














