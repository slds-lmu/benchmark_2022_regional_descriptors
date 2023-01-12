rm(list = ls())
set.seed(867853)
data.table::setDTthreads(1L)
#----
# 0) Load helper functions & libraries
#----
TEST = FALSE

# Setup
source("ird/libs_ird.R")
source("robustness/helper_irdmethods_wrapper_dataset.R")
source("ird/get_predictor_and_x_interest_pp.R")
source("helpers/get_predictor.R")
source("helpers/get_desired_range.R")

# if (TEST) {
#   source("ird/def_ird_test.R")
# } else {
  source("robustness/def_robustness_dataset.R")
# }

# Create registry
OVERWRITE = TRUE

# Create registry
if (file.exists(registry_dir)) {
  if (OVERWRITE) {
    unlink(registry_dir, recursive = TRUE)
    reg = makeExperimentRegistry(file.dir = registry_dir, packages = packages, seed = 20210814)
  } else {
    reg = loadRegistry(registry_dir, writeable = TRUE)
  }
} else {
  reg = makeExperimentRegistry(file.dir = registry_dir, packages = packages, seed = 20210814)

}

if (Sys.info()["sysname"] == "Windows") {
} else {
  reg$cluster.functions = makeClusterFunctionsMulticore(n_cores)
}


# Add problems
for (i in seq_along(data_list)) {
  addProblem(name = names(data_list)[[i]], data = data_list[[i]], seed = 457485)
}

# Add algos
addAlgorithm(name = "maire", fun = maire_wrapper)
addAlgorithm(name = "maxbox", fun = maxbox_wrapper)
addAlgorithm(name = "prim", fun = prim_wrapper)
addAlgorithm(name = "anchors", fun = anchors_wrapper)

# Add experiments
addExperiments(algo.designs = ades)
summarizeExperiments()
exp = unwrap(getJobPars())

# Run
submitJobs()
waitForJobs()
getStatus()

getErrorMessages()

