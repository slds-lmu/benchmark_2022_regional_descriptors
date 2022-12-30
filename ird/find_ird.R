rm(list = ls())
set.seed(867853)
data.table::setDTthreads(1L)
#----
# 0) Load helper functions & libraries
#----
TEST = FALSE

# Setup
source("ird/libs_ird.R")
source("ird/helper_irdmethods_wrapper.R")
source("ird/get_predictor_and_x_interest_pp.R")

# if (TEST) {
#   source("ird/def_ird_test.R")
# } else {
  source("ird/def_ird.R")
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

# Add experiments
addExperiments(algo.designs = ades)
summarizeExperiments()
exp = unwrap(getJobPars())

# exp[!duplicated(exp[c("problem", "algorithm", "model_name")])]

# jobids = exp[id_x_interest == 1 & model_name != "hyperbox", "job.id", with = FALSE][[1]]

exp[model_name == "hyperbox" & problem == "credit_g",]

testJob(id = 1L)

testJob(id = jobids[[1]])

for (id in jobids) {
  testJob(id = id)
}


# Run
submitJobs()
waitForJobs()
getStatus()

getErrorMessages()

