Sys.setenv(RETICULATE_PYTHON = "/home/susanne/.local/share/r-miniconda/envs/mlr3keras/bin/python")
reticulate::use_condaenv("mlr3keras")
# Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2) #switch off messages
packages = c("DBI", "dplyr", "tidyr", "batchtools", "keras", "data.table", "ggplot2", "xtable", # evaluation
  "devtools", "batchtools", "iml", "mlr3oml", "mlr3", "data.table", "R6", "keras", "mlr3keras") # ird
new_packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0L) install.packages(new_packages)

sapply(packages, require, character.only = TRUE)

load_all("../irgn")

