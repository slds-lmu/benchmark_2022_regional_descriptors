packages = c("batchtools", "mlr3", "mlr3learners", "mlr3extralearners", "mlr3pipelines",
  "mlr3tuning", "mlr3keras", "rpart", "partykit")
new_packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0L) install.packages(new_packages)

sapply(packages, require, character.only = TRUE)
