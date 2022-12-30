packages = "mlr3oml"
new_packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0L) install.packages(new_packages)

sapply(packages, require, character.only = TRUE)