rm(list = ls())
set.seed(867853)
#----
# 0) Load helper functions & libraries
#----
source("data/def_data.R")
cat("Loaded datasets:", names(data_ids))
source("data/libs_data.R")


#----
# 1) Load data from OpenML, sample and extract x_interests and save data and x_interests as lists in rds files
#----
data_list = vector("list", length(data_ids))
x_interest_list = vector("list", length(data_ids))
for (i in seq_along(data_ids)) {
  data_id = data_ids[i]
  oml_data = OMLData$new(data_id)
  factor_cols = names(oml_data$data)[sapply(oml_data$data, is.factor)]
  oml_data$data[, (factor_cols) :=  lapply(.SD, droplevels), .SDcols = factor_cols]
  idx_x_interest = sample.int(nrow(oml_data$data), size = 5L)
  x_interest_list[[i]] = oml_data$data[idx_x_interest, ]
  data_list[[i]] = oml_data$data
}
names(data_list) = names(data_ids)
names(x_interest_list) = names(data_ids)

if (!dir.exists(dir_name_data_storage)) {
  dir.create(dir_name_data_storage)
}

saveRDS(data_list, file.path(dir_name_data_storage, "data_list.RDS"))
saveRDS(x_interest_list, file.path(dir_name_data_storage, "x_interest_list.RDS"))

