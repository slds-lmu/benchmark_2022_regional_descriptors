# DATA configurations
data_list = readRDS(file.path("data/data_storage/data_list.RDS"))
data_names = names(data_list)

# XINTEREST
x_interest_list = readRDS(file.path("data/data_storage/x_interest_list.RDS"))
id_x_interest = 1:5

# MODEL DIR
model_dir = "models/prod/registry"
model_names = c("ranger", "hyperbox")

folder_dir <- "dataeval/prod"
if (!dir.exists(dirname(folder_dir))) dir.create(folder_dir)
if (!dir.exists(dirname(folder_dir))) dir.create(file.path(folder_dir, "largest_box"))
if (!dir.exists(dirname(folder_dir))) dir.create(file.path(folder_dir, "data_inbox_train"))
if (!dir.exists(dirname(folder_dir))) dir.create(file.path(folder_dir, "data_inbox_sampled"))
if (!dir.exists(dirname(folder_dir))) dir.create(file.path(folder_dir, "levelset"))
