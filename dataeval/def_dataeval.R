# DATA configurations
data_list = readRDS(file.path("data/data_storage/data_list.RDS"))
data_names = names(data_list)

# XINTEREST
x_interest_list = readRDS(file.path("data/data_storage/x_interest_list.RDS"))
id_x_interest = 1:5

# MODEL DIR
model_dir = "models/prod/registry"
model_names = c("ranger", "linear_model",  "neural_network", "hyperbox")

folder_dir <- "dataeval/prod"
dir.create(folder_dir, showWarnings = FALSE)
dir.create(file.path(folder_dir, "largest_box"), showWarnings = FALSE)
dir.create(file.path(folder_dir, "data_inbox_traindata"), showWarnings = FALSE)
dir.create(file.path(folder_dir, "data_inbox_sampled"), showWarnings = FALSE)
dir.create(file.path(folder_dir, "levelset"), showWarnings = FALSE)
# dir.create(file.path(folder_dir, "levelsetlargest"), showWarnings = FALSE)
# dir.create(file.path(folder_dir, "subsample_sampled"), showWarnings = FALSE)
# dir.create(file.path(folder_dir, "subsample_traindata"), showWarnings = FALSE)
