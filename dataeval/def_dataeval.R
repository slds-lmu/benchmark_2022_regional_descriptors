# DATA configurations
data_list = readRDS(file.path("data/data_storage/data_list.RDS"))
data_names = names(data_list)

# XINTEREST
x_interest_list = readRDS(file.path("data/data_storage/x_interest_list.RDS"))
id_x_interest = 1:5

# MODEL DIR
model_dir = "models/prod/registry"
model_names = c("ranger", "linear_model", "neural_network", "hyperbox")
