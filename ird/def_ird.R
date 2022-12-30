# DATA configurations
data_list = readRDS(file.path("data/data_storage/data_list.RDS"))

# configurations
models = c("ranger", "linear_model", "neural_network", "hyperbox")

ades = list(
  maire = CJ(
    id_x_interest = 1:5,
    model_name = models
  ),
  maxbox = CJ(
    id_x_interest = 1:5,
    model_name = models
  ),
  prim = CJ(
    id_x_interest = 1:5,
    model_name = models
  )
)

# BATCHTOOLS configurations
n_cores = 14L
registry_dir = "ird/prod/registry_test"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))
