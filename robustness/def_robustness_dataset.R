# DATA configurations
data_list = readRDS(file.path("data/data_storage/data_list.RDS"))

# configurations
models = c("ranger", "linear_model",  "neural_network", "hyperbox")

ades = list(
  maire = CJ(
    id_x_interest = 1:5,
    model_name = models,
    datastrategy = c("traindata", "sampled"),
    subsamples = 1:10
  ),
  maxbox = CJ(
    id_x_interest = 1:5,
    model_name = models,
    datastrategy = c("traindata", "sampled"),
    subsamples = 1:10
  ),
  prim = CJ(
    id_x_interest = 1:5,
    model_name = models,
    datastrategy = c("traindata", "sampled"),
    subsamples = 1:10
  ),
  anchors = CJ(
    id_x_interest = 1:5,
    model_name = models,
    datastrategy = c("traindata", "sampled"),
    subsamples = 1:10
  )
)

# BATCHTOOLS configurations
n_cores = 14L
registry_dir = "robustness/prod/registry_robustness_dataset"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))
