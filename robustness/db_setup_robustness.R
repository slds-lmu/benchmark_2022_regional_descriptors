source("evaluation/libs_evaluate.R")
source("robustness/compute_overlap_x.R")
source("helpers/get_predictor.R")

## --------------
reg_dir = "ird/prod/registry_new/"
reg = loadRegistry(reg_dir, make.default = FALSE)
regrobust = loadRegistry("robustness/prod/registry_robustness_x/", make.default = FALSE)
data_list = readRDS("data/data_storage/data_list.RDS")
add_results_to_db_robustness("diabetes", reg)
add_results_to_db_robustness("tic_tac_toe", reg)
add_results_to_db_robustness("cmc", reg)
add_results_to_db_robustness("vehicle", reg)
add_results_to_db_robustness("no2", reg)
add_results_to_db_robustness("plasma_retinol", reg)
