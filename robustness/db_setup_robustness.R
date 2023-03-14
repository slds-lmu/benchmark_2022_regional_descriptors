source("evaluation/libs_evaluate.R")
source("robustness/helper_db_setup_robustness.R")
source("helpers/get_predictor.R")

## --------------
reg_dir = "ird/prod/registry_new/"
reg_dir_robust = "robustness/prod/registry_robustness_x_anchors/"
reg = loadRegistry(reg_dir, make.default = FALSE)
regrobust = loadRegistry(reg_dir_robust, make.default = FALSE) # TODO: adapt!!
# regrobustanchors = loadRegistry("robustness/prod/registry_robustness_x_anchors/", make.default = FALSE)
db_name = "robustness/db_robustness_x_anchors.db"
data_list = readRDS("data/data_storage/data_list.RDS")
add_results_to_db_robustness("diabetes", reg)
add_results_to_db_robustness("tic_tac_toe", reg)
add_results_to_db_robustness("cmc", reg)
add_results_to_db_robustness("vehicle", reg)
add_results_to_db_robustness("no2", reg)
add_results_to_db_robustness("plasma_retinol", reg)
