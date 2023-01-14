source("evaluation/libs_evaluate.R")
source("evaluation/helper_db_setup.R")
source("evaluation/quality_measures.R")
source("helpers/get_predictor.R")

## --------------
reg_dir = "ird/prod/registry/"
reg = loadRegistry(reg_dir, make.default = FALSE)
add_results_to_db("diabetes", reg)
add_results_to_db("tic_tac_toe", reg)
add_results_to_db("cmc", reg)
add_results_to_db("vehicle", reg)
add_results_to_db("no2", reg)
add_results_to_db("plasma_retinol", reg)
