# library(magrittr)
# library(ggplot2)
# library(tidyverse)
library(DBI)
library(data.table)

# source("evaluation/libs_evaluate.R")
source("evaluation/helper_evaluate.R")

# Run db_setup.R first
add_evals_to_db("diabetes")
add_evals_to_db("tic_tac_toe")
add_evals_to_db("cmc")
add_evals_to_db("vehicle")
add_evals_to_db("no2")
add_evals_to_db("plasma_retinol")



