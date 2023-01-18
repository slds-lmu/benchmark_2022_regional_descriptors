library(DBI)
library(magrittr)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(scales)
source("evaluation/analysis_helper.R")

# RQ 1
compare_methods(savepdf = TRUE, orientation = NULL, datastrategy = c("sampled", "traindata"), postprocessed = c(0, 1))
# compare_methods(savepdf = FALSE, orientation = NULL, postprocessed = c(0),
#                  datastrategy = c("sampled", "traindata"))
# compare_methods(savepdf = TRUE, orientation = NULL, postprocessed = c(1),
#                 datastrategy = c("sampled", "traindata"))
# RQ 3
compare_methods(postprocessed = c(0, 1), orientation = "dataset", savepdf = FALSE)

# RQ1 + RQ3
create_runtime_maximality_table()

