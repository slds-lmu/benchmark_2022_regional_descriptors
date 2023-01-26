library(DBI)
library(magrittr)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(scales)
library(xtable)
source("evaluation/analysis_helper.R")
source("evaluation/analysis_helper_test.R")

#--- FIGURES ----
compare_methods(savepdf = TRUE, orientation = NULL,
  datastrategy = c("sampled", "traindata"), postprocessed = c(0, 1),
  quality_measures =  c("coverage_L_train", "coverage_L_sampled",
    "precision_train", "precision_sampled"))


quality = c("coverage_L_train", "coverage_L_sampled",
  "precision_train", "precision_sampled", "robustness_traindata", "robustness_sampled")

# no post-processing, dataset
compare_methods(savepdf = TRUE, orientation = "dataset", postprocessed = c(0),
  datastrategy = c("sampled", "traindata"),
  quality_measures =  quality)

# no post-processing, models
compare_methods(savepdf = TRUE, orientation = "model", postprocessed = c(0),
  datastrategy = c("sampled", "traindata"),
  quality_measures =  quality)

# post-processing, dataset
compare_methods(savepdf = TRUE, orientation = "dataset", postprocessed = c(1),
  datastrategy = c("sampled", "traindata"),
  quality_measures =  quality)

# post-processing, models
compare_methods(savepdf = TRUE, orientation = "model", postprocessed = c(1),
  datastrategy = c("sampled", "traindata"),
  quality_measures =  quality)


#--- TABLES ----
create_runtime_maximality_table(
  quality_measures = c("maximality_train", "maximality_sampled", "efficiency"))

statistical_analysis()


