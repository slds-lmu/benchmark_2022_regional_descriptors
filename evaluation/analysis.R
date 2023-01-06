# library(DBI)
# library(magrittr)
# library(data.table)
# library(ggplot2)
# library(tidyverse)
# library(ggpubr)
# library(scales)
source("evaluation/analysis_helper.R")

# RQ 1
compare_methods(savepdf = TRUE)

# RQ 3
compare_methods(postprocessed = c(0, 1), savepdf = TRUE)

# RQ1 + RQ3
comparison_table()

