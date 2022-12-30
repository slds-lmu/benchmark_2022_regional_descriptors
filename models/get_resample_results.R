library(mlr3)
library(mlr3tuning)

datal = c("credit_g", "diabetis", "tic_tac_toe", "bank8FM", "hill_valley", "run_or_walk_info")
modell = c("logistic_regression","neural_network", "ranger", "svm", "xgboost")

resagg = matrix(nrow = length(datal), ncol = length(modell))
rownames(resagg) = datal
colnames(resagg) = modell

for (datanam in datal) {
  for (mod in modell) {
  print(datanam)
  res = readRDS(paste0(file.path("models/prod/resampling", datanam), "/", mod, "_rr.rds"))
  resagg[datanam, mod] = res$aggregate(measures = list(msr("classif.acc"))) 
  }
}
saveRDS(resagg, file = "models/prod/resampling/aggregate_results.rds")