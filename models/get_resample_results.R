library(mlr3)
library(mlr3tuning)

datal = c("diabetes", "tic_tac_toe", "cmc", "vehicle", "no2", "plasma_retinol")
modell = c("ranger", "linear_model", "neural_network", "hyperbox")

resagg = matrix(nrow = length(datal), ncol = length(modell))
rownames(resagg) = datal
colnames(resagg) = modell

for (datanam in datal) {
  for (mod in modell) {
    if (mod == "hyperbox") next
  print(datanam)
  res = readRDS(paste0(file.path("models/prod/resampling", datanam), "/", mod, "_rr.rds"))
  resagg[datanam, mod] = res$aggregate()
  }
}
saveRDS(resagg, file = "models/prod/resampling/aggregate_results.rds")
print(xtable(resagg, digits = 3),include.rownames = FALSE)
