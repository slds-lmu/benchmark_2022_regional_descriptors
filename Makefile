get-data:
	R CMD BATCH data/get_data.R

train-models:
	R CMD BATCH models/train_models.R

generate-evaldata:
	R CMD BATCH dataeval/generate_dataeval.R

#resample:
#	R CMD BATCH models/resample.R

generate-irds:
	R CMD BATCH ird/find_ird.R
