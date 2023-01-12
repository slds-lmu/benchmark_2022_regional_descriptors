rm(list = ls())
set.seed(867853)
data.table::setDTthreads(1L)
#----
# 0) Load helper functions & libraries
#----
# Setup
source("dataeval/libs_dataeval.R")
source("dataeval/def_dataeval.R")
source("helpers/get_predictor.R")
source("helpers/get_desired_range.R")

#----
# 1) Get largest box, newly sampled data and local level set
#----
gridspace = data.table(expand.grid(x_interest = id_x_interest, model = model_names, data = data_names))
# gridspace = gridspace[which(gridspace$model == "neural_network" & gridspace$data == "plasma_retinol"), ]

get_closest_largest = function(data, x_interest, num) {
  data = fsetdiff(data, x_interest)
  if (nrow(data) >= num) {
    nearest = order(gower::gower_dist(x_interest, data))
    data = data[nearest,][1:num]
  }
  return(data)
}

extra_info_list = apply(gridspace, MARGIN = 1, function(row) {
  print(row)
  id_x_interest = as.integer(row[["x_interest"]])
  data_name = row[["data"]]
  model_name = row[["model"]]
  filename = paste(data_name, model_name, id_x_interest, sep = "_")

  data = data.table(data_list[[data_name]])
  x_interest = x_interest_list[[data_name]][id_x_interest,]
  pred = get_predictor(data = data, data_name = data_name, model_name = model_name, id_x_interest = id_x_interest)

  data = data.table(data_list[[data_name]])
  x_interest = x_interest[, pred$data$feature.names, with = FALSE]
  data = data[, pred$data$feature.names, with = FALSE]

  ps = make_param_set(rbind(data, x_interest))

  desired = get_desired_range(data_name, pred, x_interest)
  desired_class = desired[[1]]
  desired_range = desired[[2]]

  pred$class = desired_class

  # GET LARGEST BOX
  largest_box = get_max_box(x_interest = x_interest, fixed_features = NULL,
    predictor = pred, param_set = ps, desired_range = desired_range)

  # GET TRAINDATA IN BOX
  inbox = identify_in_box(largest_box, data)
  inboxdata = data[(inbox),]

  # SAMPLE NEW DATA
  nosamp = 2*nrow(data)
  sampdata = SamplerUnif$new(largest_box)$sample(n = nosamp)$data
  sampdata = largest_box$trafo(x = sampdata, predictor = pred)

  # GET SUBSAMPLES OF DATA
  subsamp_traindata = lapply(1:10, FUN = function(i) {
    subsamp = inboxdata[sample(seq_len(nrow(inboxdata)), size = round(2/3*nrow(inboxdata))),]
    saveRDS(subsamp, file = file.path(folder_dir, "subsample_traindata",
                                      paste0(filename, "_", i, "_", ".rds")))
  })
  subsamp_sampled = lapply(1:10, FUN = function(i) {
    subsamp = sampdata[sample(seq_len(nrow(sampdata)), size = round(2/3*nrow(sampdata))),]
    saveRDS(subsamp, file = file.path(folder_dir, "subsample_sampled",
                                      paste0(filename, "_", i, "_", ".rds")))
  })

  # GET LOCAL LEVEL SET
  levelset = get_connected_samples(predictor = pred, x_interest = x_interest,
    desired_range = desired_range, num_sampled_points = 1000L, convexset = TRUE, resolution = 50L)

  # GET LEVEL SET IN LARGEST BOX
  levelsetlargest = levelset$training[identify_in_box(largest_box, levelset$training),]
  levelsetlargest = get_closest_largest(levelsetlargest, x_interest, 5L)
  missing = 5 - nrow(levelsetlargest) - 5
  if (missing > 0) {
    samplelargest = levelset$sampled[identify_in_box(largest_box, levelset$sampled),]
    samplelargest = get_closest_largest(samplelargest, x_interest, missing)
    levelsetlargest = rbind(levelsetlargest, samplelargest)
  }

  filenamerds = paste0(filename, ".rds")
  saveRDS(largest_box, file = file.path(folder_dir, "largest_box", filenamerds))
  saveRDS(inboxdata, file = file.path(folder_dir, "data_inbox_traindata", filenamerds))
  saveRDS(sampdata, file = file.path(folder_dir, "data_inbox_sampled", filenamerds))
  saveRDS(levelset, file = file.path(folder_dir, "levelset", filenamerds))
  saveRDS(levelsetlargest, file = file.path(folder_dir, "levelsetlargest", filenamerds))
})


lookup = expand.grid(model = model_names, data = data_names)
