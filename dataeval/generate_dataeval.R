rm(list = ls())
set.seed(867853)
data.table::setDTthreads(1L)
#----
# 0) Load helper functions & libraries
#----
# Setup
source("dataeval/libs_dataeval.R")
source("dataeval/def_dataeval.R")
source("dataeval/get_predictor.R")

#----
# 1) Get largest box, newly sampled data and local level set
#----
gridspace = data.table(expand.grid(x_interest = id_x_interest, model = model_names, data = data_names))

extra_info_list = apply(gridspace, MARGIN = 1, function(row) {
  browser(i)
  id_x_interest = as.integer(row[["x_interest"]])
  data_name = row[["data"]]
  model_name = row[["model"]]
  data = data_list[[data_name]]
  x_interest = x_interest_list[[data_name]][id_x_interest,]
  pred = get_predictor(data_name = data_name, model_name = model_name, id_x_interest = id_x_interest)

  x_interest = x_interest[, pred$data$feature.names, with = FALSE]
  data = data[, pred$data$feature.names]

  ps = make_param_set(rbind(data, x_interest))

  desired_range = get_desired_range(data_name, pred, x_interest)

  pred$class = desired_range[[1]]

  # GET LARGEST BOX
  largest_box = get_max_box(x_interest = x_interest, fixed_features = NULL,
    predictor = pred, param_set = ps, desired_range = desired_range[[2]])

  # GET TRAINDATA IN BOX
  inbox = identify_in_box(largest_box, data)
  inboxdata = data[(inbox),]

  # SAMPLE NEW DATA
  nosamp = sum(inbox)*2
  sampdata = SamplerUnif$new(largest_box)$sample(n = nosamp)$data

  factor_cols = names(which(sapply(pred$data$X, is.factor)))
  for (factor_col in factor_cols) {
    fact_col_pred = pred$data$X[[factor_col]]
    value =  factor(sampdata[[factor_col]], levels = levels(fact_col_pred), ordered = is.ordered(fact_col_pred))
    set(sampdata, j = factor_col, value = value)
  }

  # GET LOCAL LEVEL SET
  levelset = get_connected_samples(predictor = pred, x_interest = x_interest,
    desired_range = desired_range[[2]], num_sampled_points = 100L, convexset = FALSE, resolution = 100L)

  return(largestB = largest_box, inboxtrain = inboxdata, sampdata = sampdata, levelset = levelset)

})
