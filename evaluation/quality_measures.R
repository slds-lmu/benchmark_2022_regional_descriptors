evaluate_quality = function(regdesc, data_set_name, model_name, id_x_interest, datastrategy) {

  pred = regdesc$.__enclos_env__$private$predictor
  data = pred$data$get.xy()

  if (model_name == "neural_network") {
    pred = get_predictor(data = data,
      model_name = model_name, data_name = data_set_name, id_x_interest = id_x_interest)
    pred$class = regdesc$class
  }
  ## locality
  locality = identify_in_box(regdesc$box, data = regdesc$x_interest)

  ## coverage and precision

  # training data
  train_in_box = identify_in_box(regdesc$box, data = pred$data$get.x())
  coverage_train = mean(train_in_box)

  # newly sampled data
  ps = regdesc$.__enclos_env__$private$param_set
  set.seed(1234L)
  sampdata = SamplerUnif$new(ps)$sample(n = 1000)$data
  sampdata = ps$trafo(x = sampdata, predictor = pred)
  sampled_in_box = identify_in_box(regdesc$box, data = sampdata)
  coverage_sampled = mean(sampled_in_box)

  # levelset
  levelset_dir = file.path("dataeval/prod/levelset", paste(data_set_name, model_name, id_x_interest, sep = "_"))
  levelset = readRDS(paste0(levelset_dir, ".rds"))
  # levelset training
  coverage_levelset_train = mean(identify_in_box(regdesc$box, data = levelset$training))
  # levelset sampled
  coverage_levelset_sampled = mean(identify_in_box(regdesc$box, data = levelset$sampled))

  ## precision
  # precision train
  if (any(train_in_box)) {
    precision_train = sum(predict_range(pred, newdata = pred$data$X[(train_in_box),],
      range = regdesc$desired_range))/sum(train_in_box)
  } else {
    precision_train = NA
  }

  # precision sampled
  if (any(sampled_in_box)) {
    precision_sampled = sum(predict_range(pred, newdata = sampdata[(sampled_in_box),], range = regdesc$desired_range))/sum(sampled_in_box)
  } else {
    precision_sampled = NA
  }

  ## maximality
  # training data
  maximality_train = assess_maximality(regdesc, pred = pred, data = data)
  # sampled data
  maximality_sampled = assess_maximality(regdesc, pred = pred, data = sampdata)

  ## efficiency
  #<FIXME:> replace by true call!
  nrow_obsdata = nrow(readRDS(file.path(paste0("dataeval/prod/data_inbox_", datastrategy),
    paste0(paste(data_set_name, model_name, id_x_interest, sep = "_"), ".rds"))))
  ### get data
  if (regdesc$method %in% c("Maire", "Prim", "MaxBox")) {
    ### full dataset
    efficiency = nrow_obsdata
  } else if (regdesc$method == "Anchor") {
    efficiency = nrow_obsdata * sample(7:15, 1)
  } else if (regdesc$method == "PostProcessing") {
    efficiency =  nrow_obsdata * sample(20:30, 1)
  }

  ## robustness
  # <FIXME>: Add!

  return(data.table(locality = locality, coverage_train = coverage_train,
    coverage_sampled = coverage_sampled, coverage_levelset_train = coverage_levelset_train,
    coverage_levelset_sampled = coverage_levelset_sampled,
    precision_train = precision_train, precision_sampled = precision_sampled,
    maximality_train = maximality_train, maximality_sampled = maximality_sampled,
    efficiency = efficiency))

}


assess_maximality = function(regdesc, pred, data) {

  feat_nams = pred$data$feature.names

  for (j in feat_nams) {

    boxsubset = regdesc$box$clone()$subset(setdiff(feat_nams, j))
    inbox = identify_in_box(boxsubset, data)
    boxdata = copy(data)[(inbox),]

    if (pred$data$feature.types[[j]] == "numerical") {
      boxdatalower = boxdata[which(boxdata[[j]] < regdesc$box$lower[[j]]), ]
      if (nrow(boxdatalower) > 0) {
        setorderv(boxdatalower, j, order = -1)
        lower = boxdatalower[1,j, with = FALSE][[1]]
        boxdatalower = boxdatalower[boxdatalower[[j]] == lower,]
        if (all(predict_range(pred, newdata = boxdatalower, range = regdesc$desired_range) == 1)) {
          return(FALSE)
        }
      }

      boxdataupper = boxdata[which(boxdata[[j]] > regdesc$box$upper[[j]]), ]
      if (nrow(boxdataupper) > 0) {
        setorderv(boxdataupper, j)
        upper = boxdataupper[1,j, with = FALSE][[1]]
        boxdataupper = boxdataupper[boxdataupper[[j]] == upper,]
        if (all(predict_range(pred, newdata = boxdataupper, range = regdesc$desired_range) == 1)) {
          return(FALSE)
        }
      }
    } else {
      for (cat in setdiff(unique(boxdata[[j]]), regdesc$box$params[[j]]$levels)) {
        temp = boxdata[boxdata[[j]] == cat, ]
        if (all(predict_range(pred, newdata = temp, range = regdesc$desired_range) == 1)) {
          return(FALSE)
        }
      }
    }
  }
  return(TRUE)
}

