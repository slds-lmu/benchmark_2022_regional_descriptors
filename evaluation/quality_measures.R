evaluate_quality = function(regdesc) {
  pred = regdesc$.__enclos_env__$private$predictor

  ## locality
  locality = identify_in_box(regdesc$box, data = regdesc$x_interest)

  ## coverage and precision

  # training data
  train_in_box = identify_in_box(regdesc$box, data = pred$data$get.x())
  coverage_train = mean(train_in_box)

  # newly sampled data
  ps = regdesc$.__enclos_env__$private$param_set
  set.seed(1234L)
  sampdata = SamplerUnif$new(ps)$sample(n = pred$data$n.rows*2)$data
  sampdata = ps$trafo(x = sampdata, predictor = pred)
  sampled_in_box = identify_in_box(regdesc$box, data = sampdata)
  coverage_sampled = mean(sampled_in_box)

  # <FIXME:> ADD levelset evaluation of coverage
  # levelset training
  ## levelset = readRDS(...)

  # levelset sampled

  ## precision
  # precision train
  if (any(train_in_box)) {
    precision_train = sum(predict_range(pred, newdata = pred$data$X[(train_in_box),], range = regdesc$desired_range))/sum(train_in_box)
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


  ## robustness

  return(data.table(locality = locality, coverage_train = coverage_train,
    coverage_sampled = coverage_sampled, precision_train = precision_train,
    precision_sampled = precision_sampled))

}
