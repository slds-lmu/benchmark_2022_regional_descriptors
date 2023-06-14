# function was copied from
# https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
permutations = function(n){
  if (n == 1){
    return(matrix(1))
  } else {
    sp = permutations(n-1)
    p = nrow(sp)
    A = matrix(nrow = n*p, ncol = n)
    for (i in 1:n) {
      A[(i - 1)*p + 1:p, ] = cbind(i, sp + (sp >= i))
    }
    return(A)
  }
}



get_connected_samples = function(predictor, x_interest, desired_range, fixed_features = NULL,
                                 num_sampled_points = 10L, convexset = FALSE, resolution = 50L,
                                 max_permutations = 100L, seed = 301122L) {
  checkmate::assertClass(predictor, classes = "Predictor")
  assert_data_frame(x_interest, nrows = 1L)
  assert_numeric(desired_range, len = 2, null.ok = FALSE)
  assert_names(names(x_interest), must.include = names(predictor$data$X))
  x_interest = setDT(x_interest)[, names(predictor$data$X), with = FALSE]
  if (!is.null(fixed_features)) {
    assert_names(fixed_features, subset.of = predictor$data$feature.names)
    dt = copy(predictor$data$get.x())[, (fixed_features) := x_interest[, fixed_features, with = FALSE]]
  } else {
    dt = copy(predictor$data$get.x())
  }
  checkmate::assertIntegerish(num_sampled_points, len = 1L, lower = 1L)
  checkmate::assertIntegerish(resolution, len = 1L, lower = 1L)

  # generate sampled data of size num_sampled_points
  param_set = make_param_set(dt = dt)
  factor_cols = names(which(sapply(predictor$data$X, is.factor)))
  set.seed(seed)
  sampleddata = SamplerUnif$new(param_set)$sample(n = num_sampled_points)$data
  sampleddata = param_set$trafo(sampleddata, predictor = predictor)
  inrange = predict_range(predictor, sampleddata, range = desired_range) == 1L
  sampleddata = sampleddata[(inrange),]
  sampleddata = unique(sampleddata)
  if (nrow(sampleddata) > 0) {
     assertTRUE(all(predict_range(predictor, sampleddata, range = desired_range) == 1L))
  }

  # combine training data with prediction in range and sampled data
  id_pos = predict_range(predictor, newdata = dt, range = desired_range) == 1
  dt$training = TRUE
  sampleddata$training = FALSE
  sampleddata = rbind(dt[id_pos, ], sampleddata)
  sampleddata = unique(sampleddata)

  # shoot rays from x_interest to all positive training/datapoints
  connected = get_path(x_interest = x_interest, sampleddata = sampleddata,
                       predictor = predictor, desired_range = desired_range, fixed_features = fixed_features,
                       num_sampled_points = num_sampled_points, resolution = resolution)

  setcolorder(connected, names(sampleddata))


  if (nrow(sampleddata) > nrow(connected) && !convexset) {
    remaining = fsetdiff(sampleddata, connected)
    # remaining = remaining[order(apply(gower.dist(remaining, justified), 1L, function(x) min(x))), ]

    for (row in sample(seq_len(nrow(remaining)), size = nrow(remaining))) {
      # train = remaining[row, training]
      # remain = copy(remaining[row,])
      connectid = nrow(get_path(x_interest = remaining[row,], sampleddata = connected, predictor = predictor,
                                desired_range = desired_range, fixed_features = fixed_features,
                                num_sampled_points = num_sampled_points, resolution = resolution)) > 0
      if (connectid) {
        connected = rbind(connected, remaining[row,])
      }
    }
  }
  training = connected$training
  connected = connected[, training := NULL]
  return(list(training = connected[training,], sampled = connected[!training,]))
}


get_path = function (x_interest, sampleddata, predictor, desired_range, fixed_features = NULL,
                     num_sampled_points = 10L, resolution = NULL) {

  # CATEGORICAL features
  # distinguish between unordered and ordered ones
  catfeat = setdiff(names(
    predictor$data$feature.types)[predictor$data$feature.types == "categorical"],
    fixed_features)
  ordid = as.logical(predictor$data$get.x()[, lapply(.SD, FUN = is.ordered), .SDcols = catfeat])
  ordfeat = catfeat[ordid]
  catfeat = catfeat[!ordid]
  numfeat = setdiff(names(predictor$data$feature.types)[predictor$data$feature.types == "numerical"],
                    fixed_features)

  if (length(catfeat) > 0) {

    # categorical features
    vec = data.table(sampleddata[, catfeat, with = FALSE] ==
                       x_interest[rep(seq_len(nrow(x_interest)), nrow(sampleddata)), catfeat, with = FALSE])
    catfeat = catfeat[!vec[, lapply(.SD, FUN = all), .SDcols = names(vec)]]

    # get possible permutations
    perm_list = matrix(catfeat[permutations(length(catfeat))], ncol = length(catfeat))
    len = nrow(perm_list)
    message(paste0("Nominal features: inspect ", len, " permutations"))

    if (len > 100) {
      message(paste0("Capped number of permutations at 100"))
      perm_list = perm_list[sample(seq_len(len), 100, replace = FALSE),]
    }

    # get possible path on categorical features
    connected_comb = data.table()
    cat = combs = unique(sampleddata[, catfeat, with = FALSE])

    # skip through each possible permutation/path
    for (row in seq_len(nrow(perm_list))) {
      perm = perm_list[row,]
      # check if path connected for different combinations in data
      for (k in seq_along(perm)) {
        comb = unique(combs[, perm[1:k], with = FALSE])
        x_interest_temp = x_interest[rep(seq_len(nrow(x_interest)), nrow(comb)), ]
        x_interest_temp[, (perm[1:k])] = comb
        id = predict_range(predictor = predictor, newdata = x_interest_temp, range = desired_range) == 1
        comb = x_interest_temp[id,(perm[1:k]), with = FALSE]
        if (nrow(comb) == 0) {
          break
        }
        combs = merge(combs, comb, by = perm[1:k], all = FALSE, sort = FALSE)
      }
      connected_comb = rbind(connected_comb, unique(combs))
      setcolorder(connected_comb, names(cat))
      # if not check next path
      combs = fsetdiff(cat, connected_comb)
      if (nrow(combs) == 0) {
        break
      }
    }
    sampleddata = merge(sampleddata, connected_comb, by = catfeat, all = FALSE, sort = FALSE)
  }

  if (length(ordfeat) > 0 & nrow(sampleddata) > 0) {

    check_ord_path = function(row, perm, x_interest) {
      if(isTRUE(all.equal(row[,(perm),with = FALSE], x_interest[,(perm),with = FALSE], check.attributes = FALSE))) return(TRUE)
      row_interest = copy(row)[, (perm) := x_interest[, perm, with = FALSE]]
      for (k in seq_along(perm)) {
        if (x_interest[[perm[k]]] == row[[perm[k]]]) next
        lev = levels(row[[perm[k]]])
        comb = lev[which(lev == row[[perm[k]]]):which(lev == x_interest[[perm[k]]])]
        row_temp = row_interest[rep(seq_len(nrow(row)), length(comb)), ]
        row_temp[, (perm[k]) := comb]
        if (any(predict_range(predictor = predictor, newdata = row_temp, range = desired_range) != 1)) {
          return(FALSE)
        } else {
          row_interest = row_interest[, (perm[[k]]) := row[[perm[[k]]]]]
        }
      }
      return(TRUE)
    }

    # categorical features
    vec = data.table(sampleddata[, ordfeat, with = FALSE] ==
                       x_interest[rep(seq_len(nrow(x_interest)), nrow(sampleddata)), ordfeat, with = FALSE])
    ordfeat = ordfeat[!vec[, lapply(.SD, FUN = all), .SDcols = names(vec)]]

    # get possible permutations
    perm_list = matrix(ordfeat[permutations(length(ordfeat))], ncol = length(ordfeat))

    len = nrow(perm_list)
    message(paste0("Ordinal features: inspect ", len, " permutations"))

    if (len > 100) {
      message(paste0("Capped number of permutations at 100"))
      perm_list = perm_list[sample(seq_len(len), 100, replace = FALSE),]
    }

    # get possible path on categorical features
    connected_comb = data.table()
    combs = unique(sampleddata[, c(ordfeat, catfeat), with = FALSE])
    combs = combs[, (numfeat) := x_interest[, numfeat, with = FALSE]]

    # skip through each possible permutation/path
    for (row in seq_len(nrow(perm_list))) {
      perm = perm_list[row,]
      connectedid = combs[, check_ord_path(.SD, perm = perm, x_interest = x_interest),
                          by = seq_len(nrow(combs)), with = TRUE]$V1
      connected_comb = rbind(connected_comb, combs[connectedid,])
      # if not check next path
      combs = combs[!connectedid,]
      if (nrow(combs) == 0) {
        break
      }
    }
    sampleddata = merge(sampleddata, connected_comb[, c(catfeat, ordfeat),with = FALSE],
                        by = c(catfeat, ordfeat), all = FALSE, sort = FALSE)
  }

  # NUMERIC features
  if (nrow(sampleddata) > 0 & length(numfeat) > 0) {
      message(paste0("Continuous features: ", length(numfeat), " with resolution ", resolution))
      # shoot linear rays
      shoot_ray = function(row, numfeat, resolution, x_interest) {
        vals = x_interest[rep(1, resolution), numfeat, with = FALSE] +
          t(c(as.matrix(row[, numfeat, with = FALSE] - x_interest[, numfeat, with = FALSE])) *
              t(matrix(rep(seq(0, 1, length.out = resolution), length(numfeat)), ncol = length(numfeat))))
        row = row[rep(1, resolution), ]
        row[, numfeat] = vals
        all(predict_range(predictor, newdata = row, range = desired_range) == 1)
      }

      # numeric features
      sampleddata = sampleddata[
        sampleddata[, shoot_ray(.SD, numfeat = numfeat, resolution = resolution, x_interest = x_interest),
                    by = seq_len(nrow(sampleddata)), with = TRUE]$V1,
      ]
    }
  return(sampleddata)
}
