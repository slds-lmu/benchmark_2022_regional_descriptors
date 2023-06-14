library("randomForest")
library("partykit")
library("rpart")

get_box_regr_mtcars = function() {
  file_path = "test_files/boxmodel_regr_mtcars.RDS"
  if (!file.exists(file_path)) {
    if (!dir.exists("test_files")) {
      dir.create("test_files")
    }
    tree = rpart::rpart(mpg ~ ., data = mtcars)
    mod = as.party(tree)
    x_interest = mtcars[1L,]
    attr(mod, "x_interest") = x_interest
    pred = Predictor$new(model = mod, data = mtcars, predict.function = function(model, newdata) {
      node_interest = predict(model, newdata = attr(model, "x_interest"), type = "node")
      prediction = ifelse(predict(model, newdata = newdata, type = "node") == node_interest, 1, 0)
    })
    saveRDS(pred, file = file_path)
    saveRDS(get_box(pred), "test_files/box_regr_mtcars.RDS")
  }
  list(model = readRDS(file_path), box = readRDS("test_files/box_regr_mtcars.RDS"))
}


# tree = rpart(risk ~ ., data = credit)
# mod = as.party(tree)
# x_interest = credit[1L,]
# attr(mod, "x_interest") = x_interest
# pred = Predictor$new(model = mod, data = credit, predict.function = function(model, newdata) {
#   node_interest = predict(model, newdata = attr(model, "x_interest"), type = "node")
#   prediction = ifelse(predict(model, newdata = newdata, type = "node") == node_interest, 1, 0)
# })

get_box = function(pred) {
  node = node_party(pred$model)
  i = predict(pred$model,
    newdata = attr(pred$model, "x_interest"), type = "node")
  dat <- data_party(pred$model, i)

  if (!is.null(pred$model$fitted)) {
    findx <- which("(fitted)" == names(dat))[1]
    fit <- dat[, findx:ncol(dat), drop = FALSE]
    dat <- dat[, -(findx:ncol(dat)), drop = FALSE]
    if (ncol(dat) == 0)
      dat <- pred$model$data
  }
  else {
    fit <- NULL
    dat <- pred$model$data
  }

  current_box = irgn:::make_param_set(pred$data$get.x())
  terminated = FALSE
  while (!terminated) {
    if (id_node(node) == i) {
      break
    }
    kid <- sapply(kids_node(node), id_node)
    whichkid <- max(which(kid <= i))
    split <- split_node(node)
    ivar <- varid_split(split)
    svar <- names(dat)[ivar]
    index <- index_split(split)
    if (is.factor(dat[, svar])) {
      if (is.null(index))
        index <- ((1:nlevels(dat[, svar])) > breaks_split(split)) +
          1
      slevels <- levels(dat[, svar])[index == whichkid]
      slevels <- slevels[!is.na(slevels)]
      current_box = update_box(current_box = current_box,
         j = svar, val = slevels, complement = FALSE)
    }
    else {
      if (is.null(index))
        index <- 1:length(kid)
      breaks <- cbind(c(-Inf, breaks_split(split)), c(breaks_split(split),
        Inf))
      sbreak <- breaks[index == whichkid, ]
      right <- right_split(split)
      srule <- c()
      if (is.finite(sbreak[1]))
        current_box = update_box(current_box = current_box,
          j = svar, lower = sbreak[1], complement = FALSE)
      if (is.finite(sbreak[2]))
        srule <- c(srule, paste(svar, ifelse(right,
          "<=", "<"), sbreak[2]))
      current_box = update_box(current_box = current_box,
        j = svar, upper = sbreak[2], complement = FALSE)
    }
    node = node[[whichkid]]

  }
    return(current_box)
}

get_rule = function(pred) {
  partykit:::.list.rules.party(pred$model, i = predict(pred$model,
    newdata = attr(pred$model, "x_interest"), type = "node"))
}

test_box1_overlap_box2 = function(box1, box2) {
  all(mapply(function(p1, p2) {
    return(switch(class(p1)[1],
      "ParamDbl" =  p1$lower <= p2$lower & p1$upper >= p2$upper,
      "ParamInt" =  p1$lower <= p2$lower & p1$upper >= p2$upper,
      "ParamFct" = all(p2$levels %in% p1$levels)
      ))
  }, box1$params, box2$params))
}

get_box_classif_iris = function() {
  set.seed(1234L)
  file_path = "test_files/box_classif_iris.RDS"
  if (!file.exists(file_path)) {
    if (!dir.exists("test_files")) {
      dir.create("test_files")
    }
    tree = rpart::rpart(Species ~ ., data = iris)
    mod = as.party(tree)
    x_interest = iris[1L,]
    attr(mod, "x_interest") = x_interest
    pred = Predictor$new(model = mod, data = iris, predict.function = function(model, newdata) {
      node_interest = predict(model, newdata = attr(model, "x_interest"), type = "node")
      prediction = ifelse(predict(model, newdata = newdata, type = "node") == node_interest, 1, 0)
    })

    saveRDS(pred, file = file_path)
  }
  readRDS(file_path)
}

save_test_png = function(code, width = 400, height = 400) {
  path = tempfile(fileext = ".png")
  cowplot::save_plot(path, code)
  path
}


make_test_obj_mixed = function(types = c("double", "integer", "character", "factor", "ordered"),
  task = "regression") {
  set.seed(45748)
  dt = data.table()

  if ("double" %in% types) {
    dt = cbind(dt, data.table(
      var_num_1 = rnorm(10),
      var_num_2 = runif(10, 0, 100)
    )
    )
  }
  if ("integer" %in% types) {
    dt = cbind(dt, data.table(
      var_int_1 = as.integer(sample(1L:10L, size = 10L, replace = TRUE)),
      var_int_2 = as.integer(sample(100:1000L, size = 10L, replace = TRUE))
    )
    )
  }
  if ("character" %in% types) {
    dt = cbind(dt, data.table(
      var_chr_1 = sample(c("ab", "bc", "cd"), size = 10L, replace = TRUE),
      var_chr_2 = sample(c("ene", "mene", "mu", "ma"), size = 10L, replace = TRUE)
    )
    )
  }
  if ("factor" %in% types) {
    dt = cbind(dt, data.table(
      var_fact_1 = as.factor(sample(c("a", "b", "c"), size = 10L, replace = TRUE)),
      var_fact_2 = as.factor(sample(c("e", "f", "g"), size = 10L, replace = TRUE))
    )
    )
  }
  if ("ordered" %in% types) {
    dt = cbind(dt, data.table(
      var_ord_1 = as.ordered(sample(c("a", "b", "c"), size = 10L, replace = TRUE)),
      var_ord_2 = as.ordered(sample(c("e", "f", "g"), size = 10L, replace = TRUE))
    )
    )
  }
  if (task == "regression") {
    dt = cbind(dt, var_target = rnorm(10L, mean = 50, sd = 10))
  } else if (task == "classification") {
    dt = cbind(dt, var_target = as.factor(sample(c(0, 1), size = 10L, replace = TRUE)))
  }
  X = dt[, 1:(ncol(dt) - 1L)]
  rf = randomForest(var_target ~ ., data = dt[-1L,])
  mod = Predictor$new(rf, data = X[-1L,])
  return(list(predictor = mod, data = dt[-1L,], x_interest = X[1L,]))
}


get_rf_classif_iris = function() {
  file_path = "test_files/rf_classif_iris.RDS"
  if (!file.exists(file_path)) {
    if (!dir.exists("test_files")) {
      dir.create("test_files")
    }
    rf = randomForest::randomForest(Species ~ ., data = iris, ntree = 20L)
    saveRDS(rf, file = file_path)
  }
  readRDS(file_path)
}

