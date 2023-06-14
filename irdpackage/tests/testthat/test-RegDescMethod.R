library(randomForest)

# $initialization ------------------------------------------------------------------------------------------------------
test_that("Init works for regression and classification tasks", {
  set.seed(54542142)

  # # Regression task
  # rf_regr = get_rf_regr_mtcars()
  # pred_regr = Predictor$new(rf_regr)
  # expect_error(RegDescMethod$new(predictor = pred_regr), NA)

  # Classification task
  rf_classif = get_rf_classif_iris()
  pred_classif = iml::Predictor$new(rf_classif, type = "class", class = "versicolor")
  expect_error(RegDescMethod$new(predictor = pred_classif, quiet = TRUE), NA)

  # The type of the task is inferred using the `inferTaskFromPrediction` from the iml package.
  # The function is called internally when a Predictor object uses the method `predict` if the task is "unkown".
  # Check that possible changes to this function don't break the code.
  pred_classif$task = NULL
  invisible(pred_classif$predict(iris[1:2, 1:4]))
  expect_identical(pred_classif$task, "classification")
})

# $find_box ------------------------------------------------------------------------------------------------
test_that("$find_box returns meaningful error if x_interest does not contain all columns of predictor$data$X", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred = Predictor$new(rf)
  cr = RegDescMethod$new(predictor = pred, quiet = TRUE)

  expect_snapshot_error(cr$find_box(iris[1L, ]))
})

test_that("x_interest may contain addtional columns to those of predictor$data$X", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred = Predictor$new(rf)
  cr = RegDescMethod$new(predictor = pred, quiet = TRUE)

  # Expect error due to abstract $run method. But for testing purposes this is sufficient here.
  suppressMessages(expect_error(cr$find_box(iris[1L, ], desired_class = "setosa", desired_range = c(0, 0.1)), "cover the prediction"))
})

test_that("$find_box returns meaningful error if x_interest has unexpected column types", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred = Predictor$new(rf)
  cr = RegDescMethod$new(predictor = pred, quiet = TRUE)

  x_interest = iris[1L, ]
  x_interest$Sepal.Length = as.factor(x_interest$Sepal.Length)
  expect_error(cr$find_box(x_interest, desired_class = "setosa"), "same types")
})

test_that("$find_box throughs error if class not specified in predictor", {
  rf_classif = get_rf_classif_iris()
  pred_classif = iml::Predictor$new(rf_classif, type = "class")
  pred_classif$predict(iris[1,])
  cr = RegDescMethod$new(predictor = pred_classif, quiet = TRUE)
  expect_error(cr$find_box(iris[1,], desired_class = "blumina"), "be element of set")
  expect_error(cr$find_box(iris[1,]), "must be specified when calling")
  expect_error(cr$find_box(iris[1,], desired_class = "setosa", desired_range = c(0.5, 1.1)), "2 is not <= 1")
  pred_classif = iml::Predictor$new(rf_classif, type = "class", class = "setosa")
  cr = RegDescMethod$new(predictor = pred_classif, quiet = TRUE)
  expect_error(cr$find_box(iris[1,], desired_range = c(0, .5)), "must cover the prediction of")
})


test_that("own dataset and largest local box could be used", {
  rf_classif = get_rf_classif_iris()
  pred_classif = iml::Predictor$new(rf_classif, type = "prob", class = "setosa")
  x_interest = iris[1,]
  pred_classif$predict(x_interest)
  data = as.data.table(iris)[, -5]
  box_largest = get_max_box(x_interest = data[1,], predictor = pred_classif, param_set = make_param_set(data), desired_range = c(0.5, 1), fixed_features = NULL)
  sampled = SamplerUnif$new(box_largest)$sample(n = 10)$data
  sampled = box_largest$trafo(sampled, predictor = pred_classif)
  expect_true(all(identify_in_box(box = box_largest, data = sampled)))
  cr = RegDescMethod$new(predictor = pred_classif, quiet = TRUE)
  expect_error(cr$find_box(x_interest, desired_range = c(0.5, 1), obsdata = sampled, box_largest = box_largest), "Abstract base class")
  prim = Prim$new(pred_classif, quiet = TRUE)
  primb = prim$find_box(x_interest = x_interest, desired_range = c(0.5, 1), obsdata = sampled, box_largest = box_largest)
  expect_true(all(sampled == prim$.__enclos_env__$private$obsdata[-1, -5]))
  expect_equal(nrow(sampled)+1, prim$calls_fhat)
  mair = Maire$new(pred_classif, convergence = FALSE, num_of_iterations = 100L, quiet = TRUE)
  mairb = mair$find_box(x_interest = x_interest, desired_range = c(0.5, 1), obsdata = sampled, box_largest = box_largest)
  expect_true(all(sampled == mair$.__enclos_env__$private$obsdata[-1,]))
  expect_equal(nrow(sampled), mair$calls_fhat)
  maxbox = MaxBox$new(pred_classif, quiet = TRUE)
  maxboxb = maxbox$find_box(x_interest = x_interest, desired_range = c(0.5, 1), obsdata = sampled, box_largest = box_largest)
  expect_true(all(sampled == maxbox$.__enclos_env__$private$obsdata[-1,]))
  expect_equal(nrow(sampled)+1, maxbox$calls_fhat)
  anchors = Anchor$new(pred_classif, quiet = TRUE)
  anchorsb = anchors$find_box(x_interest = x_interest, desired_range = c(0.5, 1), obsdata = sampled, box_largest = box_largest)
  expect_true(anchors$calls_fhat > nrow(sampled)+1L)
  expect_true(all(sampled == anchors$.__enclos_env__$private$obsdata[-1,-5]))
  postproc = PostProcessing$new(pred_classif, quiet = TRUE)
  postprocb = postproc$find_box(x_interest = x_interest, desired_range = c(0.5, 1), obsdata = sampled, box_largest = box_largest,
    box_init = mairb$box)
  expect_true(all(sampled == postproc$.__enclos_env__$private$obsdata))
  expect_true(postproc$calls_fhat > nrow(sampled)+1L)
})

