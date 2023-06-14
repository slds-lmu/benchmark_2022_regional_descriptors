test_that("Regression and mixed features + fixed_features", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.integer(mydf$cyl)

  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")

  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_range = c(15, 25)
  box = make_param_set(mydf[,names(mydf)!="mpg"])
  method = PostProcessing$new(pred, subbox_relsize = 0.5, evaluation_n = 50L, quiet = TRUE)
  res = method$find_box(x_interest = x_interest,
    desired_range = desired_range, fixed_features = c("carb"), box_init = box)
})



test_that("Box is covered by true box", {

  temp = get_box_regr_mtcars()
  pred = temp$model
  box = temp$box
  x_interest = pred$data$X[1,]

  desired_range = c(0.5, 1)

  set.seed(1234L)
  largestbox = make_param_set(pred$data$X)
  method = PostProcessing$new(pred, subbox_relsize = 0.5, evaluation_n = 50L, quiet = TRUE)
  result = method$find_box(x_interest = x_interest,
    desired_range = desired_range, box_init = largestbox)

  expect_true(all(result$evaluate() == 0))

})

test_that("different feature types work", {
  battery = list(mixed = make_test_obj_mixed(),
    numeric = make_test_obj_mixed(types = c("double", "integer")),
    cat = make_test_obj_mixed(types = c("character", "factor", "ordered")),
    mixedclass = make_test_obj_mixed(task = "classification")
  )

  for (test in battery) {
    largestbox = make_param_set(rbind(test$x_interest, test$data[, var_target := NULL]))
    method = PostProcessing$new(test$predictor, subbox_relsize = 0.5, evaluation_n = 5L, quiet = TRUE)
    set.seed(1234L)
    result = method$find_box(x_interest = test$x_interest,
      desired_class = test$predictor$model$classes[1], box_init = largestbox)
    expect_true(identify_in_box(result$box, data = test$x_interest))
  }

})
