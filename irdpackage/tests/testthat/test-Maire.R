library(randomForest)

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
  mairep = Maire$new(pred, num_of_iterations = 10L, quiet = TRUE)
  mair = mairep$find_box(x_interest = x_interest, desired_range = desired_range, fixed_features = c("carb"))
  expect_data_table(mairep$history, nrows = 10L)
  expect_true(all.equal(mair$box$params$carb$lower, mair$box$params$carb$upper, x_interest$carb))
})


test_that("Box is covered by true box", {

  temp = get_box_regr_mtcars()
  pred = temp$model
  box = temp$box
  x_interest = pred$data$X[1,]

  desired_range = c(0.5, 1)
  method = Maire$new(predictor = pred, num_of_iterations = 500L, quiet = TRUE)
  result = method$find_box(x_interest = x_interest, desired_range = desired_range)

  # expect_true(test_box1_overlap_box2(box1 = box, box2 = result$box))
  expect_true(all(result$evaluate() == 0))

  # coverage and precision increase monotonically
  # expect_true(all(method$history$cov == cummax(method$history$cov)))
  expect_true(all(method$history$prec == cummax(method$history$prec)))

})

test_that("different feature types work", {
  battery = list(mixed = make_test_obj_mixed(),
    numeric = make_test_obj_mixed(types = c("double", "integer")),
    cat = make_test_obj_mixed(types = c("character", "factor", "ordered")),
    mixedclass = make_test_obj_mixed(task = "classification")
  )

  for (test in battery) {
    method = Maire$new(predictor = test$predictor, num_of_iterations = 10L,
      strategy = "sampled", num_sampled_points = 1000L, quiet = TRUE)
    # debug(method$.__enclos_env__$private$run)
    result = method$find_box(x_interest = test$x_interest, desired_class = test$predictor$model$classes[1])
    expect_class(result, classes = "RegDesc")
    expect_true(result$evaluate_train()["precision"] == 1)
  }

})
