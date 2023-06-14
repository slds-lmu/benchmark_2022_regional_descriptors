library(randomForest)

test_that("Box is covered by true box", {

  temp = get_box_regr_mtcars()
  pred = temp$model
  box = temp$box

  # rule = partykit:::.list.rules.party(pred$model, i = predict(pred$model,
  #   newdata = attr(pred$model, "x_interest"), type = "node"))
  x_interest = pred$data$X[1,]

  desired_range = c(0.5, 1)
  anc = Anchor$new(predictor = pred, quiet = TRUE)
  an = anc$find_box(x_interest = x_interest, desired_range = desired_range)

  expect_true(test_box1_overlap_box2(box1 = box, box2 = an$box))
  expect_true(all(an$evaluate() == 0))

})
#
#
# test_that("different feature types work", {
#   battery = list(mixed = make_test_obj_mixed(),
#     numeric = make_test_obj_mixed(types = c("double", "integer")),
#     cat = make_test_obj_mixed(types = c("character", "factor", "ordered")),
#     mixedclass = make_test_obj_mixed(task = "classification")
#   )
#
#   for (test in battery) {
#     method = Anchor$new(predictor = test$predictor)
#     result = method$find_box(x_interest = test$x_interest)
#   }
#
# })
