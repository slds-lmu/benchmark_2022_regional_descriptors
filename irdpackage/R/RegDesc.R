RegDesc = R6::R6Class("RegDesc",
  public = list(
    #' Creates a new Regional Descriptor (`RegDesc`) object.
    #' This method should only be called by the `$find_box` methods of \link{RevPrim}.
    #' @param box (`ParamSet`)
    #' @param x_interest (`data.table(1)` | `data.frame(1)`) \cr
    #'   A single row with the observation of interest.
    #' @param predictor (\link[iml]{Predictor})\cr
    #'   The object (created with `iml::Predictor$new()`) holding the machine learning model and the data.
    #' @param desired_range (NULL | `numeric(2)`) \cr
    #' The desired predicted outcome. If NULL (default), the current predicted value of `predictor` for `x_interest` is used as desired prediction.
    #' Alternatively, a vector with two numeric values that specify an outcome interval. This outcome interval needs to include the predicted value of `x_interest`.
    initialize = function(box, predictor, x_interest, desired_range, fixed_features = NULL,
      desired_class = NULL, method = NULL) {
      # input checks
      assert_class(box, "ParamSet")
      assert_class(predictor, "Predictor")
      assert_data_frame(x_interest, nrows = 1L)
      setDT(x_interest)
      assert_numeric(desired_range, len = 2L)
      assert_character(desired_class, null.ok = TRUE)
      if (!is.null(fixed_features)) {
        assert_names(fixed_features, subset.of = predictor$data$features.names)
      }
      assert_character(method)
      assert_names(names(box$params), permutation.of = predictor$data$feature.names)

      # assign
      private$.box = box
      private$predictor = predictor
      private$.x_interest = x_interest
      private$.desired_range = desired_range
      private$.desired_class = desired_class
      private$.method = method
      if (predictor$task == "unknown") {
        if (is.numeric(predictor$data$y[[1]])) {
          predictor$task = "regression"
        } else {
          predictor$task = "classification"
        }
      }
      private$param_set = make_param_set(predictor$data$X)

      private$.box_single = get_max_box(x_interest = private$.x_interest,
        fixed_features = private$.fixed_features, predictor = private$predictor,
        desired_range = private$.desired_range, param_set = private$param_set,
        resolution = 50L)
    },

    #' @description Prints the `RegDesc` object.
    print = function(digits = 2L) {
      desired = private$.desired_range
      cat(nrow(private$.data), "Regional Descriptors \n \n")
      if (private$predictor$task == "classification") {
        cat("Desired class:", private$predictor$class, "\n")
      }
      if (desired[1L] != desired[2L]) {
        cat(
          "Desired range: [", desired[1L], ", ",
          desired[2L],  "] \n \n", sep = ""
        )
      } else {
        cat("Desired range:", desired[1L], "\n \n")
      }
      cat("Descriptor: \n")
      box = private$.box
      results = describe_box(box, digits = digits)
      full = describe_box(private$param_set, digits = digits)
      results_single = describe_box(private$.box_single, digits = digits)
      x_interest_print = copy(private$.x_interest)
      x_interest_print = as.character((x_interest_print[, (1:ncol(x_interest_print)) := lapply(.SD, as.character), .SDcols= 1:ncol(x_interest_print)]))
      results = data.frame(cbind(feature = results[["id"]], x_interest = x_interest_print,
        box = results[["range"]], box_single = results_single[["range"]], range = full[["range"]]))
      names(results) = c("feature", "x_interest", "regional descriptor", "1-dim descriptor", "range")
      # res = xtable::xtable(results)
      # print(res, include.rownames = FALSE)
      print(results)
    },

    plot_surface = function(feature_names, grid_size = 250L, surface = "prediction") {
      assert_names(surface, subset.of = c("prediction", "range"))
      assert_names(feature_names, subset.of = names(private$.box$params))
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
      }
      make_surface_plot(box = private$.box, param_set = private$param_set, grid_size = grid_size, predictor = private$predictor,
        x_interest = private$.x_interest, feature_names = feature_names, surface = surface, desired_range = private$.desired_range)
    },
    evaluate = function(n_samples = 100) {
      evaluate_box(box = private$.box, x_interest = private$.x_interest,
        predictor = private$predictor, n_samples = n_samples,
        desired_range = private$.desired_range)
    },
    evaluate_train = function() {
      evaldt = rbind(private$.x_interest, private$predictor$data$X)
      inbox = identify_in_box(private$.box, evaldt)
      evaldt = evaldt[inbox,]
      precision = sum(predict_range(private$predictor, newdata = evaldt, range = private$.desired_range))/sum(inbox)
      coverage = sum(inbox)/nrow(private$predictor$data$X)
      return(c(precision = precision, coverage = coverage))
    }),
  active = list(
    desired_range = function(value) {
      if (missing(value)) {
        private$.desired_range
      } else {
        stop("`$desired_range` is read only", call. = FALSE)
      }
    },
    #' @field box (`data.table`)\cr
    #'  The regional descriptor for `x_interest`.
    box = function(value) {
      if (missing(value)) {
        private$.box
      } else {
        stop("`$box` is read only", call. = FALSE)
      }
    },
    #' @field box_single (`data.table`)\cr
    #'  The regional descriptors only mutating a single feature for `x_interest`.
    box_single = function(value) {
      if (missing(value)) {
        private$.box_single
      } else {
        stop("`$box` is read only", call. = FALSE)
      }
    },
    #' @field x_interest (`data.table(1)`) \cr
    #'   A single row with the observation of interest.
    x_interest = function(value) {
      if (missing(value)) {
        private$.x_interest
      } else {
        stop("`$x_interest` is read only", call. = FALSE)
      }
    },
    #' @field method (`character(1)`) \cr
    #'   Method with which regional descriptors were generated.
    method = function(value) {
      if (missing(value)) {
        private$.method
      } else {
        stop("`$method` is read only", call. = FALSE)
      }
    },
    #' @field fixed_features (`character(1)`) \cr
    #'   Method with which regional descriptors were generated.
    fixed_features = function(value) {
      if (missing(value)) {
        private$.fixed_features
      } else {
        stop("`$fixed_features` is read only", call. = FALSE)
      }
    },
    #' @field desired_class (`character(1)`) \cr
    #'   Desired class. NULL for regression models
    desired_class = function(value) {
      if (missing(value)) {
        private$.desired_class
      } else {
        stop("`$desired_class` is read only", call. = FALSE)
      }
    }
  ),

  private = list(
    .box = NULL,
    .box_single = NULL,
    predictor = NULL,
    .x_interest = NULL,
    .desired_range = NULL,
    .fixed_features = NULL,
    .desired_class = NULL,
    .method = NULL,
    param_set = NULL
  )
)
