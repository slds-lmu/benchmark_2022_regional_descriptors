#' Base Class for Regional Descriptor Methods
#'
#' @description
#' Abstract base class for regional descriptors methods.
#'
#' @section Inheritance:
#' Child classes: \link{RevPrim}, \link{MaireOrig}
RegDescMethod = R6::R6Class("RegDescMethod",

  public = list(

    #' @description Creates a new `RegDescMethod` object.
    #' @param predictor (\link[iml]{Predictor})\cr
    #'   The object (created with `iml::Predictor$new()`) holding the machine
    #'   learning model and the data.
    #' @param quiet (`logical(1)`)\cr Should information about the optimization status be hidden? Default is FALSE.
    initialize = function(predictor, quiet = FALSE) {
      checkmate::assert_class(predictor, "Predictor")
      if (predictor$task == "unknown") {
        predictor$task = NULL
        predictor$predict(predictor$data$X[1:2, ])
      }

      private$predictor = predictor$clone()
      private$quiet = quiet
      # maximum box from available data
      private$param_set = make_param_set(predictor$data$X)
    },

    #' @description
    #' Prints a `RegDescMethod` object.
    #' The method calls a (private) `$print_parameters()` method which should be implemented by the leaf classes.
    print = function() {
      cat("Regional descriptor method: ", class(self)[1], "\n")
      cat("Parameters:\n")
      private$print_parameters()
    },
    #'
    #' Runs the hyperrectangle searching algorithm and returns the hyperrectangles interval ranges.
    #' All observations in the hyperrectangle should have a predicted probability in the interval `desired_prob`
    #' (for classification for a `desired_class`).
    #'
    #' @param x_interest (`data.table(1) | data.frame(1)`) \cr A single row with the observation of interest.
    #' @param desired_range (`numeric(2)`) \cr
    #'   The desired predicted outcome range - a vector with two numeric values
    #'   that specify an outcome interval.
    #'   For regression the interval operates on the numeric outcome,
    #'   while for classification it reflects either a hard label or
    #'   the probability for the class with the largest probability among all
    #'   possible outcome classes.
    #' @param desired_class (`character(1)` | `NULL`) \cr The desired class. Ignored if predictor$task = "regression".
    #' If NULL (default) for a classification task then predictor$class is taken.
    #' @param obsdata (`data.table` | `data.frame`) Data set used to find the box. If NULL (default) either
    #' predictor$data or newly sampled data according to the specified `strategy` is used.
    #' @param fixed_features (`character()` | `NULL`) \cr
    #' Names of features that are not allowed to be changed. NULL (default) allows all features to be changed.
    #' @param box_largest (`ParamSet`  | `NULL`) \cr
    #' Largest initial box. If NULL, largest box is generated.
    #' @param box_init (`ParamSet` | `NULL`) \cr Initial box to process. Ignored if method is not `PostProcessing`.
    #
    find_box = function(x_interest, desired_range = NULL, obsdata = NULL, fixed_features = NULL, desired_class = NULL, box_largest = NULL, box_init = NULL) {
      if (!is.null(fixed_features)) {
        assert_names(fixed_features, subset.of = private$predictor$data$feature.names)
      }

      # Checks x_interest
      assert_data_frame(x_interest, nrows = 1L)
      assert_names(names(x_interest), must.include = names(private$predictor$data$X))
      x_interest = setDT(x_interest)[, names(private$predictor$data$X), with = FALSE]
      # x_interest = data.table::data.table(x_interest)[, names(private$predictor$data$X), with = FALSE]
      if (any(sapply(x_interest, typeof) != sapply(private$predictor$data$X, typeof))) {
        stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
      }

      f_hat_interest = private$predictor$predict(x_interest)

      # Check desired_class
      if (private$predictor$task == "classification") {
        if (is.null(desired_class)) {
          if (is.null(private$predictor$class)) {
            stop("For classification models `desired_class` must be specified when calling $find_box().")
          } else {
            desired_class = private$predictor$class
            message(sprintf("The `desired_class` was set to `predictor$class` which is %s.", desired_class))
          }
        }
        assert_character(desired_class, len = 1L, any.missing = FALSE)
        assert_choice(desired_class, choices = names(f_hat_interest))
        private$predictor$class = desired_class
        f_hat_interest = private$predictor$predict(x_interest)[[1]]
      } else {
        if (!is.null(desired_class)) {
          message(sprintf("For regression models `desired_class = %s` is ignored.", desired_class))
        }
      }

      # Check desired_range

      checkmate::assert_numeric(desired_range, min.len = 1L,  max.len = 2L,
        null.ok = TRUE)
      if (is.null(desired_range)) {
        f_hat_data = private$predictor$predict(private$predictor$data$get.x())[[1]]
        sdf_hat = 1/2*sd(f_hat_data)
        desired_range = c(f_hat_interest[[1]] - sdf_hat, f_hat_interest[[1]] + sdf_hat)
        if (private$predictor$task == "classification") {
          # cap between 0 and 1
          desired_range = c(max(min(desired_range), 0), min(max(desired_range), 1))
        }
        message(sprintf("'desired_range' is NULL. Using 1/2 standard deviation of predictions of observed data (predictor$data), it was set to 'c(%f, %f)'.",
          desired_range[1], desired_range[2]))
      } else {
        if (private$predictor$task == "classification") {
          checkmate::assert_numeric(desired_range, min.len = 1L,  max.len = 2L,
            lower = 0, upper = 1)
        }
        if (length(desired_range) == 1L) {
          desired_range = c(desired_range, desired_range)
        }
        if (desired_range[2L] < desired_range[1L]) {
          stop("The lower bound of `desired_range` cannot be greater than the upper bound.")
        }
        if (f_hat_interest < desired_range[1] | f_hat_interest > desired_range[2]) {
          stop(sprintf("`desired_range` must cover the prediction of `x_interest` of %s", round(f_hat_interest, 3)))
        }
      }


      # Check fixed_features
      if (!is.null(fixed_features)) {
        assert_names(fixed_features, subset.of = private$predictor$data$feature.names)
      }

      # Check box_init
      assert_class(box_init, "ParamSet", null.ok = TRUE)
      if (!is.null(box_init)) {
        assert_set_equal(names(box_init$params), private$predictor$data$feature.names)
        # <FIXME:> Take update fixed_features into account!
      }

      assert_class(box_largest, "ParamSet", null.ok = TRUE)
       if (!is.null(box_largest)) {
        assert_set_equal(names(box_largest$params), private$predictor$data$feature.names)
      }

      # Update private$param_set
      private$param_set = make_param_set(rbind(x_interest, private$predictor$data$X))

      # Check obsdata
      if (!is.null(obsdata)) {
        obsdata = as.data.table(obsdata)[, names(private$predictor$data$X), with = FALSE]
        assert_true(all(identify_in_box(box = private$param_set, data = obsdata)))
      }

      # Set number of calls to fhat to 0
      private$.calls_fhat = 0

      private$x_interest = x_interest
      private$f_hat_interest = f_hat_interest
      private$desired_range = desired_range
      private$fixed_features = fixed_features
      private$desired_class = desired_class
      private$obsdata = obsdata
      private$box_largest = box_largest
      private$box_init = box_init
      box = private$run()
      box = private$sanitize_box(box)

      RegDesc$new(box = box, predictor = private$predictor,
        x_interest = x_interest, desired_range = desired_range,
        fixed_features = fixed_features, desired_class = desired_class,
        method = class(self)[1])

    }),
    active = list(
      #' @field history (`data.table`) \cr
      #'  stores for each iteration of the method the chosen variable, boundary
      #'  value and evaluation measures.
      history = function(value) {
        if (missing(value)) {
          private$.history
        } else {
          stop("`$history` is read only", call. = FALSE)
        }
      },
      calls_fhat = function(value) {
        if (missing(value)) {
          private$.calls_fhat
        } else {
          stop("`$calls_fhat` is read only", call. = FALSE)
        }
      }
  ),

  private = list(
    predictor = NULL,
    quiet = NULL,
    param_set = NULL,
    x_interest = NULL,
    f_hat_interest = NULL,
    desired_range = NULL,
    obsdata = NULL,
    fixed_features = NULL,
    desired_class = NULL,
    box_largest = NULL,
    box_init = NULL,
    .history = NULL,
    .calls_fhat = NULL,
    sanitize_box = function(box) {
      for (j in names(box$params)) {
        if (class(box$params[[j]])[1] == "ParamInt") {
          b = box$params[[j]]
          box$params[[j]]$lower = ceiling(b$lower)
          box$params[[j]]$upper = floor(b$upper)
        }
      }
      return(box)
    },
     run = function() {
      stop("Abstract base class")
    },
    print_parameters = function() {}
  )
)
