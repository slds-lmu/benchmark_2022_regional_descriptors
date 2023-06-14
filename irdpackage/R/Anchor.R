#' Anchors
#'
#' @export
Anchor = R6::R6Class("Anchor", inherit = RegDescMethod,
  public = list(
    #' @param predictor (\link[iml]{Predictor})\cr
    #'   The object (created with `iml::Predictor$new()`) holding the machine
    #'   learning model and the data.
    #' @param tau (numeric(1)) The desired precision an anchor needs to achieve.
    #' If no candidate achieves at least this precision, the one with the best
    #' precision will be returned.
    #' @param initialize_bins (logical(1)) Whether continuous features should be
    #' discretized into bins.
    #' @param bins (list) The discretization bins. Requires one list element for
    #' each feature (same ordering as in predictor$data).
    #' Immutable features defined in `fixed_features` are automatially set to integer().
    #' @param ... \cr Further hyperparameters of anchors (see `anchors::anchors`).
    #' @return (RegDesc) Hyperbox
    #' @import anchors
    initialize = function(predictor, tau = 1, initialize_bins = TRUE, bins = NULL, quiet = FALSE, ...) {

      super$initialize(predictor, quiet)
      if (!is.null(bins)) {
        assert_names(names(bins), subset.of = private$predictor$data$feature.names)
        bins = bins[match(names(bins), pred$data$feature.names)]
      }

      # assign private attr
      if (is.null(bins) & initialize_bins) {
        bins = private$initialize_bins()
      }
      private$bins = bins
      private$tau = tau
      private$argsanchors = list(...)
    }),
  private = list(
    argsanchors = NULL,
    bins = NULL,
    tau = NULL,
    initialize_bins = function() {
      binseq = seq(0, 1, 0.05)
      bins = lapply(private$predictor$data$feature.names, function(col_name){
        if (col_name %in% private$fixed_features) return(integer())
        column = private$predictor$data$X[[col_name]]
        if (is.numeric(column)) {
          num = as.numeric(quantile(column, probs = binseq))
          if (is.integer(column)) {
            num = round(num)
          }
          return(unique(num))
        } else {
          return(integer())
        }
      })
      names(bins) = private$predictor$data$feature.names
      return(bins)
    },
    run = function(){
      # browser()
      # closeAllConnections()
      # message("closed all connections")
      if (is.null(private$obsdata)) {
        private$obsdata = copy(private$predictor$data$get.xy())
      } else {
        private$.calls_fhat = private$.calls_fhat + nrow(private$obsdata)
        private$obsdata[[private$predictor$data$y.names]] = private$predictor$predict(private$obsdata)[[1]]
      }
      xinterest = copy(private$x_interest)
      xinterest[[private$predictor$data$y.names]] = private$f_hat_interest[[1]]
      private$obsdata = rbind(xinterest, private$obsdata)

      current_box = private$param_set

      # adapt bins and training data if fixed_features
      if (!is.null(private$fixed_features)) {
        fixed_features = private$fixed_features
        private$obsdata = private$obsdata[, (fixed_features) := private$x_interest[, fixed_features, with = FALSE]]
        if (!is.null(private$bins[fixed_features])) {
          for (ff in fixed_features) {
            private$bins[[ff]] = integer()
            if (private$predictor$data$feature.types[ff] == "numerical") {
              current_box = update_box(current_box, j = ff, lower = private$x_interest[[ff]],
                upper = private$x_interest[[ff]], complement = FALSE)
            } else {
              current_box = update_box(current_box, j = ff, val = private$x_interest[[ff]],
                complement = FALSE)
            }
          }
        }
      }

      explainer = do.call(anchors::anchors, c(list(x = private$obsdata,
        model = private$predictor, bins = private$bins, tau = private$tau,
        target = private$predictor$data$y.names, verbose = !private$quiet), private$argsanchors))

      predict_model.Predictor <<- function(x, newdata, type, ...) {
        private$.calls_fhat = private$.calls_fhat + nrow(newdata)
        predtab <- predict_range(x, newdata = newdata, range = private$desired_range)
        pred <- predtab
      }

      model_type.Predictor <<- function(x, ...) {
        obj <- "classification"
        # if (is.null(obj)|obj == "unknown") stop('Model type needs to be specified', call. = FALSE)
        # if (is.function(obj)) stop('Unsupported model type', call. = FALSE)
        # if (!obj %in% c("regression", "classification")) stop('Unsupported model type', call. = FALSE)
        return(obj)
      }

      if (private$quiet) {
        explanations = try({suppressMessages(explain(private$x_interest, explainer, labels = 1))})
      } else {
        explanations = try({explain(private$x_interest, explainer)
        })
      }
      # if (private$tries_if_error > 0) {
      #   i = 1
      #   while (inherits(explanations, "try-error") & i <= 3) {
      #     message(explanations)
      #     # closeAllConnections()
      #     # stop("error occurred")
      #     explanations = try({explain(private$x_interest, explainer)})
      #     i = i + 1
      #   }
      # }
      if (inherits(explanations, "try-error")) {
        message(explanations)
        closeAllConnections()
        stop(explanations)
      }

      # postprocess output
      private$.history = explanations
      if (is.null(private$bins)) {
        for (feat in explanations$feature) {
          if (feat == "base") next
          val = private$x_interest[[feat]]
          if (private$predictor$data$feature.types[[feat]] == "categorical") {
            current_box = update_box(current_box = current_box, j = feat, val = as.character(val),
              complement = FALSE)
          } else {
            current_box = update_box(current_box = current_box, j = feat, lower = val, upper = val,
              complement = FALSE)
          }
        }
      } else {
        for (expr in explanations$feature_desc) {
          if (is.na(expr)) next
          feat = gsub("(.+?)( [IN|=].*)", "\\1", expr)
          if (feat %in% private$fixed_features) next
          operator = gsub("(.+?IN )(.)(.*)", "\\2", expr)
          if (operator == "[") {
            val1 = gsub("(.+?\\[)(.+?)(,.*)", "\\2", expr)
            val2 = gsub("(.+?\\[.+?\\,)(.+?)(\\).*)", "\\2", expr)
            val = as.numeric(c(val1, val2))
            current_box = update_box(current_box = current_box,
              j = feat, lower = as.numeric(val[1]), upper = as.numeric(val[2]),
              complement = FALSE)
          } else if (operator == ">") {
            val = gsub("(.+?)(>=)(.+?)", "\\3", expr)
            current_box = update_box(current_box = current_box,
              j = feat, lower = as.numeric(val), complement = FALSE)
          } else if (operator == "<") {
            val = gsub("(.+?)(<=)(.+?)", "\\3", expr)
            current_box = update_box(current_box = current_box,
              j = feat, upper = as.numeric(val),  complement = FALSE)
          } else if (operator == "{") {
            val = gsub("(.+?\\{)(.+?)(\\}.*)", "\\2", expr)
            if (grepl(",", val)) val = eval(parse(text = paste0("c(", val, ")")))
            if (private$param_set$params[[feat]]$is_number) {
              current_box = update_box(current_box = current_box,
                j = feat, lower = min(val), upper = max(val), complement = FALSE)
            }
            current_box = update_box(current_box = current_box,
              j = feat, val = val, complement = FALSE)
          }
        }
      }
      return(current_box)
    },
    print_parameters = function() {
      # cat(" - searchgrid_resolution: ", private$searchgrid_resolution, "\n")
      # cat(" - evaluation_n: ", private$evaluation_n, "\n")
      # cat(" - paste_alpha: ", private$paste_alpha, "\n")
    }
  )
)
