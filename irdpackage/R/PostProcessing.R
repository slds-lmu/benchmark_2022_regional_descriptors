#' PostProcessing
#'
#' @description Proposed post-processing method
#'
#' @export
PostProcessing = R6::R6Class("PostProcessing", inherit = RegDescMethod,
  public = list(
    #' @param subbox_relsize (`numeric(1)`) \cr Number of proposed values
    #' to search over for numeric features. If increased, more upper and lower values are
    #' inspected. Default = 30 meaning that each subbox covers 1/30 = 3.3 % of whole observed
    #' feature range.
    #' @param evaluation_n (`numeric(1)`) \cr The number of samples randomly drawn for a specific subbox in order to evaluate this box.
    #' @param paste_alpha (NULL | `numeric(1)`)  Stopping criterion. Minimum fraction of box
    #' allowed for adding. See `Details`.
    #' @param strategy_ties (`character(1)`) \cr Strategy to use if
    #'  multiple subboxes have 0 impurity in order to break ties. Options are
    #'  `'random'` to choose randomly, `'preddist'` to choose box with lowest average
    #'  distance to prediction of `x_interest`. Default is `'preddist'`.
    #'
    #' @return (RegDesc) Hyperbox
    initialize = function(predictor, subbox_relsize = 0.05,
      evaluation_n = 1000, paste_alpha = 0.05, strategy_ties = "preddist", quiet = FALSE) {
      # input checks
      super$initialize(predictor, quiet)
      checkmate::assert_numeric(subbox_relsize, lower = 0, upper = 1)
      checkmate::assert_integerish(evaluation_n, lower = 0)
      checkmate::assert_numeric(paste_alpha, len = 1L)
      checkmate::assert_names(strategy_ties, subset.of = c("preddist", "random"))
      # assign private attr
      private$subbox_relsize = subbox_relsize
      private$evaluation_n = evaluation_n
      private$paste_alpha = paste_alpha
      private$strategy_ties = strategy_ties

    }),
  private = list(
    subbox_relsize = NULL,
    evaluation_n = NULL,
    i = 0,
    alpha = 1,
    lookup_sizes = NULL,
    paste_alpha = NULL,
    strategy_ties = NULL,
    box_largest = NULL,
    searchspace = NULL,
    run = function(){
      private$i = 0L

      # estimate largest box according to ice values
      # get ranges for features
      if (is.null(private$box_largest)) {
        private$box_largest = get_max_box(private$x_interest, private$fixed_features,
          predictor = private$predictor,
          param_set = private$param_set, desired_range = private$desired_range,
          resolution = 500L)
      }

      if (is.null(private$box_init)) stop("`box_init` must be specified")

      # smallest box (includes only x_interest)
      box_new = private$box_init
      # <FIXME:> Take update fixed_features into account!

      vars_diff = setdiff(private$predictor$data$feature.names, private$fixed_features)
      # vars_diff = private$predictor$data$features.names

      sampled = SamplerUnif$new(box_new)$sample(n = private$evaluation_n*5)$data
      sampled = box_new$trafo(sampled, predictor = private$predictor)

      private$.calls_fhat = private$.calls_fhat + nrow(sampled)
      homogeneous = sum(predict_range(private$predictor, newdata = sampled,
        range = private$desired_range)) == nrow(sampled)

      # divide feature ranges into equally sized bins, each bin containing >=
      # 1/subbox_relsize * 100% of the overall feature range
      private$lookup_sizes = lapply(vars_diff, FUN = function(j) {
        ps = private$param_set$clone()$subset(j)
        if (ps$all_numeric) {
          (ps$upper - ps$lower)/(1/private$subbox_relsize)
        }
      })
      names(private$lookup_sizes) = vars_diff

      # Remove subboxes
      if (!homogeneous) {
        # identify features that are set to x_interest --> nothing to peel

        # define bounds of removed boxes using an equidistant grid,
        # granularity/resolution is driven by subbox_relsize
        # consider size of box!!!
        private$searchspace = lapply(vars_diff, FUN = function(j) {
          ps = box_new$clone()$subset(j)
          if (ps$all_numeric) {
            if (box_new$upper[[j]] == box_new$lower[[j]]) return(NULL)
            if (ps$lower[[1]] < private$x_interest[[j]]) {
              xvecl = c(seq(ps$lower[[1]], private$x_interest[[j]], by = private$lookup_sizes[[j]][[1]])[-1], private$x_interest[[j]])
            } else {
              xvecl = numeric()
              box_new = update_box(box_new, j = j, lower = private$x_interest[[j]])
            }
            if (ps$upper > private$x_interest[[j]]) {
              xvecu = c(seq(ps$upper, private$x_interest[[j]], by = -private$lookup_sizes[[j]])[-1], private$x_interest[[j]])
              } else {
              xvecu = numeric()
              box_new = update_box(box_new, j = j, upper = private$x_interest[[j]])
            }
            if (ps$storage_type[[1]] == "integer") {
              xvecl = unique(round(xvecl))
              xvecu = unique(round(xvecu))
            }
            return(list(lower = xvecl, upper = xvecu))
          } else if (ps$all_categorical) {
            if (box_new$nlevels[[j]] == 1) return(NULL)
            return(list(val = setdiff(ps$levels[[j]], as.character(private$x_interest[[j]]))))
          }
        })
        names(private$searchspace) = vars_diff
        for (vd in vars_diff) {
          private$searchspace = private$declutter_searchspace(vd)
        }

        while (!homogeneous) {
          box_update = private$peeling_subbox(box_new = box_new)
          if (!is.null(box_update)) {
            box_new = box_update
            rm(box_update)
          } else {
            homogeneous = TRUE
          }
        }
      }

      private$searchspace = list()
      for (j in vars_diff) {
        ps = private$box_largest$clone()$subset(j)
        if (ps$all_numeric) {
          if (box_new$lower[[j]] > ps$lower[[1]]) {
            xvecl = unique(c(seq(box_new$lower[[j]], ps$lower[[1]], by = -private$lookup_sizes[[j]][[1]])[-1], ps$lower[[1]]))
          } else {
            xvecl = numeric()
            box_new = update_box(box_new, j = j, lower = ps$lower[[1]])
          }
          if (box_new$upper[[j]] < ps$upper[[1]]) {
            xvecu = unique(c(seq(box_new$upper[[j]], ps$upper[[1]], by = private$lookup_sizes[[j]])[-1], ps$upper[[1]]))
          } else {
            xvecu = numeric()
            box_new = update_box(box_new, j = j, upper = ps$upper[[1]])
          }
          if (ps$storage_type[[1]] == "integer") {
            xvecl = unique(round(xvecl))
            xvecu = unique(round(xvecu))
          }
          private$searchspace = c(private$searchspace, list(list(lower = xvecl, upper = xvecu)))
        } else if (ps$all_categorical) {
          private$searchspace = c(private$searchspace, list(list(val = setdiff(ps$levels[[j]], box_new$levels[[j]]))))
        }
      }
      names(private$searchspace) = vars_diff
      for (vd in vars_diff) {
        private$searchspace = private$declutter_searchspace(vd)
      }
      private$i = 0L
      temp = 0L
      # Main algorithm for adding boxes
      while (private$alpha > private$paste_alpha & length(private$searchspace) > 0 & temp < 300) {
        box_new = private$pasting_subbox(box_new = box_new)
        if (private$alpha < 1) {
          temp = temp + 1L
        }
      }
      return(box_new)
    },
    create_subbox = function(current_box, j, lower = NULL, upper = NULL, val = NULL) {
      new_box = update_box(current_box = current_box, j = j, lower = lower, upper = upper, val = val, complement = TRUE)

      if (!is.null(lower) && !is.na(lower)) {
        new_box$params[[j]]$upper = current_box$params[[j]]$lower
      }
      if (!is.null(upper) && !is.na(upper)) {
        new_box$params[[j]]$lower = current_box$params[[j]]$upper
      }

      if (!is.null(val) && !is.na(val)) {
        new_box$params[[j]]$levels = setdiff(val, current_box$params[[j]]$levels)
      }

      return(new_box)
    },
    evaluate_box = function(box, desired_range, x_interest) {
      private$.calls_fhat = private$.calls_fhat + private$evaluation_n
      evaluate_box(box, predictor = private$predictor, x_interest = x_interest,
        n_samples = private$evaluation_n, desired_range)
    },
    peeling_subbox = function(box_new) {
      a = lapply(sample(names(private$searchspace)), FUN = function(j) { #<FIXME:> mclapply
        res = data.table(var = character(), lower = numeric(), upper = numeric(),
          val = character(), impurity = numeric(), dist = numeric(), size = numeric())
        # identify boxes and evaluate them
        if (box_new$is_categ[[j]]) {
          res = data.table()
          for (cat in private$searchspace[[j]]$val) {
            subbox = update_box(current_box = box_new, j = j, lower = NULL, upper = NULL,
              val = cat, complement = FALSE)
            eval = private$evaluate_box(box = subbox, desired_range = private$desired_range,
              x_interest = private$x_interest)
            size = (1/private$param_set$nlevels[[j]])/private$subbox_relsize
            resrow = data.table(var = j, lower = NA, upper = NA, val = cat, impurity = eval[1], dist = eval[2], size = size)
            res = rbind(res, resrow)
          }
        } else {
          for (l in names(private$searchspace[[j]])) {
            bound = private$searchspace[[j]][[l]][1]
            if (l == "lower") {
              subbox = update_box(current_box = box_new, j = j, upper = bound)
            } else {
              subbox = update_box(current_box = box_new, j = j, lower = bound)
            }
            size =  (subbox$upper[[j]]- subbox$lower[[j]])/private$lookup_sizes[[j]]
            eval = private$evaluate_box(subbox, desired_range = private$desired_range,
              x_interest = private$x_interest)
            resrow = data.table(var = j, lower = NA, upper = NA, val = NA, impurity = eval[1], dist = eval[2], size = size)
            resrow[, (l) := bound]
            res = rbind(res, resrow)
          }
        }
        res$mode = "peeling"
        return(res)
      })
      res_table = rbindlist(a)

      if (nrow(res_table) == 0) {
        return(NULL)
      }

      ## identify worst (largest impurity) relative to size (if both boxes same precision, choose smaller one!)
      best = res_table[order(impurity/size, -dist, decreasing = TRUE)[1]]
      # <FIXME:> change this to allow for slight impurity --> new parameter

      if (best$impurity == 0) {
        return(NULL)
      }

      # make boxes smaller with respect to one direction
      if (!is.na(best$upper)) {
        private$searchspace[[best$var]]$upper = private$searchspace[[best$var]]$upper[-1]
      } else if (!is.na(best$lower)) {
        private$searchspace[[best$var]]$lower = private$searchspace[[best$var]]$lower[-1]
      } else if (!is.na(best$val)) {
        private$searchspace[[best$var]]$val = setdiff(private$searchspace[[best$var]]$val, best$val)
      }
      ## Declutter searchspace
      private$searchspace = private$declutter_searchspace(best$var)
      box_new = update_box(current_box = box_new, j = best$var, lower = best[["lower"]],
        upper = best[["upper"]], val = setdiff(box_new$levels[[best$var]], best[["val"]]), complement = FALSE)

      ## Save info in history
      best$alpha = 1
      private$.history = rbind(private$.history, best)
      private$i = private$i+1L
      if (!private$quiet) {
        message(paste("peeling iteration", private$i, "with peeling variable", best$var, "and impurity = ", best$impurity))
      }
      return(box_new)
    },
    pasting_subbox = function(box_new) {
      a = lapply(sample(names(private$searchspace)), FUN = function(j) {
        res = data.table(var = character(), lower = numeric(), upper = numeric(),
          val = character(), impurity = numeric(), dist = numeric(), size = numeric())
        # identify boxes and evaluate them
        if (box_new$is_categ[[j]]) {
          res = data.table()
          for (cat in private$searchspace[[j]]$val) {
            box = private$create_subbox(current_box = box_new, j = j, lower = NULL, upper = NULL, val = cat)
            eval = private$evaluate_box(box = box, desired_range = private$desired_range,
              x_interest = private$x_interest)
            size = (1/private$param_set$nlevels[[j]])/private$subbox_relsize
            resrow = data.table(var = j, lower = NA, upper = NA, val = cat, impurity = eval[1], dist = eval[2], size = size)
            res = rbind(res, resrow)
          }
        } else {
          for (l in names(private$searchspace[[j]])) {
            bound = private$alpha*(private$searchspace[[j]][[l]][1])+(1-private$alpha)*box_new$params[[j]][[l]]
            if (l == "lower") {
              subbox = private$create_subbox(current_box = box_new, j = j, lower = bound)
            } else {
              subbox = private$create_subbox(current_box = box_new, j = j, upper = bound)
            }
            size =  (subbox$upper[[j]]- subbox$lower[[j]])/private$lookup_sizes[[j]]
            eval = private$evaluate_box(subbox, desired_range = private$desired_range,
              x_interest = private$x_interest)
            resrow = data.table(var = j, lower = NA, upper = NA, val = NA, impurity = eval[1], dist = eval[2], size = size)
            resrow[, (l) := bound]
            res = rbind(res, resrow)
          }
        }
        res$mode = "pasting"
        res$alpha = private$alpha
        return(res)
      })
      res_table = rbindlist(a)
      # remove the categories with impurity > 0
      # <FIXME:> change this to allow for slight impurity
      remove = res_table[!is.na(val) & impurity > 0, ]
      if (nrow(remove) > 0) {
        for (i in seq_len(nrow(remove))) {
          private$searchspace[[remove[i, var]]]$val = setdiff(private$searchspace[[remove[i, var]]]$val, remove[i, val])
          if (length(private$searchspace[[remove[i, var]]]$val) == 0) {
            private$searchspace[[remove[i, var]]] = NULL
          }
        }
      }
      # identify best (full purity + lowest distance to pred_x_interest)
      # only impure --> smaller boxes
      if (private$strategy_ties == "random") {
        best = res_table[order(impurity, size)[1]]
      } else {
        # choose box with impurity == 0 and with smallest changes in prediction function relative to size!
        best = res_table[order(impurity, dist*size)[1]]
      }
      # <FIXME:> change this to allow for slight impurity --> new parameter
      if (best$impurity > 0) {
        private$alpha = private$alpha*1/2
      } else {
        # make boxes larger with respect to one direction
        if (!is.na(best$upper) && private$alpha == 1) {
          private$searchspace[[best$var]]$upper = private$searchspace[[best$var]]$upper[-1]
        } else if (!is.na(best$lower) && private$alpha == 1) {
          private$searchspace[[best$var]]$lower = private$searchspace[[best$var]]$lower[-1]
        } else if (!is.na(best$val)) {
          private$searchspace[[best$var]]$val = setdiff(private$searchspace[[best$var]]$val, best$val)
        }
        ## Declutter searchspace
        private$searchspace = private$declutter_searchspace(best$var)
        box_new = update_box(current_box = box_new, j = best$var, lower = best[["lower"]],
          upper = best[["upper"]], val = best[["val"]], complement = TRUE)
        ## Save info in history
        private$.history = rbind(private$.history, best)
        private$i = private$i+1L
        if (!private$quiet) {
          message(paste("pasting iteration", private$i, "with pasting variable", best$var, "and alpha = ", private$alpha))
        }
      }
      return(box_new)
    },
    declutter_searchspace = function(var) {
      s = private$searchspace[[var]]
      private$searchspace[[var]] = s[lapply(s, length) > 0]
      private$searchspace = private$searchspace[lapply(private$searchspace, length) > 0]
    },
    print_parameters = function() {
      cat(" - subbox_relsize: ", private$subbox_relsize, "\n")
      cat(" - evaluation_n: ", private$evaluation_n, "\n")
      cat(" - paste_alpha: ", private$paste_alpha, "\n")
    }
  )
)
