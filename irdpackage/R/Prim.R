#' PRIM
#'
#' @export
Prim = R6::R6Class("Prim", inherit = RegDescMethod,

  public = list(
    #' @param subbox_relsize (`numeric(1)`) \cr Number of proposed values
    #' to search over for numeric features. If increased, more upper and lower values are
    #' inspected. Default = 0.05 meaning that each subbox covers 5 % of the data points in
    #' a box.
    #' #' @param strategy (`character(1)`)\cr
    #' Either `traindata` using training data or `sampled` using newly sampled data in
    #' ICE curve identified box.
    #' @param num_sampled_points (`numeric(1)`)\cr Only considered if `strategy = 'sampled'`.
    #' The number of samples randomly drawn at the beginning.
    #' @return (RegDesc) Hyperbox
    #' @export
    initialize = function(predictor, subbox_relsize = 0.05, strategy = "traindata", num_sampled_points = 500L, quiet = FALSE) {
      # input checks
      super$initialize(predictor, quiet)
      checkmate::assert_numeric(subbox_relsize, lower = 0, upper = 1)
      assert_character(strategy, len = 1L)
      assert_names(strategy, subset.of = c("traindata", "sampled"))

      # assign private attr
      private$subbox_relsize = subbox_relsize
      private$strategy = strategy
      if (strategy == "sampled") {
        assert_numeric(num_sampled_points, lower = 1, finite = TRUE, len = 1L, null.ok = FALSE)
        private$num_sampled_points = num_sampled_points
      }
    }),
  private = list(
    subbox_relsize = NULL,
    strategy = NULL,
    num_sampled_points = NULL,
    lookup_sizes = NULL,
    i = NULL,
    evalcurrent = NULL,
    box_largest = NULL,
    run = function(){

      # Get data (either training or newly sampled depending on strategy)
      if (is.null(private$obsdata)) {
        private$obsdata = sampling(predictor = private$predictor, x_interest = private$x_interest,
          fixed_features = private$fixed_features, desired_range = private$desired_range,
          param_set = private$param_set, num_sampled_points = private$num_sampled_points,
          strategy = private$strategy)
      }
      private$obsdata = rbind(private$x_interest, private$obsdata[, private$predictor$data$feature.names, with = FALSE])
      private$.calls_fhat = private$.calls_fhat + nrow(private$obsdata)
      private$obsdata = private$obsdata[, positive := predict_range(private$predictor, newdata = private$obsdata, range = private$desired_range)]

      # estimate largest box according to ice values
      # get ranges for features
      if (is.null(private$box_largest)) {
        private$box_largest = get_max_box(private$x_interest, private$fixed_features,
          predictor = private$predictor,
          param_set = private$param_set, desired_range = private$desired_range,
          resolution = 500L) # <FIXME:> is this a good parameter value??
      }

      assert_true(identify_in_box(private$box_largest, private$x_interest))

      # features to change
      vars_diff = setdiff(private$predictor$data$feature.names, private$fixed_features)

      # # define bounds of removed boxes using quantiles
      private$lookup_sizes = lapply(vars_diff, FUN = function(j) {
        ps = private$param_set$clone()$subset(j)
        if (ps$all_numeric) {
          (ps$upper - ps$lower)/(1/private$subbox_relsize)
        }
      })
      names(private$lookup_sizes) = vars_diff

      history = data.table()
      box_new = private$box_largest$clone()
      private$i = 0L

      ### PEELING ###

      heterogeneous = sum(private$obsdata$positive == 0) > 0
      # Main algorithm for removing boxes
      while (heterogeneous) {
        a = lapply(sample(vars_diff), FUN = function(j) {
          res = data.table(var = character(), lower = numeric(), upper = numeric(),
            val = character(), impurity = numeric(), coverage = numeric(), size = numeric())
          if (box_new$is_categ[[j]]) {
            res = data.table()
            for (cat in setdiff(box_new$params[[j]]$levels, private$x_interest[[j]])) {
              subbox = update_box(current_box = box_new, j = j, lower = NULL,
                upper = NULL, val = setdiff(box_new$levels[[j]], cat),
                complement = FALSE)
              size = (1/private$param_set$nlevels[[j]])/private$subbox_relsize
              eval = private$evaluate_box(box = subbox)
              resrow = data.table(var = j, lower = NA, upper = NA, val = cat,
                impurity = eval["impurity"], coverage = eval["coverage"], size = size)
              res = rbind(res, resrow)
            }
          } else {

            # boxsubset = box_new$clone()$subset(setdiff(vars_diff, j))
            inbox = identify_in_box(box_new, private$obsdata)
            boxdata = copy(private$obsdata)[(inbox),]

            # get quantile
            lower = round(boxdata[, lapply(.SD, quantile, prob = private$subbox_relsize),  .SDcols = j])[[1]]
            upper = round(boxdata[, lapply(.SD, quantile, prob = 1-private$subbox_relsize),  .SDcols = j])[[1]]

            selection = c("lower", "upper")

            # get next larger/lower value
            idl = which(boxdata[[j]] > lower)
            if (length(idl) > 0) {
              lower = min(boxdata[idl, j, with = FALSE])
            } else {
              selection = selection[selection != "lower"]
            }
            idu = which(boxdata[[j]] < upper)
            if (length(idu) > 0) {
              upper = max(boxdata[idu, j, with = FALSE])
            } else {
              selection = selection[selection != "upper"]
            }

            # set manually to x_interest value such that x_interest is covered!
            lower = min(lower, private$x_interest[[j]])
            upper = max(upper, private$x_interest[[j]])

            for (l in selection) {
              bound = switch(l,
                "lower" = lower,
                "upper" = upper)
              if (l == "lower") {
                if (box_new$lower[[j]] == lower) {
                  next
                }
                subbox = update_box(current_box = box_new, j = j, lower = bound)
                size =  (subbox$lower[[j]] - box_new$lower[[j]])/private$lookup_sizes[[j]]
              } else {
                if (box_new$upper[[j]] == upper) {
                  next
                }
                subbox = update_box(current_box = box_new, j = j, upper = bound)
                size = (box_new$upper[[j]]- subbox$upper[[j]])/private$lookup_sizes[[j]]
              }
              eval = private$evaluate_box(subbox)
              resrow = data.table(var = j, lower = NA, upper = NA, val = NA,
                impurity = eval["impurity"], coverage = eval["coverage"], size = size)
              resrow[, (l) := bound]
              res = rbind(res, resrow)
            }
          }
          return(res)
        })
        res_table = rbindlist(a)

        if (nrow(res_table) == 0) break

        res_table$mode = "peeling"

        # #remove the categories with coverage == 0
       #  res_table = res_table[coverage != 0, ]

        # get best (low impurity, high coverage)
        best = res_table[order(impurity, -coverage)[1]]
        box_new = update_box(current_box = box_new, j = best$var, lower = best[["lower"]],
          upper = best[["upper"]], val = setdiff(box_new$levels[[best$var]], best[["val"]]), complement = FALSE)

        ## Save info in history
        history = rbind(history, best)
        private$i = private$i+1L
        if (!private$quiet) {
          message(paste("peeling iteration", private$i, "with peeling variable", best$var, "and impurity = ", best$impurity))
        }
        if (best$impurity == 0) {
          heterogeneous = FALSE
        }
      }

      private$i = 0L
      ### PASTING ###
      homogeneous = TRUE

      # Main algorithm for removing boxes
      while (homogeneous) {
        # evaluate current best box
        # private$evalcurrent = private$evaluate_box(box_new)

        a = lapply(sample(vars_diff), FUN = function(j) {
          res = data.table(var = character(), lower = numeric(), upper = numeric(),
            val = character(), impurity = numeric(), coverage = numeric(), size = numeric())
          if (box_new$is_categ[[j]]) {
            res = data.table()
            for (cat in setdiff(private$box_largest$params[[j]]$levels, box_new$params[[j]]$levels)) {
              subbox = update_box(current_box = box_new, j = j, lower = NULL,
                upper = NULL, val = c(box_new$levels[[j]], cat),
                complement = FALSE)
              size = (1/private$param_set$nlevels[[j]])/private$subbox_relsize
              eval = private$evaluate_box(box = subbox)
              resrow = data.table(var = j, lower = NA, upper = NA, val = cat,
                impurity = eval["impurity"], coverage = eval["coverage"], size = size)
              res = rbind(res, resrow)
            }
          } else {

            selection = vector()

            if (box_new$lower[[j]] != private$box_largest$lower[[j]]) selection = c(selection, "lower")
            if (box_new$upper[[j]] != private$box_largest$upper[[j]]) selection = c(selection, "upper")

            if (length(selection) > 0) {
              boxsubset = box_new$clone()$subset(setdiff(vars_diff, j))
              inbox = identify_in_box(boxsubset, private$obsdata)
              boxdata = copy(private$obsdata)[(inbox),]

              # number of observations to add
              num = max(round(sum(identify_in_box(box = box_new, data = private$obsdata))*private$subbox_relsize), 1)

              if ("lower" %in% selection) {
              # get next lower num observations
                boxdatalower = boxdata[which(boxdata[[j]] < box_new$lower[[j]]), ]
                if (nrow(boxdatalower) > 0) {
                  setorderv(boxdatalower, j, order = -1)
                  lower = min(head(boxdatalower, n = num)[[j]])
                } else {
                  selection = selection[selection != "lower"]
                }
              }

              if ("upper" %in% selection) {
                # get next upper num observations
                boxdataupper = boxdata[which(boxdata[[j]] > box_new$upper[[j]]), ]
                if (nrow(boxdataupper) > 0) {
                  setorderv(boxdataupper, j)
                  upper = max(head(boxdataupper, n = num)[[j]])
                } else {
                  selection = selection[selection != "upper"]
                }
              }

              for (l in selection) {
                bound = switch(l,
                  "lower" = lower,
                  "upper" = upper)
                if (l == "lower") {
                  if (box_new$lower[[j]] == lower) next
                  subbox = update_box(current_box = box_new, j = j, lower = bound)
                  size =  (box_new$lower[[j]] - subbox$lower[[j]])/private$lookup_sizes[[j]]
                } else {
                  if (box_new$upper[[j]] == upper) next
                  subbox = update_box(current_box = box_new, j = j, upper = bound)
                  size = (subbox$upper[[j]] - box_new$upper[[j]])/private$lookup_sizes[[j]]
                }
                eval = private$evaluate_box(subbox)
                resrow = data.table(var = j, lower = NA, upper = NA, val = NA,
                  impurity = eval["impurity"], coverage = eval["coverage"], size = size)
                resrow[, (l) := bound]
                res = rbind(res, resrow)
              }
            }

          }
          return(res)
        })

        res_table = rbindlist(a)

        if (nrow(res_table) == 0) break

        res_table$mode = "pasting"

        # #remove the categories with coverage == 0
        # res_table = res_table[coverage != 0, ]

        # get best (low impurity, high coverage)
        best = res_table[order(impurity, -coverage)[1]]

        if (best$impurity > 0) {
          break
        }

        box_new = update_box(current_box = box_new, j = best$var, lower = best[["lower"]],
          upper = best[["upper"]], val = best[["val"]], complement = TRUE)

        ## Save info in history
        history = rbind(history, best)
        private$i = private$i+1L
        if (!private$quiet) {
          message(paste("pasting iteration", private$i, "with pasting variable", best$var,"and impurity = ", best$impurity))
        }
        if (best$impurity == 0) {
          heterogeneous = FALSE
        }
      }


      private$.history = history
      return(box_new)
    },
    evaluate_dataset = function(evaldt) {
      impurity = sum(evaldt$positive == 0)/nrow(evaldt)
      coverage = nrow(evaldt)/nrow(private$obsdata)
      return(c(impurity = impurity, coverage = coverage))
    },
    evaluate_box = function(box) {
      inbox = identify_in_box(box, private$obsdata[, !"positive"])
      evaldt = private$obsdata[inbox,]
      impurity = sum(evaldt$positive == 0)/sum(inbox)
      coverage = sum(inbox)/nrow(private$obsdata)

      return(c(impurity = impurity, coverage = coverage))
    },
    declutter_searchspace = function(searchspace, var) {
      s = searchspace[[var]]
      searchspace[[var]] = s[lapply(s, length) > 0]
      searchspace = searchspace[lapply(searchspace, length) > 0]
      return(searchspace)
    },
    print_parameters = function() {
      cat(" - subbox_relsize: ", private$subbox_relsize, "\n")
    }
  )
)
