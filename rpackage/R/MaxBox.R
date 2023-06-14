#' MaxBox
#'
#' @description Maximum Box approach of Eckstein.
#'
#' @export
MaxBox = R6::R6Class("MaxBox", inherit = RegDescMethod,
  public = list(
    #' @param predictor (\link[iml]{Predictor})\cr
    #'   The object (created with `iml::Predictor$new()`) holding the machine
    #'   learning model and the data.
    #' @param strategy (`character(1)`)\cr
    #' Either `traindata` using training data or `sampled` using newly sampled data in
    #' ICE curve identified box.
    #' @param num_sampled_points (`numeric(1)`)\cr Only considered if `strategy = 'sampled'`.
    #' The number of samples randomly drawn at the beginning.
    #' @param efficient (`logical(1)`)\cr Whether the efficient computation
    #' proposed by Eckstein et al. should be used. Default TRUE.
    #' @param quiet (`logical(1)`)\cr Should information about the optimization status be hidden? Default is FALSE.
    #' @return (RegDesc) Hyperbox
    initialize = function(predictor, strategy = "traindata", num_sampled_points = 500L, efficient = TRUE,  quiet = FALSE) {
      super$initialize(predictor, quiet)
      assert_character(strategy, len = 1L)
      assert_names(strategy, subset.of = c("traindata", "sampled"))
      assert_logical(efficient)

      private$strategy = strategy
      if (strategy == "sampled") {
        assert_numeric(num_sampled_points, lower = 1, finite = TRUE, len = 1L, null.ok = FALSE)
        private$num_sampled_points = num_sampled_points
      }

      private$efficient = efficient

    }),
  private = list(
    strategy = NULL,
    num_sampled_points = NULL,
    efficient = NULL,
    categorylist = NULL,
    expldata = NULL,
    expldata_colnames = NULL,
    explx_interest = NULL,
    positive = NULL,
    plotdata = NULL, # <FIXME>: remove!!
    run = function(){
      if (is.null(private$obsdata)) {
        # Get data (either training or newly sampled depending on strategy)
        private$obsdata = sampling(predictor = private$predictor, x_interest = private$x_interest,
          fixed_features = private$fixed_features, desired_range = private$desired_range,
          param_set = private$param_set, num_sampled_points = private$num_sampled_points,
          strategy = private$strategy)
      }
      private$obsdata = rbind(private$x_interest, private$obsdata[, private$predictor$data$feature.names, with = FALSE])

      # get positive and negative samples
      private$.calls_fhat = private$.calls_fhat + nrow(private$obsdata)
      private$positive = ifelse(predict_range(private$predictor,
        newdata = private$obsdata,
        range = private$desired_range) == 1, TRUE, FALSE)
      assert_true(length(private$positive) == nrow(private$obsdata))

      if (!is.null(private$fixed_features))  {
        private$obsdata = private$obsdata[, (private$fixed_features) := NULL]
        x_interest = copy(private$x_interest)[, (private$fixed_features) := NULL]
      } else {
        x_interest = private$x_interest
      }

      # transform categorical features
      # 1-hot encoded
      expldata = transform_for_explanation(predictor = private$predictor,
        data = private$obsdata, x_interest = x_interest, version = 2)
      assert_true(length(private$positive) == nrow(expldata$expldata))

      private$explx_interest = expldata$explx_interest
      private$expldata = expldata$expldata
      private$expldata = private$expldata[, positive:= private$positive]
      private$expldata$x_interest = FALSE
      private$expldata$x_interest[1] = TRUE
      private$categorylist = attr(expldata, "categorylist")
      rm(expldata)

      # initialize P = P0 = min(min X+, max X+, min X+, max X+)
      posmin = private$expldata[positive == 1, lapply(.SD, min), .SDcols = names(private$expldata)]
      posmax = private$expldata[positive == 1, lapply(.SD, max), .SDcols = names(private$expldata)]

      inbox = private$expldata[, all(between(.SD, lower = posmin, upper = posmax)), by = seq_len(nrow(private$expldata)),]$seq_len
      private$expldata = private$expldata[inbox,]
      private$obsdata = private$obsdata[inbox,]

      if (ncol(private$expldata) > 500L) {
        stop(sprintf("%s subproblems were detected that need to be inspected per iteration. The MaxBox approach is not suitable for this number.", ncol(private$expldata)))
      }

      # if (TRUE) {
      #   private$plotdata = cbind(private$expldata, class = private$positive)
      # }

      private$.history = data.table(iteration=numeric(), cov = numeric(),
        subprobleft = numeric())

      ###### Branch-and-bound #####
      # 1. store upper bound (for/and) best solution found so far
      problem_upper_bound = -Inf
      # heuristic_solution = heuristic_solve(problem)
      # problem_upper_bound = evaluate(heuristic_solution)
      # current_optimum = heuristic_solution
      current_optimum = NULL
      # 2. Initialize a queue
      candidate_queue = list()
      i = 0L
      inbox = seq_len(nrow(private$expldata))
      # 3. Loop until queue empty
      if (any(!private$expldata$positive)) {
        while (length(candidate_queue) > 0 | i == 0L) {
          if (problem_upper_bound == -Inf) {
            criterion = "upper_bound"
          } else {
            criterion = "frac_pos_neg"
          }
          # take a node of queue
          if (i > 0L) {
            nodeid = private$get_best(candidate_queue, criterion = criterion)
            node = candidate_queue[[nodeid]]
            candidate_queue = candidate_queue[-nodeid]
          } else {
            splitcriteria = private$get_splitcriteria(inbox = inbox)
            node = list(inbox = inbox, upper_bound = splitcriteria$upper_bound,
              child_inbox = splitcriteria$child_inbox, depth = 0)
          }
          # plot:
          # private$plot_box(node)
          # generate new candidates
          children =  private$get_subproblems(node)
          # if new child boxes that include x_interest found --> inspect
          if (length(children) > 0) {
            # private$plot_box(children[[1]], negsample = node$negsample)
            # private$plot_box(children[[2]], negsample = node$negsample)
            # check if any node homogeneous
            feas = sapply(children, function(ch) all(ch[["feasible"]]))
            if (any(feas)) {
              # if the case, get no. positive samples
              obj = sapply(children[feas], function(ch) ch[["upper_bound"]])
              # if max objective value higher than current best --> current node best
              if (max(obj) > problem_upper_bound) {
                current_optimum = children[feas][[which.max(obj)]]
                problem_upper_bound = max(obj)
                cutid = which(sapply(candidate_queue, function(ch) ch[["upper_bound"]]) <= problem_upper_bound)
                if (length(cutid) > 0) {
                  candidate_queue = candidate_queue[-cutid]
                }
                private$.history = rbind(private$.history, data.table(iteration = i,
                  cov = current_optimum$upper_bound, subprobleft = length(candidate_queue)))
              }
              # remove feasible nodes
              children = children[!feas]
            }
            # if not homogeneous
            if (any(!feas)) {
              # update upper_bound
              obj = sapply(children, function(ch) ch[["upper_bound"]])
              # check child has a upper bound > current best upper bound
              promising = sapply(children, function(ch) ch[["upper_bound"]] > problem_upper_bound)
              children = children[promising]
              # add promising children to queue
              if (length(children) > 0) {
                candidate_queue = c(candidate_queue, children)
              }
            }
          }
          i = i+1L
          if (!private$quiet) {
            message(paste("iteration", i, "with", length(candidate_queue), "candidates alive"))
          }
        }
      }
      else {
        splitcriteria = private$get_splitcriteria(inbox = inbox)
        current_optimum = list(inbox = inbox, upper_bound = splitcriteria$upper_bound,
          child_inbox = splitcriteria$child_inbox, depth = 0)

      }
      if (is.null(current_optimum)) current_optimum = node
      param_set = make_param_set(private$obsdata[current_optimum$inbox,])
      return(param_set)
    },
    # check feasibility --> p. 290
    # additionally, we require x_interest to be in there!
    check_feasible = function(inbox) {
      c2 = c("|pos| > 0" = any(private$expldata[inbox, positive]))
      c3 = c("|neg| = 0" = !any(!private$expldata[inbox, positive]))
      c4 = c("x_interest inbox" = any(private$expldata[inbox, x_interest]))
      return(c(c2, c3, c4))
    },
    # get best candidate id
    get_best = function(candidate_queue, criterion = c("upper_bound", "no_neg", "frac_pos_neg")) {
      if (criterion == "upper_bound") {
        quality = sapply(candidate_queue, function(cq) cbind(cq[["depth"]], cq[["upper_bound"]]))
        best = order(quality[1,], quality[2,], decreasing = TRUE)[1]
      } else if (criterion == "no_neg") {
        quality = sapply(candidate_queue, function(cq) private$evaluate(cq)[criterion])
        best = which.min(quality)
      } else if (criterion == "frac_pos_neg") {
        quality = sapply(candidate_queue, function(cq) private$evaluate(cq)[criterion])
        best = which.max(quality)
      } else {
        stop("criterion for selecting a candidate unknown")
      }
    },
    # branching rule
    get_subproblems = function(node) {
      depth = node$depth + 1L
      children = lapply(node$child_inbox, function(child) {
        if (length(child) == 0) {
          return()
        } else {
          splitcrit = private$get_splitcriteria(child)
          splitcrit$feasible = private$check_feasible(splitcrit$inbox)
          splitcrit$depth = depth
          if (!all(splitcrit$feasible[c(1, 3)])) {
            return()
          }
          return(splitcrit)
        }
      })
      children[sapply(children, is.null)] <- NULL
      return(children)
    },
    evaluate = function(node) {
      inbox = node$inbox
      pos = sum(private$expldata[inbox, positive])
      neg = length(inbox) - pos
      bb = c(pos = pos,
        no_neg = neg,
        frac_pos_neg = pos/neg)
      return(bb)
    },

    get_splitcriteria = function(inbox) {
      if (all(private$expldata[inbox, positive])) {
        best_coverage_feasible = length(inbox)
        child_inbox = NULL
      } else {
        # posinbox = subset(private$expldata[inbox,], positive)
        neginbox = subset(private$expldata[inbox,], !positive)
        idsneg = inbox[!private$expldata$positive[inbox]]
        # if (length(idsneg) > 100) {
        #   idsneg = sample(idsneg, 10)
        # }

        compare = function(a, b, c) {
          if (c < b) {
            (a < b)
          } else if (c > b) {
            (a > b)
          } else {
            FALSE
          }
        }

        child_inbox = NULL
        best_coverage = Inf
        best_coverage_feasible = Inf

       for (i in idsneg) {
          vec = names(private$expldata)
          vec = vec[-c(length(vec)-1, length(vec))]
          overview = private$expldata[inbox, vec, with = FALSE]
          overview = overview[, Map(compare, .SD, private$expldata[i,vec, with = FALSE], private$explx_interest), .SDcols = vec]
          # maximum number of positive samples
          overview[, positive := private$expldata[inbox, positive]]
          coverage = colSums(subset(overview, positive))[-ncol(overview)]
          if (private$efficient) {
            no_inbox = colSums(overview)[-ncol(overview)]
            all_positive = ifelse(coverage/colSums(overview)[-ncol(overview)] == 1, 1, 0)
            # get upper bound
            if (any(all_positive == 1, na.rm = TRUE)) {
              coverage_feasible = coverage[order(all_positive, coverage, decreasing = TRUE)][[1]]
            } else {
              coverage_feasible = Inf
            }
            if (coverage_feasible < best_coverage_feasible) {
              best_coverage_feasible = coverage_feasible
            }
          } else {
            best_coverage_feasible = max(coverage)
          }
            # which samples per feature in box
            coverage = max(coverage)
            if (coverage < best_coverage) {
              overview[, positive := NULL]
              child_inbox = apply(overview, MARGIN = 2L, FUN = function(col) {
                a = inbox[which(col)]
              })
                best_coverage = coverage
            }
         }
        if (best_coverage_feasible == Inf) {
          best_coverage_feasible = 0
        }
      }
      return(list(inbox = inbox, child_inbox = child_inbox,
        upper_bound = best_coverage_feasible))
    },

    plot_box = function(node, negsample = NULL) {
      box = node$inbox
      p = ggplot(data = private$plotdata, aes(x1, x2, color = class)) +
        geom_point() +
        geom_vline(xintercept = box[[1,1]]) +
        geom_vline(xintercept = box[[4,1]]) +
        geom_hline(yintercept = box[[1,2]]) +
        geom_hline(yintercept = box[[4,2]]) +
        geom_point(x = private$x_interest$x1, y = private$x_interest$x2, color = "black")
      if (!is.null(negsample)) {
        p = p + geom_point(x = private$plotdata$x1[negsample], y = private$plotdata$x2[negsample], color = "darkblue")
      }
      if (!is.null(node$negsample)){
        p = p + geom_point(x = private$plotdata$x1[node$negsample], y = private$plotdata$x2[node$negsample], color = "gray")
      }
      print(p)
    },
    retransform_to_ps = function(box) {
      box = box[c(1, 4),]
      current_box = make_param_set(private$x_interest)
      for (fnam in private$predictor$data$feature.names) {
        if (fnam %in% private$fixed_features) next
        fcol = private$obsdata[[fnam]]
        j = which(private$predictor$data$feature.names == fnam)

        if (private$predictor$data$feature.types[fnam] == "numerical") {
          l_val = box[[1, fnam]]
          u_val = box[[2, fnam]]
          current_box = update_box(current_box, j = j, lower = l_val, upper = u_val)
        } else {
          if (!is.ordered(fcol)) {
            fcol = factor(fcol)
            lev = levels(fcol)
            if (length(unique(fcol)) == 2L) {
              val = unique(levels(fcol)[box[[fnam]]+1])
            } else {
              #<FIXME:> apply strategy!!
              colid = grepl(names(box), pattern = paste0(fnam, "_"))
              oneinc = as.logical(box[ , lapply(.SD, function(s) any(s == 1)), .SDcols = names(box)[colid]])
              val = sub(paste0(fnam, "_"), replacement = "", x = names(box)[which(colid)[oneinc]])
            }
          } else {
            val = private$categorylist[[fnam]][box[[1, fnam]]:box[[2, fnam]]]
          }
          current_box = update_box(current_box, j = j, val = val, complement = FALSE)
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
