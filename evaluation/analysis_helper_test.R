statistical_analysis = function (methods = c("maxbox", "prim", "anchors", "maire"),
  quality_measures = c("coverage_train", "coverage_sampled", "coverage_levelset_train", "coverage_levelset_sampled",
    "precision_train", "precision_sampled")) {

  data_set_names = c("diabetes", "tic_tac_toe", "cmc", "vehicle", "no2", "plasma_retinol")

  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, datanam) %>% collect()
    DBI::dbDisconnect(con)

    if ("coverage_L_train" %in% quality_measures) {
      res = res %>% rename(coverage_L_train = coverage_levelset_train)
    }

    if ("coverage_L_sampled" %in% quality_measures) {
      res = res %>% rename(coverage_L_sampled = coverage_levelset_sampled)
    }

    res = res %>%
      filter(algorithm %in% methods) %>%
      mutate(model_name = recode(model_name, ranger = "randomforest",
        logistic_regression = "logreg", neural_network = "neuralnet"),
        algorithm = factor(algorithm, levels = methods))%>%
      filter(model_name != "neuralnet")
    # select(algorithm, quality_measure)

    return(res)
  })

  names(aggrres) = data_set_names
  ll = dplyr::bind_rows(aggrres, .id = "dataset")
  ll$algorithm = factor(ll$algorithm, methods)


  ########### RQ 1 #############

  rq1 = ll %>% filter(postprocessed == 0 & datastrategy == "traindata")

  combinations = t(combn(methods, 2))
  colnams = apply(combinations, MARGIN = 1, function(comb) {
    paste(comb[[1]], "=", comb[[2]])
  })

  no = nrow(combinations)*length(quality_measures)
  print(no)
  alpha = 0.05/no

  restable = setNames(data.frame(matrix(ncol = nrow(combinations)+1, nrow = 0)), c("measure", colnams))

  for (measure in quality_measures) {

    rq1a = rq1 %>%
      pivot_wider(names_from = algorithm, values_from = measure,
        id_cols = c("problem", "model_name", "id_x_interest", "datastrategy", "postprocessed")) %>%
      na.omit()

    testresults = apply(combinations, MARGIN = 1, function(comb) {
      print(comb)
      if (all(rq1a[[comb[[1]]]] == rq1a[[comb[[2]]]])) {
        t = 1
      } else {
        t = round(wilcox.test(x = rq1a[[comb[[1]]]], y = rq1a[[comb[[2]]]], alternative = "two.sided",
          paired = TRUE, exact = FALSE, conf.int = FALSE)$p.value, 3)
      }
      if (t <= alpha) {
        t = paste0('\\textbf{', t,'}')
      }
      return(t)
    }
    )

    resrow = setNames(data.frame(measure, t(testresults)), c("measure", colnams))
    restable = rbind(restable, resrow)
  }

  restable$measure = gsub(pattern = "_", replacement = "\\\\_", x = restable$measure)

  message("RQ 1")
  print(xtable(restable), sanitize.text.function=I, include.rownames = FALSE)


  ################ RQ 2 ##############

  rq2 = ll %>% filter(postprocessed == 0)

  no = (length(methods)+1)*length(quality_measures)
  print(no)
  alpha = 0.05/no

  restable2 = setNames(data.frame(matrix(ncol = length(methods)+2, nrow = 0)), c("measure", "overall", methods))

  for (measure in quality_measures) {

    rq2a = ll %>%
      pivot_wider(names_from = datastrategy, values_from = measure,
        id_cols = c("problem", "model_name", "id_x_interest", "algorithm", "postprocessed")) %>%
      na.omit()

    overall = round(wilcox.test(x = rq2a[["traindata"]], y = rq2a[["sampled"]],  exact = FALSE,
      paired = TRUE, conf.int = FALSE, alternative = "less")$p.value, 3)

    testresults2 = sapply(methods, function(method) {

      x = rq2a[rq2a$algorithm == method,]$traindata
      y = rq2a[rq2a$algorithm == method,]$sampled
      if (all(x == y)) {
        t = 1
      } else {
        t = round(wilcox.test(x = x, y = y,  exact = FALSE,
          paired = TRUE, conf.int = FALSE, alternative = "less")$p.value, 3)
      }
      if (t <= alpha) {
        t = paste0('\\textbf{', t,'}')
      }
      return(t)
    }
    )

    resrow = setNames(data.frame(measure, overall, t(testresults2)), c("measure", "overall", methods))
    restable2 = rbind(restable2, resrow)
  }

  restable2$measure = gsub(pattern = "_", replacement = "\\\\_", x = restable2$measure)

  message("RQ 2")
  print(xtable(restable2), sanitize.text.function=I, include.rownames = FALSE)

  ################ RQ 3 ##############

  rq3 = ll

  no = ((length(methods)+1)*2)*length(quality_measures)
  print(no)
  alpha = 0.05/no

  restable3 = setNames(data.frame(matrix(ncol = length(quality_measures)+1, nrow = 2*length(methods)+2)), c("method", quality_measures))
  restable3$method = c("\\textbf{traindata}", methods, "\\textbf{sampled}", methods)

  for (measure in quality_measures) {

    testresults3 = sapply (c("traindata", "sampled"), function(strategy) {

      rq3a = ll %>%
        filter(datastrategy == strategy) %>%
        pivot_wider(names_from = postprocessed, values_from = measure,
          id_cols = c("problem", "model_name", "id_x_interest", "algorithm")) %>%
        na.omit()

      overall = round(wilcox.test(x = rq3a[["0"]], y = rq3a[["1"]],  exact = FALSE,
        paired = TRUE, conf.int = FALSE, alternative = "less")$p.value, 3)

      testresults3a = sapply(methods, function(method) {

        x = rq3a[rq3a$algorithm == method,]$`0`
        y = rq3a[rq3a$algorithm == method,]$`1`

        if (all(x == y)) {
          t = 1
        } else {
          t = round(wilcox.test(x = x, y = y,  exact = FALSE,
            paired = TRUE, conf.int = FALSE, alternative = "less")$p.value, 3)
        }
        if (t <= alpha) {
          t = paste0('\\textbf{', t,'}')
        }
        return(t)
      }
      )
      return(c(overall, testresults3a))
    })
    rescol = c(testresults3[, 1], testresults3[, 2])
    restable3[[measure]] = rescol
  }
  names(restable3) = gsub(pattern = "_", replacement = "\\\\_", x = names(restable3))

  message("RQ 3")
  print(xtable(restable3), sanitize.text.function=I, include.rownames = FALSE)
}
