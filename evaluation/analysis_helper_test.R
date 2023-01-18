statistical_analysis = function (methods = c("maxbox", "prim", "anchors", "maire"),
  quality_measure = c("coverage_train"),
  postprocessed = c(0, 1), datastrategy = c("traindata", "sampled")) {

  data_set_names = c("diabetes", "tic_tac_toe", "cmc", "vehicle", "no2", "plasma_retinol")

  checkmate::assert_names(datastrategy, subset.of = c("traindata", "sampled"))

  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, datanam) %>% collect()
    DBI::dbDisconnect(con)

    pp = postprocessed
    ds = datastrategy

    if ("coverage_L_train" %in% quality_measure) {
      res = res %>% rename(coverage_L_train = coverage_levelset_train)
    }

    if ("coverage_L_sampled" %in% quality_measure) {
      res = res %>% rename(coverage_L_sampled = coverage_levelset_sampled)
    }

    res = res %>%
      filter(postprocessed %in% pp) %>%
      filter(datastrategy %in% ds) %>%
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

  browser()

  rq1 = ll %>%
    pivot_wider(names_from = algorithm, values_from = quality_measure,
      id_cols = c("problem", "model_name", "id_x_interest", "datastrategy", "postprocessed")) %>%
    na.omit() %>%
    filter(postprocessed == 0 & datastrategy == "traindata")

  friedman.test(as.matrix(rq1[, c("maire", "maxbox", "prim", "anchors")]))
  # unterscheiden sich
  lookup = expand_grid(a = methods, b = methods)
  t = apply(lookup, MARGIN = 1L, FUN = function(row) {
    wilcox.test(x = rq1[[row[[1]]]], y = rq1[[row[[2]]]], alternative = "greater",
      paired = TRUE, exact = FALSE, correct = FALSE, conf.int = FALSE)$p.value
  })
  # anchors zu anderen gleich, maxbox/prim/maire unterscheiden sich
  lookup$p = t
  message("H0: approaches perform equally well")
  print(dcast(lookup, a ~ b))

  rq2 = ll %>%
    pivot_wider(names_from = datastrategy, values_from = quality_measure,
      id_cols = c("problem", "model_name", "id_x_interest", "algorithm", "postprocessed")) %>%
    na.omit() %>%
    filter(postprocessed == 0)

  message("H0: traindata < sampled")
  print(wilcox.test(x = rq2[["traindata"]], y = rq2[["sampled"]],  exact = FALSE,
    paired = TRUE, correct = FALSE, conf.int = FALSE, alternative = "greater")$p.value)
  # insgesamt lohnts sich schon!

  t2 = lapply(methods, function(method) {
  wilcox.test(x = rq2[rq2$algorithm == method,]$traindata,
    y = rq2[rq2$algorithm == method,]$sampled,  exact = FALSE,
    paired = TRUE, correct = FALSE, conf.int = FALSE, alternative = "greater")$p.value
  })

  cbind(methods, t2)
  # für maxbox, prim bringts was, für anchors und maire weniger!

  rq3 = ll %>%
    pivot_wider(names_from = postprocessed, values_from = quality_measure,
      id_cols = c("problem", "model_name", "id_x_interest", "algorithm", "datastrategy")) %>%
    na.omit()

  message("H0: no postprocessing < postprocessing")
  print(wilcox.test(x = rq3[["0"]], y = rq3[["1"]],  exact = FALSE,
    paired = TRUE, correct = FALSE, conf.int = FALSE, alternative = "greater")$p.value)
  t3 = lapply(methods, function(method) {
    data = rq3 %>% filter(datastrategy == "sampled")
    wilcox.test(x = data[data$algorithm == method,"0"][[1]],
      y = data[data$algorithm == method,"1"][[1]],  exact = FALSE,
      paired = TRUE, correct = FALSE, conf.int = FALSE, alternative = "greater")$p.value
  })
  message("H0: no postprocessing < postprocessing, sampled")
  print(cbind(methods, t3))

  t3 = lapply(methods, function(method) {
    data = rq3 %>% filter(datastrategy == "traindata")
    wilcox.test(x = data[data$algorithm == method,"0"][[1]],
      y = data[data$algorithm == method,"1"][[1]],  exact = FALSE,
      paired = TRUE, correct = FALSE, conf.int = FALSE, alternative = "greater")$p.value
  })
  message("H0: no postprocessing < postprocessing, traindata")
  print(cbind(methods, t3))

}


create_test_df = function(data, method = c("nice", "moc")) {
    browser()
    testdata = data %>% filter(algorithm %in% method)
    testdata$algorithm = factor(testdata$algorithm, labels = method, levels = method)
    if (orientation == "model") {
      stratif = unique(testdata$model_name)
      names(testdata)[which(names(testdata) == "model_name")] = "stratif"
      lookup = expand.grid(stratif, unique(testdata$objective)[-4])
      names(lookup) = c("model_name", "objective")
    } else if (orientation == "dataset") {
      stratif = unique(testdata$dataset)
      names(testdata)[which(names(testdata) == "dataset")] = "stratif"
      lookup = expand.grid(stratif, unique(testdata$objective)[-4])
      names(lookup) = c("dataset", "objective")
    }
    t = apply(lookup, MARGIN = 1L, FUN = function(row) {
      wilcox.test(value ~ algorithm, data = testdata %>% filter(stratif == row[[1]], objective == row[[2]]),
        exact = FALSE, correct = FALSE, conf.int = FALSE)$p.value
    })
    lookup$p.signif = ifelse(t > 0.1, "ns", ifelse(t > 0.05, ".", ifelse(t > 0.01, "*", ifelse(t > 0.001, "**", "***"))))
    lookup$p = ""
    lookup$group1 = method[1]
    lookup$group2 = method[2]
    lookup$.y. = "value"
    lookup = tibble::as_tibble(lookup)
  }
