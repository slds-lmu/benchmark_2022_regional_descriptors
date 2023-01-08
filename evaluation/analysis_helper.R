compare_methods = function (methods = c("maire", "maxbox", "prim"), postprocessed = c(0), orientation = "model", savepdf = FALSE) {

  data_set_names = c("diabetes", "tic_tac_toe", "cmc", "vehicle", "no2") # fixme add: plasma_retinol
  checkmate::assert_names(orientation, subset.of = c("model", "dataset"))

  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {

        con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, datanam) %>% collect()
    DBI::dbDisconnect(con)

    res_long = res %>%
      filter(postprocessed %in% postprocessed) %>%
      filter(algorithm %in% methods) %>%
      mutate(model_name = recode(model_name, ranger = "randomforest",
        logistic_regression = "logreg", neural_network = "neuralnet")) %>%
      pivot_longer(c(locality:maximality), names_to = "quality") %>%
      mutate(quality = factor(quality, levels = c("locality", "coverage_train", "coverage_sampled",
        "precision_train", "precision_sampled", "maximality")))

    if (length(postprocessed) == 2) {
      res_long = res_long %>%
        mutate(algorithm = paste(algorithm, postprocessed, sep = "_"))
    }

    return(res_long)
  })

  names(aggrres) = data_set_names
  ll = dplyr::bind_rows(aggrres, .id = "dataset")

  ll = ll %>%
    group_by(problem, model_name, id_x_interest) %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup()


  # if (test) {
  #   create_test_df = function(data, subset = c("nice", "moc")) {
  #     testdata = data %>% filter(algorithm %in% subset)
  #     testdata$algorithm = factor(testdata$algorithm, labels = subset, levels = subset)
  #     if (orientation == "model") {
  #       stratif = unique(testdata$model_name)
  #       names(testdata)[which(names(testdata) == "model_name")] = "stratif"
  #       lookup = expand.grid(stratif, unique(testdata$objective)[-4])
  #       names(lookup) = c("model_name", "objective")
  #     } else if (orientation == "dataset") {
  #       stratif = unique(testdata$dataset)
  #       names(testdata)[which(names(testdata) == "dataset")] = "stratif"
  #       lookup = expand.grid(stratif, unique(testdata$objective)[-4])
  #       names(lookup) = c("dataset", "objective")
  #     }
  #     t = apply(lookup, MARGIN = 1L, FUN = function(row) {
  #       wilcox.test(value ~ algorithm, data = testdata %>% filter(stratif == row[[1]], objective == row[[2]]),
  #         exact = FALSE, correct = FALSE, conf.int = FALSE)$p.value
  #     })
  #     lookup$p.signif = ifelse(t > 0.1, "ns", ifelse(t > 0.05, ".", ifelse(t > 0.01, "*", ifelse(t > 0.001, "**", "***"))))
  #     lookup$p = ""
  #     lookup$group1 = subset[1]
  #     lookup$group2 = subset[2]
  #     lookup$.y. = "value"
  #     lookup = tibble::as_tibble(lookup)
  #   }
  #   lookup1 = create_test_df(ll)
  #   lookup2 = create_test_df(ll, subset = c("nice", "whatif"))
  # }

  ll$dataset = factor(ll$dataset, levels = data_set_names, labels = data_set_names)
  n_colors = length(unique(ll$algorithm))

  plt = ggplot(ll) +
    geom_boxplot(aes(x = algorithm, y = value, fill = algorithm), show.legend = FALSE) +
    ylab("value") +
    xlab("quality measure")
  if (orientation == "model") {
    plt = plt +  facet_grid(model_name ~ quality, scales = "free")
    height = 7
  } else if (orientation == "dataset") {
    plt = plt + facet_grid(dataset ~ quality, scales = "free")
    height = 7.5
  }
  # mindata = left_join(x = minid, y = ll, by = c("model_name", "algorithm", "id", "dataset"))
  plt = plt +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = n_colors, name = "Paired")) +
    theme_bw() +
    theme(
      #strip.text = element_text(size = 10, margin = margin(t = 2.5, r = 2.5,
      #  b = 2.5, l = 2.5, unit = "pt")),
      axis.text.x = element_text(size = 10, angle = 90),
      axis.text.y = element_text(size = 8),
      panel.spacing = unit(2, "pt")
    )
  # colorlines = "tan4"
  plt = plt +
    geom_line(aes(x = algorithm, y = value, group=group_id), alpha=0.3, color = "grey20")

  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    height = 3.5
    ggsave(filename = file.path(fig.path, paste0(paste("comparison_methods",
      orientation, paste0(postprocessed, collapse = ""), sep = "_"), ".pdf")), plot = plt, width = 7,
      height = height) # 5.5, 3.8
  }

  return(plt)
}

comparison_table = function(methods = c("maire", "maxbox", "prim"), orientation = "model", savextable = FALSE) {

  data_set_names = c("diabetes", "tic_tac_toe", "cmc", "vehicle", "no2") #<FIXME>plasma_retinol
  checkmate::assert_names(orientation, subset.of = c("model", "dataset"))

  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {

    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, datanam) %>% collect()
    DBI::dbDisconnect(con)

    res_long = res %>%
      filter(algorithm %in% methods) %>%
      mutate(model_name = recode(model_name, ranger = "randomforest",
        logistic_regression = "logreg", neural_network = "neuralnet")) %>%
      pivot_longer(c(locality:precision_sampled), names_to = "quality") %>%
      mutate(quality = factor(quality, levels = c("locality", "coverage_train", "coverage_sampled",
        "precision_train", "precision_sampled", "maximality")))
    return(res_long)
  })

  names(aggrres) = data_set_names
  ll = dplyr::bind_rows(aggrres, .id = "dataset")

  ll = ll %>% group_by(
    model_name, postprocessed, algorithm, quality
  ) %>%
    summarize(value = round(mean(value, na.rm = TRUE), 3)) %>%
     # sd = round(sd(value, na.rm = TRUE), 3)) %>%
    #mutate(value = paste0(mean, " (", sd, ")")) %>%
    # select(-mean, -sd) %>%
    ungroup %>%
    pivot_wider(names_from = c("quality", "postprocessed"), values_from = "value") %>%
    select(model_name, algorithm, starts_with(c("locality", "coverage_train", "coverage_sampled",
      "precision_train", "precision_sampled")))


  rle.lengths <- rle(ll[[1]])$lengths
  first <- !duplicated(ll[[1]])
  ll[[1]][!first] <- ""
  ll[[1]][first] <-
    paste0("\\midrule\\multirow{", rle.lengths, "}{*}{\\textbf{", ll[[1]][first], "}}")

  print(xtable(ll, digits = 3, # digits = c(0, 0, 0, 3, 1, 0, 6), # first zero "represents" row numbers which we skip later
    align = "lllrr|rr|rr|rr|rr",  # align and put a vertical line (first "l" again represents column of row numbers)
    label = "testTable"),
    size = "footnotesize", #Change size; useful for bigger tables "normalsize" "footnotesize"
    include.rownames = FALSE, #Don't print rownames
    include.colnames = FALSE, #We create them ourselves
    caption.placement = "top", #"top", NULL
    hline.after=NULL, #We don't need hline; we use booktabs
    floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
    sanitize.text.function = force, # Important to treat content of first column as latex function
    add.to.row = list(pos = list(-1,
      3,
      nrow(ll)),
      command = c(paste("\\toprule \n",  # NEW row
        "\\multicolumn{2}{c}{} & \\multicolumn{2}{c}{locality} & \\multicolumn{2}{c}{coverage_{train}} & \\multicolumn{2}{c}{coverage_{samp}} & \\multicolumn{2}{c}{precision_{train}} & \\multicolumn{2}{c}{precision_{samp}} \\\\\n",
        "\\cmidrule(l){3-4} \\cmidrule(l){5-6} \\cmidrule(l){7-8} \\cmidrule(l){9-10} \\cmidrule(l){11-12}\n",
        " & & 0 & 1 & 0 & 1 & 0 & 1 & 0 & 1 & 0 & 1 \\\\\n", # NEW row
        "\\midrule \n"
      ),
        paste("\\cmidrule(l){3-4} \\cmidrule(l){5-6} \\cmidrule(l){7-8} \\cmidrule(l){9-10} \\cmidrule(l){11-12}\n" # we may also use 'pos' and 'command' to add a midrule
        ),
        paste("\\bottomrule \n"  # paste is used as it is more flexible regarding adding lines
        )
      )
    )
  )

}


