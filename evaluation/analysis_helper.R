compare_methods = function (methods = c("maxbox", "prim", "anchors", "maire"),
    quality_measures = c("coverage_train", "coverage_sampled", "precision_train", "precision_sampled"), # coverage_leverset_train, coverage_levelset_sampled
    postprocessed = c(0), datastrategy = c("traindata"), orientation = NULL, savepdf = FALSE) {

  data_set_names = c("diabetes", "tic_tac_toe", "cmc", "vehicle", "no2", "plasma_retinol")
  if (!is.null(orientation)) {
    checkmate::assert_names(orientation, subset.of = c("model", "dataset"))
  }
  checkmate::assert_names(datastrategy, subset.of = c("traindata", "sampled"))

  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, datanam) %>% collect()
    DBI::dbDisconnect(con)

    pp = postprocessed
    ds = datastrategy

    if ("coverage_L_train" %in% quality_measures) {
      res = res %>% rename(coverage_L_train = coverage_levelset_train,
                                     coverage_L_sampled = coverage_levelset_sampled)
    }


    res = res %>%
      filter(postprocessed %in% pp) %>%
      filter(datastrategy %in% ds) %>%
      filter(algorithm %in% methods) %>%
      mutate(model_name = recode(model_name, ranger = "randomforest",
        logistic_regression = "logreg", neural_network = "neuralnet"),
        algorithm = factor(algorithm, levels = methods)) %>%

       ###### FIXME: remove!!!!
      filter(model_name != "neuralnet")
    ###########


    res_long = res %>%
      pivot_longer(c(quality_measures), names_to = "quality") %>%
      mutate(quality = factor(quality, levels = quality_measures))


    if (length(datastrategy) == 2) {
      res_long = res_long %>%
        mutate(algorithm = paste(algorithm, datastrategy, sep = "_"))
    }

    # if (length(postprocessed) == 2) {
    #   res_long = res_long %>%
    #     mutate(algorithm = paste(algorithm, postprocessed, sep = "_"))
    # }

    return(res_long)
  })

  names(aggrres) = data_set_names
  ll = dplyr::bind_rows(aggrres, .id = "dataset")

  ll = ll %>%
    group_by(problem, model_name, id_x_interest) %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup()

  ll$postprocessed = factor(ll$postprocessed, levels = c(0, 1), label = c("without postproc", "with postproc"))
  ll$algorithm = factor(ll$algorithm, levels = apply(expand.grid(rev(methods), datastrategy), 1, paste, collapse="_"))

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
    ylab("") +
    xlab("")
  if (is.null(orientation)) {
    if (all(c(0, 1) %in% postprocessed)) {
      plt = plt + facet_grid(postprocessed ~ quality)
    } else {
      plt = plt + facet_wrap(~quality)
    }
    height = 4
  } else if (orientation == "model") {
    plt = plt +  facet_grid(model_name ~ quality, scales = "free")
    height = 10
  } else if (orientation == "dataset") {
    plt = plt + facet_grid(dataset ~ quality, scales = "free")
    height = 7.5
  }
  # mindata = left_join(x = minid, y = ll, by = c("model_name", "algorithm", "id", "dataset"))
  plt = plt +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = n_colors, name = "Paired")) +
    theme_bw() +
    scale_y_continuous(breaks= pretty_breaks(n = 5)) +
    # scale_y_continuous(breaks=c(0, 0.25, 0.5, 1)) +
    theme(
      #strip.text = element_text(size = 10, margin = margin(t = 2.5, r = 2.5,
      #  b = 2.5, l = 2.5, unit = "pt")),
      axis.text.x = element_text(size = 7),
      panel.spacing = unit(3, "pt")
    ) +
    coord_flip()
  # colorlines = "tan4"
  plt = plt +
    geom_line(aes(x = algorithm, y = value, group=group_id), alpha=0.2, color = "grey20")

  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    height = 3
    ggsave(filename = file.path(fig.path, paste0(paste("comparison_methods",
      orientation, paste0(datastrategy, collapse = "_"),
      paste0(postprocessed, collapse = "_"), sep = "_"), ".pdf")),
      plot = plt, width = 7, height = height) # 5.5, 3.8
  }

  return(plt)
}

comparison_table = function(methods = c("maxbox", "prim", "anchors", "maire"), orientation = "model", savextable = FALSE) {
  browser()
  data_set_names = c("diabetes", "tic_tac_toe", "cmc", "vehicle", "no2", "plasma_retinol")
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
       pivot_longer(c(locality:efficiency), names_to = "quality") %>%
      mutate(quality = factor(quality, levels = c("locality", "coverage_train", "coverage_sampled",
       "coverage_levelset_train", "coverage_levelset_sampled",
       "precision_train", "precision_sampled",
       "maximality_train", "maximality_sampled", "efficiency")))
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

create_runtime_maximality_table = function(methods = c("maxbox", "prim", "anchors", "maire"), orientation = NULL, savextable = FALSE) {
  data_set_names = c("diabetes", "tic_tac_toe", "cmc", "vehicle", "no2", "plasma_retinol")
  if (!is.null(orientation)) {
    checkmate::assert_names(orientation, subset.of = c("model", "dataset"))
  }

  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {

    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, datanam) %>% collect()
    DBI::dbDisconnect(con)

    res_long = res %>%
      filter(algorithm %in% methods) %>%
      mutate(model_name = recode(model_name, ranger = "randomforest",
        logistic_regression = "logreg", neural_network = "neuralnet")) %>%

      ###### FIXME: remove!!!!
      filter(model_name != "neuralnet") %>%
    ###########


      pivot_longer(c("maximality_train", "maximality_sampled", "efficiency"), names_to = "quality") %>%
      mutate(quality = factor(quality, levels = c("maximality_train", "maximality_sampled", "efficiency")))

    # res_long = res_long %>%
    #   mutate(algorithm = paste(algorithm, datastrategy, sep = "_"))

    return(res_long)

  })

  names(aggrres) = data_set_names
  ll = dplyr::bind_rows(aggrres, .id = "dataset")
  ll$datastrategy = factor(ll$datastrategy, levels = c("traindata", "sampled"))

  ll = ll %>% group_by(
    postprocessed, algorithm, datastrategy, quality
  ) %>%
    summarize(value = round(mean(value, na.rm = TRUE), 2)) %>%
    #   sd = round(sd(value, na.rm = TRUE), 2)) %>%
    # mutate(value = paste0(mean, " (", sd, ")")) %>%
    # select(-mean, -sd) %>%
    ungroup %>%
    pivot_wider(names_from = c("quality", "postprocessed"), values_from = "value") %>%
    mutate(efficiency_0 = round(efficiency_0, 0),
      efficiency_1 = round(efficiency_1, 0)) %>%
    arrange(datastrategy) %>%
    select(datastrategy, algorithm, starts_with(c("maximality_train", "maximality_sampled", "efficiency")))

  ll$datastrategy = as.character(ll$datastrategy)
  xtable(ll)

  # rle.lengths <- rle(ll[[1]])$lengths
  # first <- !duplicated(ll[[1]])
  # ll[[1]][!first] <- ""
  # ll[[1]][first] <-
  #   paste0("\\midrule\\multirow{", rle.lengths, "}{*}{\\textbf{", ll[[1]][first], "}}")

  print(xtable(ll[, -1], digits = 2, # digits = c(0, 0, 0, 3, 1, 0, 6), # first zero "represents" row numbers which we skip later
    align = "llrr|rr|rr",  # align and put a vertical line (first "l" again represents column of row numbers)
    label = "testTable"),
    size = "footnotesize", #Change size; useful for bigger tables "normalsize" "footnotesize"
    include.rownames = FALSE, #Don't print rownames
    include.colnames = FALSE, #We create them ourselves
    caption.placement = "top", #"top", NULL
    hline.after=NULL, #We don't need hline; we use booktabs
    floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
    sanitize.text.function = force, # Important to treat content of first column as latex function
    add.to.row = list(pos = list(-1,
      4,
      nrow(ll)),
      command = c(paste("\\toprule \n",  # NEW row
        " & \\multicolumn{2}{c}{maximality_{train}} & \\multicolumn{2}{c}{maximality_{samp}} & \\multicolumn{2}{c}{efficiency} \\\\\n",
        "\\cmidrule(l){2-3} \\cmidrule(l){4-5} \\cmidrule(l){6-7} \n \n",
        "& 0 & 1 & 0 & 1 & 0 & 1   \\\\\n", # NEW row
        "\\midrule \n"
      ),
        paste("\\cmidrule(l){2-3} \\cmidrule(l){4-5} \\cmidrule(l){6-7} \n \n" # we may also use 'pos' and 'command' to add a midrule
        ),
        paste("\\bottomrule \n"  # paste is used as it is more flexible regarding adding lines
        )
      )
    )
  )

}

