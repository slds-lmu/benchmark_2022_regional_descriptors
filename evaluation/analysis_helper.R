compare_methods = function (methods = c("maxbox", "prim", "maire", "anchors"),
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


    if (any(c("robustness_train", "robustness_sampled") %in% quality_measures)) {
      conres = dbConnect(RSQLite::SQLite(), "robustness/db_robustness_x.db")
      conresanchors = dbConnect(RSQLite::SQLite(), "robustness/db_robustness_x_anchors.db")
      # all
      resrobustness = tbl(conres, datanam) %>% collect() %>% filter(algorithm != "anchors")
      resrobustness_anchors = tbl(conresanchors, datanam) %>% collect()
      resrobustness = rbind(resrobustness, resrobustness_anchors)
      DBI::dbDisconnect(conres)
      resrobustness = resrobustness %>% select(job.id, problem, algorithm, id_x_interest, model_name, datastrategy,
                               postprocessed, robustness_traindata, robustness_sampled) %>%
        # filter(algorithm != "anchors") %>%
        mutate(robustness_traindata = 1 - robustness_traindata,
               robustness_sampled = 1 - robustness_sampled)

       res = merge(res, resrobustness, by = c("job.id", "problem", "algorithm", "id_x_interest",
                                       "model_name", "datastrategy", "postprocessed"), all = TRUE)

    }

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
      mutate(model_name = recode(model_name, ranger = "random forest",
        linear_model = "linear model", neural_network = "neural net"),
        algorithm = factor(algorithm, levels = methods))


    res_long = res %>%
      pivot_longer(c(quality_measures), names_to = "quality") %>%
      mutate(quality = factor(quality, levels = quality_measures))

    return(res_long)
  })

  names(aggrres) = data_set_names
  ll = dplyr::bind_rows(aggrres, .id = "dataset")

  ll = ll %>%
    group_by(problem, model_name, id_x_interest) %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup()

  ll$postprocessed = factor(ll$postprocessed, levels = c(0, 1), label = c("without postproc", "with postproc"))
  # ll$algorithm = factor(ll$algorithm, levels = apply(expand.grid(rev(methods), datastrategy), 1, paste, collapse="_"))
  ll$algorithm = factor(ll$algorithm, levels = rev(methods))
  ds = datastrategy
  ll$datastrategy = factor(ll$datastrategy, levels = rev(ds))
  ll$dataset = factor(ll$dataset, levels = data_set_names, labels = data_set_names)

  # create plots
  n_colors = length(unique(ll$algorithm))
  plt = ggplot(ll) +
    geom_boxplot(aes(x = algorithm, y = value, fill = algorithm), show.legend = FALSE) +
    ylab("") +
    xlab("")
  if (is.null(orientation)) {
    if (all(c(0, 1) %in% postprocessed)) {
      plt = plt + facet_grid(postprocessed + datastrategy ~ quality)
    } else {
      plt = plt + facet_wrap(~quality)
    }
    height = 3
    width = 7
  } else if (orientation == "model") {
    if (all(c("traindata", "sampled") %in% datastrategy)) {
      plt = plt + facet_grid(model_name + datastrategy ~ quality)
    } else {
      plt = plt +  facet_grid(model_name ~ quality, scales = "free")
    }
    height = 8.5
    width = 9.5
  } else if (orientation == "dataset") {
    if (all(c("traindata", "sampled") %in% datastrategy)) {
      plt = plt + facet_grid(dataset + datastrategy ~ quality)
    } else {
      plt = plt +  facet_grid(dataset ~ quality, scales = "free")
    }
    height = 12
    width = 9.5
  }
  plt = plt +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = n_colors, name = "Paired")) +
    theme_bw() +
    scale_y_continuous(breaks= pretty_breaks(n = 4)) +
    # scale_y_continuous(breaks=c(0, 0.25, 0.5, 1)) +
    theme(
      #strip.text = element_text(size = 10, margin = margin(t = 2.5, r = 2.5,
      #  b = 2.5, l = 2.5, unit = "pt")),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
      panel.spacing = unit(3, "pt")
    ) +
    coord_flip()
  # colorlines = "tan4"
  # plt = plt +
  #   geom_line(aes(x = algorithm, y = value, group=group_id), alpha=0.2, color = "grey20")

  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    ggsave(filename = file.path(fig.path, paste0(paste("comparison_methods",
      orientation, paste0(datastrategy, collapse = "_"),
      paste0(postprocessed, collapse = "_"), sep = "_"), ".pdf")),
      plot = plt, width = width, height = height) # 5.5, 3.8
  }

  return(plt)
}

create_runtime_maximality_table = function(methods = c("maxbox", "prim", "anchors", "maire"),
                  quality_measures = c("maximality_train", "maximality_sampled", "efficiency"), orientation = NULL, savextable = FALSE) {
  data_set_names = c("diabetes", "tic_tac_toe", "cmc", "vehicle", "no2", "plasma_retinol")
  if (!is.null(orientation)) {
    checkmate::assert_names(orientation, subset.of = c("model", "dataset"))
  }

  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {

    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, datanam) %>% collect()
    DBI::dbDisconnect(con)

    res = res %>% mutate(maximality_train = ifelse(precision_train == 1, maximality_train, 0),
                         maximality_sampled = ifelse(precision_sampled == 1, maximality_sampled, 0)
         #  maximality_sampled = ifelse(!is.na(precision_sampled), ifelse(precision_sampled == 1, maximality_sampled, 0), ifelse(precision_train == 1, maximality_sampled, 0))
    )

    res_long = res %>%
      filter(algorithm %in% methods) %>%
      mutate(model_name = recode(model_name, ranger = "randomforest",
        logistic_regression = "logreg", neural_network = "neuralnet")) %>%
      pivot_longer(quality_measures, names_to = "quality") %>%
      mutate(quality = factor(quality, levels = quality_measures))

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
    pivot_wider(names_from = c("quality", "postprocessed"), values_from = "value") # %>%
    # mutate(efficiency_1 = efficiency_0 + efficiency_1,
    #        efficiency_rel = efficiency_1/efficiency_0) %>%
    arrange(datastrategy) %>%
    select(datastrategy, algorithm, starts_with(quality_measures))

  ll$algorithm = factor(ll$algorithm, levels = methods)
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

