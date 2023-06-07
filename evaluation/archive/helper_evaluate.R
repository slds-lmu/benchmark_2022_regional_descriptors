add_evals_to_db = function(data_set_name) {
  browser()
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = tbl(con, toupper(data_set_name)) %>% collect()

  # whatif = res %>%
  #   filter(algorithm == "whatif") %>%
  #   # mutate(algo_spec = "whatif") %>%
  #   group_by(id_x_interest, model_name) %>%
  #   group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
  #   ungroup() %>%
  #   rename(no_counterfactuals = n)
  #
  #
  # # derive nondominated
  #  nondomindwi = whatif %>% group_by(id_x_interest, model_name) %>%
  #   group_modify(~ broom::tidy(get_nondominated(cbind(.x$dist_x_interest, .x$no_changed, .x$dist_train)))) %>%
  #   ungroup()
  #  whatif = whatif %>%
  #   mutate(nondom = nondomindwi$x) %>%
  #   filter(nondom) %>%
  #   group_by(id_x_interest, model_name) %>%
  #   group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
  #   ungroup %>%
  #   rename(no_nondom = n)
  #
  # lookup_runtime = res %>%
  #   filter(algorithm  == "nice") %>%
  #   group_by(id_x_interest, model_name) %>%
  #   summarise(time_running = sum(unique(time_running), na.rm = TRUE)) %>%
  #   ungroup()
  #
  # nice_all = res %>%
  #   filter(algorithm  == "nice") %>%
  #   # add overall runtime from lookup
  #   select(-time_running) %>%
  #   left_join(lookup_runtime, by = c("model_name", "id_x_interest")) %>%
  #   group_by(id_x_interest, model_name) %>%
  #   group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
  #   ungroup() %>%
  #   rename(no_counterfactuals = n) # %>%
  # nondomind = nice_all %>% group_by(id_x_interest, model_name) %>%
  #   group_modify(~ broom::tidy(get_nondominated(cbind(.x$dist_x_interest, .x$no_changed, .x$dist_train)))) %>%
  #   ungroup()
  # nice_all = nice_all %>%
  #     mutate(nondom = nondomind$x) %>%
  #           filter(nondom) %>%
  #     # remove duplicates
  #     distinct_at(vars(-job.id, -optimization, -ID, -time_running), .keep_all = TRUE) %>%
  #     group_by(id_x_interest, model_name) %>%
  #     group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
  #     ungroup %>%
  #     rename(no_nondom = n)
  #
  # moc = res %>%
  #   filter(algorithm  == "moc") %>%
  #   filter(dist_target == 0) %>%
  #   group_by(id_x_interest, model_name) %>%
  #   group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
  #   ungroup() %>%
  #   rename(no_counterfactuals = n)  %>%
  #     group_by(id_x_interest, model_name) %>%
  #     group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
  #     ungroup %>%
  #     rename(no_nondom = n)


  all_methods = rbindlist(list(whatif, nice_all, moc), fill = TRUE)
  dbWriteTable(con, paste0(toupper(data_set_name), "_EVAL"), all_methods, overwrite = TRUE)
  dbDisconnect(con)
}
