add_results_to_db_robustness = function(data_set_name, reg) {
  reg = loadRegistry(reg_dir, make.default = FALSE)
  job_overview = unwrap(getJobPars(reg = reg))
  jobs_of_this_data_set = job_overview[problem == data_set_name & algorithm == "anchors",]

  traindata = data_list[[data_set_name]]
  con = dbConnect(RSQLite::SQLite(), db_name)
  for (job_id in jobs_of_this_data_set$job.id) {
    this_job = job_overview[job.id == job_id]

    if (all(this_job$algorithm %in% "maxbox")) {
      ird = try(readRDS(file.path("ird/prod/registry_maxbox/", "results", paste0(this_job$job.id, ".rds"))))
    } else {
      ird = try(readRDS(file.path(reg_dir, "results", paste0(this_job$job.id, ".rds"))))
    }
    if (inherits(ird, "try-error")) next

    print(job_id)

    sampdata = readRDS(paste0("dataeval/prod/data_inbox_sampled/",
                              paste(data_set_name, this_job$model_name, this_job$id_x_interest, sep = "_"), ".rds"))

    orig = ird$orig
    postproc = ird$postproc

    if (length(ird) > 0L) {
      origeval = evaluate_robustness(orig, data_set_name = this_job$problem, traindata = traindata, sampdata = sampdata,
                                     model_name = this_job$model_name, id_x_interest = this_job$id_x_interest,
                                     datastrategy = this_job$datastrategy, method = this_job$algorithm, postproc = FALSE)
      postproceval = evaluate_robustness(postproc, data_set_name = this_job$problem, traindata = traindata, sampdata = sampdata,
                                         model_name = this_job$model_name, id_x_interest = this_job$id_x_interest,
                                         datastrategy = this_job$datastrategy, method = this_job$algorithm, postproc = TRUE)
    }
    dt_standard = cbind(rbind(origeval, postproceval), this_job)
    dt_standard = dt_standard[, postprocessed := c(FALSE, TRUE)]

    if (!toupper(data_set_name) %in% DBI::dbListTables(con)) {
      dbWriteTable(con, toupper(data_set_name), as.data.frame(dt_standard), overwrite = TRUE)
    } else {
      dbAppendTable(con, toupper(data_set_name), as.data.frame(dt_standard))
    }
  }
  dbExecute(con, paste("ALTER TABLE", toupper(data_set_name), "ADD COLUMN ID INT"))
  res = tbl(con, toupper(data_set_name)) %>% collect()
  res$ID = 1:nrow(res)
  dbWriteTable(con, toupper(data_set_name), as.data.frame(res), overwrite = TRUE)
  dbDisconnect(con)
}

evaluate_robustness = function(regdesc, data_set_name, traindata,
                               sampdata, model_name, id_x_interest, datastrategy, method, postproc) {
  ids_td = which(irgn::identify_in_box(regdesc$box, data = traindata))
  ids_sd = which(irgn::identify_in_box(regdesc$box, data = sampdata))

  max_overlap_td = NA
  max_overlap_sd = NA

  # get jobs ids for newly x
  job_overview = unwrap(getJobPars(reg = regrobust))
  md = model_name
  idx = id_x_interest
  ds = datastrategy
  jobs_of_this_data_set = job_overview[problem == data_set_name &
                                         model_name == md & id_x_interest == idx &
                                         datastrategy == ds & algorithm == method,]
  # for each job id
  for (job_id in jobs_of_this_data_set$job.id) {
    # ird = try(readRDS(file.path("robustness/prod/registry_robustness_x_resubmit/",
    #                             "results", paste0(job_id, ".rds"))))
    # if (inherits(ird, "try-error")) {
      ird = try(readRDS(file.path(reg_dir_robust,
                                  "results", paste0(job_id, ".rds"))))
    # }

    if (inherits(ird, "try-error")) next

    if (is.na(max_overlap_td) & is.na(max_overlap_sd)) {
      max_overlap_td = 0
      max_overlap_sd = 0
    }

    if (!postproc) {
      box = ird$orig$box
    } else {
      box = ird$postproc$box
    }

    dist = StatMatch::gower.dist(regdesc$x_interest, ird$orig$x_interest)
    # evaluate overlap
    ids_td_new = which(irgn::identify_in_box(box, data = traindata))
    tab_td = table(c(ids_td_new, ids_td))
    overlap_td = (1-(sum(tab_td == 2)/length(tab_td)))

    ids_sd_new = which(irgn::identify_in_box(box, data = sampdata))
    tab_sd = table(c(ids_sd_new, ids_sd))
    overlap_sd = (1-(sum(tab_sd == 2)/length(tab_sd)))

    # update max overlap
    if (!is.na(overlap_td) & overlap_td > max_overlap_td) {
      max_overlap_td = overlap_td
    }
    if (!is.na(overlap_sd) & overlap_sd > max_overlap_sd) {
      max_overlap_sd = overlap_sd
    }
  }
  return(c(robustness_traindata = max_overlap_td, robustness_sampled = max_overlap_sd))
}
