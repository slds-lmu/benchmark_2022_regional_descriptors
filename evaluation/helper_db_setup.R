add_results_to_db = function(data_set_name, reg) {

  reg = loadRegistry(reg_dir, make.default = FALSE)
  job_overview = unwrap(getJobPars(reg = reg))
  jobs_of_this_data_set = job_overview[problem == data_set_name]

  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")

  for (job_id in jobs_of_this_data_set$job.id) {
    this_job = job_overview[job.id == job_id]
    ird = try(readRDS(file.path(reg_dir, "results", paste0(this_job$job.id, ".rds"))))
    attr(ird, "time_running") <- getJobStatus(this_job, reg)$time.running
    if (inherits(ird, "try-error")) next

    orig = ird$orig
    postproc = ird$postproc

    print(job_id)
    if (length(ird) > 0L) {

      origeval = evaluate_quality(orig)
      postproceval = evaluate_quality(postproc)

      dt_standard = cbind(rbind(origeval, postproceval), this_job,
        "time_running" = as.numeric(getJobStatus(this_job, reg)$time.running))
      dt_standard = dt_standard[, postprocessed := c(FALSE, TRUE)]

      if (!toupper(data_set_name) %in% DBI::dbListTables(con)) {
        dbWriteTable(con, toupper(data_set_name), as.data.frame(dt_standard), overwrite = TRUE)
      } else {
        dbAppendTable(con, toupper(data_set_name), as.data.frame(dt_standard))
      }
    }
  }

  dbExecute(con, paste("ALTER TABLE", toupper(data_set_name), "ADD COLUMN ID INT"))
  res = tbl(con, toupper(data_set_name)) %>% collect()
  res$ID = 1:nrow(res)
  dbWriteTable(con, toupper(data_set_name), as.data.frame(res), overwrite = TRUE)
  dbDisconnect(con)
}
