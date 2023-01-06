read_in_results = function(data_set_name, reg_dir = "cfactuals/prod/registry") {
  reg = loadRegistry(reg_dir, make.default = FALSE)
  job_overview = unwrap(getJobPars(reg = reg))
  
  checkmate::assert_choice(data_set_name, unique(job_overview[["problem"]]))
  cat("start read in")
  cat("read in whatif")
  dt1 = read_in_what_if(reg, job_overview, data_set_name)
  cat("read in nice")
  dt2 = read_in_nice(reg, job_overview, data_set_name)
  cat("read in moc")
  dt3 = read_in_moc(reg, job_overview, data_set_name)
  rbind(dt1, dt2, dt3)
}

read_in_what_if = function(reg, job_overview, data_set_name) {
  jobs_what_if = job_overview[algorithm == "whatif" & problem == data_set_name]
  res = data.table::rbindlist(lapply(jobs_what_if[["job.id"]], function(x) {
    res_i = loadResult(x)[, c("dist_x_interest", "no_changed", "dist_train", "dist_target")]
    res_i[, job.id := x]
    res_i
  }), use.names = TRUE)
  jobs_what_if[, algo_spec := paste(algorithm, n_counterfactuals, sep = "_")]
  jobs_what_if = jobs_what_if[, c("job.id", "algo_spec", "model_name", "id_x_interest")]
  dt = merge(jobs_what_if, res, by = 'job.id', all.x = TRUE)
  merge(dt, getJobStatus()[, c("job.id", "time.running")])
}

read_in_nice = function(reg, job_overview, data_set_name) {
  jobs_nice = job_overview[algorithm == "nice" & problem == data_set_name]
  res = data.table::rbindlist(lapply(jobs_nice[["job.id"]], function(x) {
    res_i = loadResult(x)[, c("dist_x_interest", "no_changed", "dist_train", "dist_target")]
    res_i[, job.id := x]
    res_i
  }), use.names = TRUE)
  jobs_nice[, algo_spec := paste(algorithm, optimization, sep = "_")]
  jobs_nice = jobs_nice[, c("job.id", "algo_spec", "model_name", "id_x_interest")]
  dt = merge(jobs_nice, res, by = 'job.id', all.x = TRUE)
  merge(dt, getJobStatus()[, c("job.id", "time.running")])
}

read_in_moc = function(reg, job_overview, data_set_name) {
  jobs_moc = job_overview[algorithm == "moc" & problem == data_set_name]
  res = data.table::rbindlist(lapply(jobs_moc[["job.id"]], function(x) {
    res_i = loadResult(x)[, c("dist_x_interest", "no_changed", "dist_train", "dist_target")]
    res_i[, job.id := x]
    res_i
  }), use.names = TRUE)
  jobs_moc[, algo_spec := paste(algorithm, init_strategy, use_conditional_mutator, sep = "_")]
  jobs_moc = jobs_moc[,  c("job.id", "algo_spec", "model_name", "id_x_interest")]
  dt = merge(jobs_moc, res, by = 'job.id', all.x = TRUE)
  merge(dt, getJobStatus()[, c("job.id", "time.running")])
}


