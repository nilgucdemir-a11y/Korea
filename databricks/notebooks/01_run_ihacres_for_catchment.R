# Databricks notebook source
# MAGIC %md
# MAGIC # Run IHACRES for one catchment
# MAGIC Intended for Databricks For-Each task fan-out (one catchment per task).

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text("catchment_id", "", "Catchment ID")
  dbutils.widgets.text("sub_region", "KOR", "Sub-region")
  dbutils.widgets.text("weights_file", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Catchmentweights/R02_precip_ops_per_catchment.csv", "Weights CSV")
  dbutils.widgets.text("precip_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.precip.data/", "Precip RDS directory")
  dbutils.widgets.text("temp_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.temp.data/", "Temp RDS directory")
  dbutils.widgets.text("river_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.river.data/", "River RDS directory")
  dbutils.widgets.text("ptq_output_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/catchments_daily_PTQ_by_RiverID/KOR/", "Daily PTQ output directory")
  dbutils.widgets.text("ihacres_output_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/ihacres_results/KOR_1000y/", "IHACRES output directory")
  dbutils.widgets.text("start_date", "0000-01-01", "Series start date")
  dbutils.widgets.text("end_date", "1000-12-31", "Series end date")
  dbutils.widgets.dropdown("write_csv_out", "true", c("true", "false"), "Write PTQ CSV")
  dbutils.widgets.text("calibration_samples", "1000", "fitByOptim samples")
  dbutils.widgets.dropdown("optimization_method", "PORT", c("PORT", "NLOPT_LN_COBYLA"), "Optimization method")
  dbutils.widgets.dropdown("objective", "kge", c("kge", "NSE"), "Objective")
  dbutils.widgets.dropdown("model_type", "snow", c("snow", "cmd"), "SMA model")
  dbutils.widgets.text("min_obs", "365", "Minimum complete rows")
}

# COMMAND ----------

get_param <- function(name, default) {
  if (exists("dbutils")) return(dbutils.widgets.get(name))
  default
}

catchment_id <- get_param("catchment_id", "")
sub_region <- get_param("sub_region", "KOR")
weights_file <- get_param("weights_file", "")
precip_dir <- get_param("precip_dir", "")
temp_dir <- get_param("temp_dir", "")
river_dir <- get_param("river_dir", "")
ptq_output_dir <- get_param("ptq_output_dir", "/tmp/ihacres/ptq")
ihacres_output_dir <- get_param("ihacres_output_dir", "/tmp/ihacres/results")
start_date <- parse_date_or_stop(get_param("start_date", "0000-01-01"), "start_date")
end_date <- parse_date_or_stop(get_param("end_date", "1000-12-31"), "end_date")
write_csv_out <- parse_bool(get_param("write_csv_out", "true"), default = TRUE)
calibration_samples <- as.integer(get_param("calibration_samples", "1000"))
optimization_method <- get_param("optimization_method", "PORT")
objective <- get_param("objective", "kge")
model_type <- get_param("model_type", "snow")
min_obs <- as.integer(get_param("min_obs", "365"))

if (!nzchar(catchment_id)) stop("catchment_id must be provided")
if (!file.exists(weights_file)) stop(sprintf("Weights file does not exist: %s", weights_file))

safe_dir_create(ptq_output_dir)
safe_dir_create(ihacres_output_dir)
safe_dir_create(file.path(ihacres_output_dir, "metrics"))
safe_dir_create(file.path(ihacres_output_dir, "timeseries"))
safe_dir_create(file.path(ihacres_output_dir, "models"))
safe_dir_create(file.path(ihacres_output_dir, "logs"))

ensure_packages_installed()

library(dplyr)
library(readr)
library(tibble)
library(jsonlite)
library(zoo)
library(hydromad)
library(hydroGOF)

ptq_rds_path <- file.path(ptq_output_dir, paste0(catchment_id, ".rds"))
ptq_csv_path <- file.path(ptq_output_dir, paste0(catchment_id, ".csv"))
metrics_path <- file.path(ihacres_output_dir, "metrics", paste0(catchment_id, "_metrics.csv"))
model_path <- file.path(ihacres_output_dir, "models", paste0(catchment_id, "_fit.rds"))
timeseries_path <- file.path(ihacres_output_dir, "timeseries", paste0(catchment_id, "_sim_vs_obs.csv"))
log_path <- file.path(ihacres_output_dir, "logs", paste0(catchment_id, "_run_log.json"))

run_started_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

result_row <- tryCatch({
  weights <- readr::read_csv(weights_file, show_col_types = FALSE, col_types = readr::cols(.default = "c")) %>%
    dplyr::filter(sub.region == sub_region) %>%
    dplyr::mutate(weight = as.numeric(weight))

  required_cols <- c("catchment_id", "op.id", "weight")
  if (!all(required_cols %in% names(weights))) {
    stop("Weights file must contain columns: catchment_id, op.id, weight")
  }

  precip_index <- index_rds_files(precip_dir, ".*_(G[0-9]+_[0-9]+)\\.rds$")
  temp_index <- index_rds_files(temp_dir, ".*_(G[0-9]+_[0-9]+)\\.rds$")
  river_index <- index_rds_files(river_dir, ".*_(R[0-9]+_[^\\.]+)\\.rds$")

  daily_result <- build_daily_ptq_for_catchment(
    catchment_id = catchment_id,
    weights_df = weights,
    precip_files_index = precip_index,
    temp_files_index = temp_index,
    river_files_index = river_index,
    start_date = start_date
  )

  if (daily_result$status != "ok") {
    tibble::tibble(
      catchment_id = catchment_id,
      status = "skip",
      reason = daily_result$reason,
      n_rows_ptq = NA_integer_,
      n_rows_model = NA_integer_,
      optimizer = NA_character_,
      objective = objective,
      model_type = model_type,
      KGE = NA_real_,
      NSE = NA_real_,
      RMSE = NA_real_,
      ptq_rds_path = ptq_rds_path,
      model_path = NA_character_,
      timeseries_path = NA_character_,
      run_started_utc = run_started_utc,
      run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  } else {
    daily_df <- daily_result$data
    saveRDS(daily_df, ptq_rds_path)
    if (write_csv_out) readr::write_csv(daily_df, ptq_csv_path)

    fit_result <- fit_ihacres_model(
      daily_df = daily_df,
      start_date = start_date,
      end_date = end_date,
      samples = calibration_samples,
      optimization_method = optimization_method,
      objective = objective,
      model_type = model_type,
      min_obs = min_obs
    )

    if (fit_result$status != "ok") {
      tibble::tibble(
        catchment_id = catchment_id,
        status = fit_result$status,
        reason = fit_result$reason,
        n_rows_ptq = nrow(daily_df),
        n_rows_model = NA_integer_,
        optimizer = NA_character_,
        objective = objective,
        model_type = model_type,
        KGE = NA_real_,
        NSE = NA_real_,
        RMSE = NA_real_,
        ptq_rds_path = ptq_rds_path,
        model_path = NA_character_,
        timeseries_path = NA_character_,
        run_started_utc = run_started_utc,
        run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
    } else {
      saveRDS(fit_result$fit, model_path)

      ts_dates <- as.Date(zoo::index(fit_result$model_ts))
      len <- min(length(ts_dates), length(fit_result$obs_q), length(fit_result$sim_q))
      ts_df <- tibble::tibble(
        Date = ts_dates[seq_len(len)],
        Q_obs = fit_result$obs_q[seq_len(len)],
        Q_sim = fit_result$sim_q[seq_len(len)],
        catchment_id = catchment_id
      )
      readr::write_csv(ts_df, timeseries_path)

      tibble::tibble(
        catchment_id = catchment_id,
        status = "ok",
        reason = NA_character_,
        n_rows_ptq = nrow(daily_df),
        n_rows_model = fit_result$n_obs,
        optimizer = fit_result$optimizer_used,
        objective = objective,
        model_type = model_type,
        KGE = as.numeric(fit_result$metrics$KGE),
        NSE = as.numeric(fit_result$metrics$NSE),
        RMSE = as.numeric(fit_result$metrics$RMSE),
        ptq_rds_path = ptq_rds_path,
        model_path = model_path,
        timeseries_path = timeseries_path,
        run_started_utc = run_started_utc,
        run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
    }
  }
}, error = function(e) {
  tibble::tibble(
    catchment_id = catchment_id,
    status = "error",
    reason = as.character(e$message),
    n_rows_ptq = NA_integer_,
    n_rows_model = NA_integer_,
    optimizer = NA_character_,
    objective = objective,
    model_type = model_type,
    KGE = NA_real_,
    NSE = NA_real_,
    RMSE = NA_real_,
    ptq_rds_path = ptq_rds_path,
    model_path = NA_character_,
    timeseries_path = NA_character_,
    run_started_utc = run_started_utc,
    run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
})

readr::write_csv(result_row, metrics_path)

run_log <- list(
  catchment_id = catchment_id,
  status = result_row$status[[1]],
  reason = result_row$reason[[1]],
  metrics_path = metrics_path,
  finished_utc = result_row$run_finished_utc[[1]]
)
writeLines(jsonlite::toJSON(run_log, auto_unbox = TRUE, pretty = TRUE), log_path)

safe_set_task_value("status", result_row$status[[1]])
safe_set_task_value("metrics_path", metrics_path)

print(result_row)
