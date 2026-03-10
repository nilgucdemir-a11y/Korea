# Databricks notebook source
# MAGIC %md
# MAGIC # Calibrate IHACRES for one catchment
# MAGIC Runs calibration only and saves fitted model for later simulation.
# MAGIC If `catchment_id` is empty, it runs the full catchment list from setup manifest.

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text("catchment_id", "", "Catchment ID (optional)")
  dbutils.widgets.text("catchment_idx", "", "Catchment index in manifest (optional)")
  dbutils.widgets.text("run_config_path", "", "Run config path from setup task (optional)")
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 1) Read inputs and run config

# COMMAND ----------

catchment_id_input <- trimws(get_widget_or_default("catchment_id", ""))
catchment_idx_input <- trimws(get_widget_or_default("catchment_idx", ""))
run_config_path_input <- get_widget_or_default("run_config_path", "")

cfg <- read_run_config_or_stop(run_config_path_input)
run_config_path <- as.character(if (is.null(cfg$resolved_run_config_path)) run_config_path_input else cfg$resolved_run_config_path)
message(sprintf("Using run_config_path: %s", run_config_path))

sub_region <- toupper(as.character(cfg_value(cfg, "sub_region", "KOR")))
use_existing_peq <- parse_bool(cfg_value(cfg, "use_existing_peq", TRUE), default = TRUE)
peq_dir <- as.character(cfg_value(cfg, "peq_dir", ""))
weights_file <- as.character(cfg_value(cfg, "weights_file", ""))
precip_dir <- as.character(cfg_value(cfg, "precip_dir", ""))
temp_dir <- as.character(cfg_value(cfg, "temp_dir", ""))
river_dir <- as.character(cfg_value(cfg, "river_dir", ""))
ptq_output_dir <- as.character(cfg_value(cfg, "ptq_output_dir", "/tmp/ihacres/ptq"))
ihacres_output_dir <- as.character(cfg_value(cfg, "ihacres_output_dir", "/tmp/ihacres/results"))
catchment_manifest_path <- as.character(cfg_value(cfg, "catchment_manifest_path", ""))
catalog_rds_path <- as.character(cfg_value(cfg, "runtime_catalog_path", file.path(ihacres_output_dir, "manifests", "runtime_catalog.rds")))
start_date <- parse_date_or_stop(as.character(cfg_value(cfg, "start_date", "0000-01-01")), "start_date")
calibration_years <- parse_int_or_stop(as.character(cfg_value(cfg, "calibration_years", 100L)), "calibration_years", min_value = 1L)
days_per_year <- parse_int_or_stop(as.character(cfg_value(cfg, "days_per_year", 365L)), "days_per_year", min_value = 1L)
write_csv_out <- parse_bool(cfg_value(cfg, "write_csv_out", TRUE), default = TRUE)
calibration_samples <- parse_int_or_stop(as.character(cfg_value(cfg, "calibration_samples", 1000L)), "calibration_samples", min_value = 1L)
optimization_method <- as.character(cfg_value(cfg, "optimization_method", "PORT"))
objective <- normalize_objective(cfg_value(cfg, "objective", "kge"))
model_type <- tolower(as.character(cfg_value(cfg, "model_type", "snow")))
min_obs <- parse_int_or_stop(as.character(cfg_value(cfg, "min_obs", 365L)), "min_obs", min_value = 30L)

if (!(model_type %in% c("snow", "cmd"))) stop("model_type must be snow or cmd")
if (!(tolower(objective) %in% c("kge", "nse"))) stop("objective must be kge or NSE")

calibration_end_date <- window_end_from_years(start_date, calibration_years, days_per_year = days_per_year)

# COMMAND ----------

# MAGIC %md
# MAGIC ## 2) Prepare output folders, libraries, and catchment list

# COMMAND ----------

safe_dir_create(ihacres_output_dir)
safe_dir_create(file.path(ihacres_output_dir, "peq"))
safe_dir_create(file.path(ihacres_output_dir, "calibration_models"))
safe_dir_create(file.path(ihacres_output_dir, "calibration_metrics"))
safe_dir_create(file.path(ihacres_output_dir, "calibration_timeseries"))
safe_dir_create(file.path(ihacres_output_dir, "calibration_logs"))

if (!use_existing_peq) safe_dir_create(ptq_output_dir)

ensure_packages_installed()

library(dplyr)
library(readr)
library(tibble)
library(jsonlite)
library(zoo)
library(hydromad)

catchment_ids <- resolve_requested_catchments(
  catchment_id_input = catchment_id_input,
  catchment_idx_input = catchment_idx_input,
  catchment_manifest_path = catchment_manifest_path
)

if (!nzchar(catchment_id_input) && !nzchar(catchment_idx_input)) {
  message(sprintf("No catchment_id/catchment_idx provided. Running full list from manifest: %s catchments", length(catchment_ids)))
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 3) Calibrate helper for one catchment

# COMMAND ----------

catalog <- resolve_catalog(
  catalog_rds_path = catalog_rds_path,
  use_existing_peq = use_existing_peq,
  sub_region = sub_region,
  weights_file = weights_file,
  peq_dir = peq_dir,
  precip_dir = precip_dir,
  temp_dir = temp_dir,
  river_dir = river_dir
)

run_one_catchment <- function(catchment_id) {
  run_started_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  peq_rds_path <- file.path(ihacres_output_dir, "peq", paste0(catchment_id, ".rds"))
  peq_csv_path <- file.path(ihacres_output_dir, "peq", paste0(catchment_id, ".csv"))
  ptq_fallback_rds <- file.path(ptq_output_dir, paste0(catchment_id, ".rds"))
  ptq_fallback_csv <- file.path(ptq_output_dir, paste0(catchment_id, ".csv"))
  fit_path <- file.path(ihacres_output_dir, "calibration_models", paste0(catchment_id, "_fit.rds"))
  cal_ts_path <- file.path(ihacres_output_dir, "calibration_timeseries", paste0(catchment_id, "_calibration_sim_vs_obs.csv"))
  metrics_path <- file.path(ihacres_output_dir, "calibration_metrics", paste0(catchment_id, "_calibration_metrics.csv"))

  result_row <- tryCatch({
    peq_result <- resolve_peq_for_catchment(catchment_id = catchment_id, catalog = catalog, start_date = start_date)
    if (!identical(peq_result$status, "ok")) {
      tibble::tibble(
        catchment_id = catchment_id,
        status = "error",
        reason = peq_result$reason,
        model_type = model_type,
        objective = objective,
        optimizer = NA_character_,
        calibration_years = calibration_years,
        calibration_start_date = format_date_ymd(start_date),
        calibration_end_date = format_date_ymd(calibration_end_date),
        n_rows_peq = NA_integer_,
        n_rows_calibration = NA_integer_,
        n_obs_metrics = NA_integer_,
        KGE = NA_real_,
        NSE = NA_real_,
        RMSE = NA_real_,
        peq_path = NA_character_,
        fit_path = NA_character_,
        calibration_timeseries_path = NA_character_,
        run_started_utc = run_started_utc,
        run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )
    } else {
      peq_df <- peq_result$data
      save_rds_verified(peq_df, peq_rds_path, label = "PEQ cache RDS")
      if (write_csv_out) {
        write_csv_verified(peq_df, peq_csv_path, label = "PEQ cache CSV")
      }

      if (!use_existing_peq) {
        save_rds_verified(peq_df, ptq_fallback_rds, label = "PTQ fallback RDS")
        if (write_csv_out) {
          write_csv_verified(peq_df, ptq_fallback_csv, label = "PTQ fallback CSV")
        }
      }

      cal_start_text <- format_date_ymd(start_date)
      cal_end_text <- format_date_ymd(calibration_end_date)

      ts_result <- prepare_model_ts(
        peq_df = peq_df,
        start_date = start_date,
        end_date = calibration_end_date,
        min_obs = min_obs,
        require_q = TRUE
      )

      if (!identical(ts_result$status, "ok")) {
        tibble::tibble(
          catchment_id = catchment_id,
          status = "error",
          reason = ts_result$reason,
          model_type = model_type,
          objective = objective,
          optimizer = NA_character_,
          calibration_years = calibration_years,
          calibration_start_date = cal_start_text,
          calibration_end_date = cal_end_text,
          n_rows_peq = nrow(peq_df),
          n_rows_calibration = NA_integer_,
          n_obs_metrics = NA_integer_,
          KGE = NA_real_,
          NSE = NA_real_,
          RMSE = NA_real_,
          peq_path = peq_rds_path,
          fit_path = NA_character_,
          calibration_timeseries_path = NA_character_,
          run_started_utc = run_started_utc,
          run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        )
      } else {
        cal_result <- calibrate_hydromad_model(
          model_ts = ts_result$model_ts,
          samples = calibration_samples,
          optimization_method = optimization_method,
          objective = objective,
          model_type = model_type
        )

        save_rds_verified(cal_result$fit, fit_path, label = "calibration fit RDS")

        sim_result <- simulate_with_fit(
          fit = cal_result$fit,
          model_ts = ts_result$model_ts,
          model_type = model_type,
          objective = objective
        )

        if (!identical(sim_result$status, "ok")) {
          tibble::tibble(
            catchment_id = catchment_id,
            status = "error",
            reason = sim_result$reason,
            model_type = model_type,
            objective = objective,
            optimizer = cal_result$optimizer_used,
            calibration_years = calibration_years,
            calibration_start_date = cal_start_text,
            calibration_end_date = cal_end_text,
            n_rows_peq = nrow(peq_df),
            n_rows_calibration = ts_result$n_rows,
            n_obs_metrics = NA_integer_,
            KGE = NA_real_,
            NSE = NA_real_,
            RMSE = NA_real_,
            peq_path = peq_rds_path,
            fit_path = fit_path,
            calibration_timeseries_path = NA_character_,
            run_started_utc = run_started_utc,
            run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          )
        } else {
          metrics <- evaluate_simulation_metrics(sim_result$sim_q, sim_result$obs_q)

          cal_ts <- tibble::tibble(
            Date = sim_result$dates,
            Q_obs = sim_result$obs_q,
            Q_sim = sim_result$sim_q,
            catchment_id = catchment_id,
            stage = "calibration"
          )
          write_csv_verified(cal_ts, cal_ts_path, label = "calibration timeseries CSV")

          tibble::tibble(
            catchment_id = catchment_id,
            status = "ok",
            reason = NA_character_,
            model_type = model_type,
            objective = objective,
            optimizer = cal_result$optimizer_used,
            calibration_years = calibration_years,
            calibration_start_date = cal_start_text,
            calibration_end_date = cal_end_text,
            n_rows_peq = nrow(peq_df),
            n_rows_calibration = ts_result$n_rows,
            n_obs_metrics = metrics$n_obs,
            KGE = as.numeric(metrics$KGE),
            NSE = as.numeric(metrics$NSE),
            RMSE = as.numeric(metrics$RMSE),
            peq_path = peq_rds_path,
            fit_path = fit_path,
            calibration_timeseries_path = cal_ts_path,
            run_started_utc = run_started_utc,
            run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          )
        }
      }
    }
  }, error = function(e) {
    tibble::tibble(
      catchment_id = catchment_id,
      status = "error",
      reason = as.character(e$message),
      model_type = model_type,
      objective = objective,
      optimizer = NA_character_,
      calibration_years = calibration_years,
      calibration_start_date = format_date_ymd(start_date),
      calibration_end_date = format_date_ymd(calibration_end_date),
      n_rows_peq = NA_integer_,
      n_rows_calibration = NA_integer_,
      n_obs_metrics = NA_integer_,
      KGE = NA_real_,
      NSE = NA_real_,
      RMSE = NA_real_,
      peq_path = NA_character_,
      fit_path = NA_character_,
      calibration_timeseries_path = NA_character_,
      run_started_utc = run_started_utc,
      run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  })

  write_csv_verified(result_row, metrics_path, label = "calibration metrics CSV")

  result_row
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 4) Run selected catchment(s), save outputs and task values

# COMMAND ----------

all_rows <- dplyr::bind_rows(lapply(catchment_ids, run_one_catchment))
status_summary <- all_rows %>% dplyr::count(status, name = "rows")

if (length(catchment_ids) == 1) {
  cid <- catchment_ids[[1]]
  metrics_path <- file.path(ihacres_output_dir, "calibration_metrics", paste0(cid, "_calibration_metrics.csv"))
  safe_set_task_value("calibration_status", all_rows$status[[1]])
  safe_set_task_value("calibration_metrics_path", metrics_path)
  safe_set_task_value("fit_path", all_rows$fit_path[[1]])
} else {
  batch_metrics_path <- file.path(ihacres_output_dir, "calibration_logs", "bulk_calibration_results.csv")
  write_csv_verified(all_rows, batch_metrics_path, label = "bulk calibration metrics CSV")
  safe_set_task_value("calibration_status", ifelse(all(all_rows$status == "ok"), "ok", "mixed"))
  safe_set_task_value("calibration_metrics_path", batch_metrics_path)
  safe_set_task_value("fit_path", "")
}

print(status_summary)
print(all_rows)
