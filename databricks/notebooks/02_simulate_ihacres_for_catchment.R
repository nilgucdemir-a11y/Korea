# Databricks notebook source
# MAGIC %md
# MAGIC # Simulate IHACRES for one catchment using calibrated model
# MAGIC Loads saved calibration fit and produces simulation outputs for each year window.

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text("catchment_id", "", "Catchment ID")
  dbutils.widgets.text("sub_region", "KOR", "Sub-region")
  dbutils.widgets.dropdown("use_existing_peq", "true", c("true", "false"), "Use existing PEQ files")
  dbutils.widgets.text("peq_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/catchments_daily_PTQ_by_RiverID/KOR/", "Existing PEQ directory")

  dbutils.widgets.text("weights_file", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Catchmentweights/R02_precip_ops_per_catchment.csv", "Weights CSV (non-PEQ mode)")
  dbutils.widgets.text("precip_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.precip.data/", "Precip RDS directory (non-PEQ mode)")
  dbutils.widgets.text("temp_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.temp.data/", "Temp RDS directory (non-PEQ mode)")
  dbutils.widgets.text("river_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.river.data/", "River RDS directory (non-PEQ mode)")
  dbutils.widgets.text("ptq_output_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/catchments_daily_PTQ_by_RiverID/KOR/", "Built PEQ output directory (non-PEQ mode)")

  dbutils.widgets.text("ihacres_output_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/ihacres_results/KOR_1000y/", "IHACRES output directory")
  dbutils.widgets.text("start_date", "0000-01-01", "Series start date")
  dbutils.widgets.text("simulation_years_csv", "100,1000", "Simulation years (CSV)")
  dbutils.widgets.text("days_per_year", "365", "Days per year for windowing")
  dbutils.widgets.dropdown("objective", "kge", c("kge", "NSE"), "Objective")
  dbutils.widgets.dropdown("model_type", "snow", c("snow", "cmd"), "Model type (snow/cmd)")
  dbutils.widgets.text("min_obs", "365", "Minimum complete rows")
}

# COMMAND ----------

get_param <- function(name, default) {
  if (exists("dbutils")) return(dbutils.widgets.get(name))
  default
}

catchment_id <- get_param("catchment_id", "")
sub_region <- get_param("sub_region", "KOR")
use_existing_peq <- parse_bool(get_param("use_existing_peq", "true"), default = TRUE)
peq_dir <- get_param("peq_dir", "")
weights_file <- get_param("weights_file", "")
precip_dir <- get_param("precip_dir", "")
temp_dir <- get_param("temp_dir", "")
river_dir <- get_param("river_dir", "")
ptq_output_dir <- get_param("ptq_output_dir", "/tmp/ihacres/ptq")
ihacres_output_dir <- get_param("ihacres_output_dir", "/tmp/ihacres/results")
start_date <- parse_date_or_stop(get_param("start_date", "0000-01-01"), "start_date")
simulation_years <- parse_years_csv(get_param("simulation_years_csv", "1000"), default = c(1000L))
days_per_year <- parse_int_or_stop(get_param("days_per_year", "365"), "days_per_year", min_value = 1L)
objective <- normalize_objective(get_param("objective", "kge"))
model_type <- tolower(get_param("model_type", "snow"))
min_obs <- parse_int_or_stop(get_param("min_obs", "365"), "min_obs", min_value = 30L)

if (!nzchar(catchment_id)) stop("catchment_id must be provided")
if (!(model_type %in% c("snow", "cmd"))) stop("model_type must be snow or cmd")
if (!(tolower(objective) %in% c("kge", "nse"))) stop("objective must be kge or NSE")

run_started_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

safe_dir_create(ihacres_output_dir)
safe_dir_create(file.path(ihacres_output_dir, "peq"))
safe_dir_create(file.path(ihacres_output_dir, "simulation_metrics"))
safe_dir_create(file.path(ihacres_output_dir, "simulation_timeseries"))
safe_dir_create(file.path(ihacres_output_dir, "simulation_logs"))

ensure_packages_installed()

library(dplyr)
library(readr)
library(tibble)
library(jsonlite)
library(zoo)
library(hydromad)
library(hydroGOF)

fit_path <- file.path(ihacres_output_dir, "calibration_models", paste0(catchment_id, "_fit.rds"))
peq_cache_path <- file.path(ihacres_output_dir, "peq", paste0(catchment_id, ".rds"))
metrics_path <- file.path(ihacres_output_dir, "simulation_metrics", paste0(catchment_id, "_simulation_metrics.csv"))
log_path <- file.path(ihacres_output_dir, "simulation_logs", paste0(catchment_id, "_simulation_log.json"))

load_or_build_peq <- function() {
  if (file.exists(peq_cache_path)) {
    return(normalize_peq_df(readRDS(peq_cache_path), catchment_id = catchment_id))
  }

  catalog <- prepare_source_catalog(
    use_existing_peq = use_existing_peq,
    sub_region = sub_region,
    weights_file = weights_file,
    peq_dir = peq_dir,
    precip_dir = precip_dir,
    temp_dir = temp_dir,
    river_dir = river_dir
  )

  peq_result <- resolve_peq_for_catchment(catchment_id = catchment_id, catalog = catalog, start_date = start_date)
  if (!identical(peq_result$status, "ok")) {
    stop(peq_result$reason)
  }

  saveRDS(peq_result$data, peq_cache_path)
  peq_result$data
}

result_rows <- tryCatch({
  if (!file.exists(fit_path)) stop(sprintf("Calibration fit file is missing: %s", fit_path))
  fit <- readRDS(fit_path)

  peq_df <- load_or_build_peq()

  rows <- lapply(simulation_years, function(y) {
    end_date <- window_end_from_years(start_date, y, days_per_year = days_per_year)
    ts_result <- prepare_model_ts(
      peq_df = peq_df,
      start_date = start_date,
      end_date = end_date,
      min_obs = min_obs,
      require_q = FALSE
    )

    if (!identical(ts_result$status, "ok")) {
      return(tibble::tibble(
        catchment_id = catchment_id,
        simulation_years = y,
        simulation_start_date = as.character(start_date),
        simulation_end_date = as.character(end_date),
        status = ts_result$status,
        reason = ts_result$reason,
        model_type = model_type,
        objective = objective,
        simulation_method = NA_character_,
        n_rows_simulation = NA_integer_,
        n_obs_metrics = NA_integer_,
        KGE = NA_real_,
        NSE = NA_real_,
        RMSE = NA_real_,
        fit_path = fit_path,
        timeseries_path = NA_character_,
        run_started_utc = run_started_utc,
        run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ))
    }

    sim_result <- simulate_with_fit(
      fit = fit,
      model_ts = ts_result$model_ts,
      model_type = model_type,
      objective = objective
    )

    if (!identical(sim_result$status, "ok")) {
      return(tibble::tibble(
        catchment_id = catchment_id,
        simulation_years = y,
        simulation_start_date = as.character(start_date),
        simulation_end_date = as.character(end_date),
        status = "error",
        reason = sim_result$reason,
        model_type = model_type,
        objective = objective,
        simulation_method = sim_result$method_used,
        n_rows_simulation = ts_result$n_rows,
        n_obs_metrics = NA_integer_,
        KGE = NA_real_,
        NSE = NA_real_,
        RMSE = NA_real_,
        fit_path = fit_path,
        timeseries_path = NA_character_,
        run_started_utc = run_started_utc,
        run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ))
    }

    metrics <- evaluate_simulation_metrics(sim_result$sim_q, sim_result$obs_q)
    ts_path <- file.path(ihacres_output_dir, "simulation_timeseries", paste0(catchment_id, "_sim_", y, "y.csv"))

    sim_ts <- tibble::tibble(
      Date = sim_result$dates,
      Q_obs = sim_result$obs_q,
      Q_sim = sim_result$sim_q,
      catchment_id = catchment_id,
      simulation_years = y
    )
    readr::write_csv(sim_ts, ts_path)

    tibble::tibble(
      catchment_id = catchment_id,
      simulation_years = y,
      simulation_start_date = as.character(start_date),
      simulation_end_date = as.character(end_date),
      status = "ok",
      reason = NA_character_,
      model_type = model_type,
      objective = objective,
      simulation_method = sim_result$method_used,
      n_rows_simulation = ts_result$n_rows,
      n_obs_metrics = metrics$n_obs,
      KGE = as.numeric(metrics$KGE),
      NSE = as.numeric(metrics$NSE),
      RMSE = as.numeric(metrics$RMSE),
      fit_path = fit_path,
      timeseries_path = ts_path,
      run_started_utc = run_started_utc,
      run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  })

  dplyr::bind_rows(rows)
}, error = function(e) {
  tibble::tibble(
    catchment_id = catchment_id,
    simulation_years = NA_integer_,
    simulation_start_date = as.character(start_date),
    simulation_end_date = NA_character_,
    status = "error",
    reason = as.character(e$message),
    model_type = model_type,
    objective = objective,
    simulation_method = NA_character_,
    n_rows_simulation = NA_integer_,
    n_obs_metrics = NA_integer_,
    KGE = NA_real_,
    NSE = NA_real_,
    RMSE = NA_real_,
    fit_path = fit_path,
    timeseries_path = NA_character_,
    run_started_utc = run_started_utc,
    run_finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
})

readr::write_csv(result_rows, metrics_path)

run_log <- list(
  catchment_id = catchment_id,
  status_counts = as.list(table(result_rows$status)),
  metrics_path = metrics_path,
  finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)
writeLines(jsonlite::toJSON(run_log, auto_unbox = TRUE, pretty = TRUE), log_path)

safe_set_task_value("simulation_metrics_path", metrics_path)
safe_set_task_value("simulation_status", ifelse(any(result_rows$status == "ok"), "ok", result_rows$status[[1]]))

print(result_rows)
