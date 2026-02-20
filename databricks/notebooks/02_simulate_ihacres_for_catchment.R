# Databricks notebook source
# MAGIC %md
# MAGIC # Simulate IHACRES for one catchment using calibrated model
# MAGIC Loads saved calibration fit and produces simulation outputs for each year window.
# MAGIC If `catchment_id` is empty, it runs the full catchment list from setup manifest.

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text("catchment_id", "", "Catchment ID (optional)")
  dbutils.widgets.text("run_config_path", "", "Run config path from setup task")
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 1) Read inputs and run config

# COMMAND ----------

catchment_id_input <- trimws(get_widget_or_default("catchment_id", ""))
run_config_path <- get_widget_or_default("run_config_path", "")

cfg <- read_run_config_or_stop(run_config_path)

sub_region <- toupper(as.character(cfg_value(cfg, "sub_region", "KOR")))
use_existing_peq <- parse_bool(cfg_value(cfg, "use_existing_peq", TRUE), default = TRUE)
peq_dir <- as.character(cfg_value(cfg, "peq_dir", ""))
weights_file <- as.character(cfg_value(cfg, "weights_file", ""))
precip_dir <- as.character(cfg_value(cfg, "precip_dir", ""))
temp_dir <- as.character(cfg_value(cfg, "temp_dir", ""))
river_dir <- as.character(cfg_value(cfg, "river_dir", ""))
ptq_output_dir <- as.character(cfg_value(cfg, "ptq_output_dir", "/tmp/ihacres/ptq"))
ihacres_output_dir <- as.character(cfg_value(cfg, "ihacres_output_dir", "/tmp/ihacres/results"))
catalog_rds_path <- as.character(cfg_value(cfg, "runtime_catalog_path", file.path(ihacres_output_dir, "manifests", "runtime_catalog.rds")))
start_date <- parse_date_or_stop(as.character(cfg_value(cfg, "start_date", "0000-01-01")), "start_date")

simulation_years <- if (!is.null(cfg$simulation_years_csv)) {
  parse_years_csv(cfg$simulation_years_csv, default = c(1000L))
} else if (!is.null(cfg$simulation_years)) {
  parse_years_csv(paste(unlist(cfg$simulation_years), collapse = ","), default = c(1000L))
} else {
  c(1000L)
}

days_per_year <- parse_int_or_stop(as.character(cfg_value(cfg, "days_per_year", 365L)), "days_per_year", min_value = 1L)
objective <- normalize_objective(cfg_value(cfg, "objective", "kge"))
model_type <- tolower(as.character(cfg_value(cfg, "model_type", "snow")))
min_obs <- parse_int_or_stop(as.character(cfg_value(cfg, "min_obs", 365L)), "min_obs", min_value = 30L)

if (!(model_type %in% c("snow", "cmd"))) stop("model_type must be snow or cmd")
if (!(tolower(objective) %in% c("kge", "nse"))) stop("objective must be kge or NSE")

# COMMAND ----------

# MAGIC %md
# MAGIC ## 2) Prepare output folders, libraries, and catchment list

# COMMAND ----------

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

catchment_ids <- if (nzchar(catchment_id_input)) {
  catchment_id_input
} else {
  manifest_path <- as.character(cfg_value(cfg, "catchment_manifest_path", ""))
  if (!nzchar(manifest_path) || !file.exists(manifest_path)) {
    stop("catchment_id was empty and catchment_manifest_path was not available in run config")
  }
  manifest_df <- readr::read_csv(manifest_path, show_col_types = FALSE)
  if (!"catchment_id" %in% names(manifest_df)) {
    stop("catchment manifest does not include catchment_id column")
  }
  ids <- unique(as.character(manifest_df$catchment_id))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) stop("No catchments found in catchment manifest")
  message(sprintf("No catchment_id provided. Running full list from manifest: %s catchments", length(ids)))
  ids
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 3) Simulation helper for one catchment

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

  fit_path <- file.path(ihacres_output_dir, "calibration_models", paste0(catchment_id, "_fit.rds"))
  peq_cache_path <- file.path(ihacres_output_dir, "peq", paste0(catchment_id, ".rds"))
  metrics_path <- file.path(ihacres_output_dir, "simulation_metrics", paste0(catchment_id, "_simulation_metrics.csv"))
  log_path <- file.path(ihacres_output_dir, "simulation_logs", paste0(catchment_id, "_simulation_log.json"))

  load_or_build_peq <- function() {
    if (file.exists(peq_cache_path)) {
      return(normalize_peq_df(readRDS(peq_cache_path), catchment_id = catchment_id))
    }

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

    base_df <- peq_df %>%
      dplyr::transmute(
        Date = as.Date(Date),
        P = as.numeric(P),
        E = as.numeric(E),
        Q = as.numeric(Q)
      ) %>%
      dplyr::filter(!is.na(Date), Date >= start_date) %>%
      dplyr::filter(stats::complete.cases(P, E))

    rows <- lapply(simulation_years, function(y) {
      end_date <- window_end_from_years(start_date, y, days_per_year = days_per_year)
      window_df <- base_df[base_df$Date <= end_date, , drop = FALSE]

      if (nrow(window_df) < min_obs) {
        return(tibble::tibble(
          catchment_id = catchment_id,
          simulation_years = y,
          simulation_start_date = as.character(start_date),
          simulation_end_date = as.character(end_date),
          status = "skip",
          reason = sprintf("Not enough rows after filtering (%s)", nrow(window_df)),
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

      model_ts <- zoo::zoo(window_df[, c("P", "E", "Q")], order.by = window_df$Date)

      sim_result <- simulate_with_fit(
        fit = fit,
        model_ts = model_ts,
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
          n_rows_simulation = nrow(window_df),
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
        n_rows_simulation = nrow(window_df),
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

  result_rows
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 4) Run selected catchment(s), save outputs and task values

# COMMAND ----------

all_rows <- dplyr::bind_rows(lapply(catchment_ids, run_one_catchment))
status_summary <- all_rows %>% dplyr::count(status, name = "rows")

if (length(catchment_ids) == 1) {
  cid <- catchment_ids[[1]]
  metrics_path <- file.path(ihacres_output_dir, "simulation_metrics", paste0(cid, "_simulation_metrics.csv"))
  safe_set_task_value("simulation_metrics_path", metrics_path)
  safe_set_task_value("simulation_status", ifelse(any(all_rows$status == "ok"), "ok", all_rows$status[[1]]))
} else {
  batch_metrics_path <- file.path(ihacres_output_dir, "simulation_logs", "bulk_simulation_results.csv")
  readr::write_csv(all_rows, batch_metrics_path)
  safe_set_task_value("simulation_metrics_path", batch_metrics_path)
  safe_set_task_value("simulation_status", ifelse(all(all_rows$status == "ok"), "ok", "mixed"))
}

print(status_summary)
print(all_rows)
