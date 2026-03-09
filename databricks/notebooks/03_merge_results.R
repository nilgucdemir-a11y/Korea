# Databricks notebook source
# MAGIC %md
# MAGIC # Consolidate calibration + simulation outputs
# MAGIC Produces comparison-ready summaries by status and simulation years.

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text("run_config_path", "", "Run config path from setup task (optional)")
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 1) Read run config

# COMMAND ----------

run_config_path_input <- get_widget_or_default("run_config_path", "")
cfg <- read_run_config_or_stop(run_config_path_input)
run_config_path <- as.character(if (is.null(cfg$resolved_run_config_path)) run_config_path_input else cfg$resolved_run_config_path)
message(sprintf("Using run_config_path: %s", run_config_path))

ihacres_output_dir <- as.character(cfg_value(cfg, "ihacres_output_dir", "/tmp/ihacres/results"))
sub_region <- as.character(cfg_value(cfg, "sub_region", "KOR"))

cal_metrics_dir <- file.path(ihacres_output_dir, "calibration_metrics")
sim_metrics_dir <- file.path(ihacres_output_dir, "simulation_metrics")
summary_dir <- file.path(ihacres_output_dir, "summaries")

if (!dir.exists(cal_metrics_dir)) stop(sprintf("Calibration metrics directory does not exist: %s", cal_metrics_dir))
if (!dir.exists(sim_metrics_dir)) stop(sprintf("Simulation metrics directory does not exist: %s", sim_metrics_dir))
safe_dir_create(summary_dir)

# COMMAND ----------

# MAGIC %md
# MAGIC ## 2) Load metrics and build summaries

# COMMAND ----------

ensure_packages_installed()

library(readr)
library(dplyr)
library(tibble)
library(purrr)

collect_metric_files <- function(dir_path, pattern, label) {
  files <- sort(list.files(dir_path, pattern = pattern, full.names = TRUE))
  if (length(files) == 0) stop(sprintf("No %s files found", label))

  info <- file.info(files)
  valid_idx <- !is.na(info$size) & info$size > 0 & !is.na(info$isdir) & !info$isdir
  valid_files <- files[valid_idx]
  skipped_files <- files[!valid_idx]

  if (length(skipped_files) > 0) {
    message(sprintf(
      "Skipping %s empty or invalid %s file(s): %s",
      length(skipped_files),
      label,
      paste(basename(skipped_files), collapse = ", ")
    ))
  }

  if (length(valid_files) == 0) {
    stop(sprintf("No non-empty %s files found", label))
  }

  valid_files
}

# Read everything as text first so bind_rows() does not fail when per-file
# type inference disagrees on optional date fields such as calibration_start_date.
read_metrics_csv <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA", "NaN", "NULL")
  ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ dplyr::na_if(trimws(.x), "")
      )
    )
}

coerce_metric_columns <- function(df, integer_cols = character(), numeric_cols = character()) {
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(integer_cols),
        ~ readr::parse_integer(.x, na = c("", "NA", "NaN", "NULL"))
      ),
      dplyr::across(
        dplyr::any_of(numeric_cols),
        ~ readr::parse_double(.x, na = c("", "NA", "NaN", "NULL"))
      )
    )
}

bind_metric_files <- function(files, integer_cols = character(), numeric_cols = character()) {
  purrr::map_dfr(files, read_metrics_csv) %>%
    coerce_metric_columns(integer_cols = integer_cols, numeric_cols = numeric_cols)
}

cal_metric_files <- collect_metric_files(
  cal_metrics_dir,
  pattern = "_calibration_metrics\\.csv$",
  label = "calibration metrics"
)
sim_metric_files <- collect_metric_files(
  sim_metrics_dir,
  pattern = "_simulation_metrics\\.csv$",
  label = "simulation metrics"
)

cal_all <- bind_metric_files(
  cal_metric_files,
  integer_cols = c("calibration_years", "n_rows_peq", "n_rows_calibration", "n_obs_metrics"),
  numeric_cols = c("KGE", "NSE", "RMSE")
)
sim_all <- bind_metric_files(
  sim_metric_files,
  integer_cols = c("simulation_years", "n_rows_simulation", "n_obs_metrics"),
  numeric_cols = c("KGE", "NSE", "RMSE")
)

cal_status_summary <- cal_all %>%
  dplyr::count(status, name = "catchment_count") %>%
  dplyr::arrange(dplyr::desc(catchment_count))

sim_status_summary_by_year <- sim_all %>%
  dplyr::count(simulation_years, status, name = "rows") %>%
  dplyr::arrange(simulation_years, dplyr::desc(rows))

sim_perf_by_year <- sim_all %>%
  dplyr::filter(status == "ok") %>%
  dplyr::group_by(simulation_years) %>%
  dplyr::summarise(
    runs = dplyr::n(),
    mean_KGE = mean(KGE, na.rm = TRUE),
    median_KGE = median(KGE, na.rm = TRUE),
    mean_NSE = mean(NSE, na.rm = TRUE),
    median_NSE = median(NSE, na.rm = TRUE),
    mean_RMSE = mean(RMSE, na.rm = TRUE),
    .groups = "drop"
  )

run_summary <- tibble::tibble(
  sub_region = sub_region,
  calibration_total = nrow(cal_all),
  calibration_ok = sum(cal_all$status == "ok", na.rm = TRUE),
  calibration_skip = sum(cal_all$status == "skip", na.rm = TRUE),
  calibration_error = sum(cal_all$status == "error", na.rm = TRUE),
  simulation_rows_total = nrow(sim_all),
  simulation_ok_rows = sum(sim_all$status == "ok", na.rm = TRUE),
  simulation_skip_rows = sum(sim_all$status == "skip", na.rm = TRUE),
  simulation_error_rows = sum(sim_all$status == "error", na.rm = TRUE),
  generated_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)

cal_all_path <- file.path(summary_dir, "calibration_metrics_all_catchments.csv")
sim_all_path <- file.path(summary_dir, "simulation_metrics_all_catchments.csv")
cal_status_path <- file.path(summary_dir, "calibration_status_summary.csv")
sim_status_by_year_path <- file.path(summary_dir, "simulation_status_summary_by_year.csv")
sim_perf_by_year_path <- file.path(summary_dir, "simulation_performance_by_year.csv")
run_summary_path <- file.path(summary_dir, "run_summary.csv")

# COMMAND ----------

# MAGIC %md
# MAGIC ## 3) Save merged outputs and display summary

# COMMAND ----------

write_csv_verified(cal_all, cal_all_path, label = "summary calibration_metrics_all_catchments CSV")
write_csv_verified(sim_all, sim_all_path, label = "summary simulation_metrics_all_catchments CSV")
write_csv_verified(cal_status_summary, cal_status_path, label = "summary calibration_status_summary CSV")
write_csv_verified(sim_status_summary_by_year, sim_status_by_year_path, label = "summary simulation_status_summary_by_year CSV")
write_csv_verified(sim_perf_by_year, sim_perf_by_year_path, label = "summary simulation_performance_by_year CSV")
write_csv_verified(run_summary, run_summary_path, label = "summary run_summary CSV")
message("Summary output save verification completed.")

safe_set_task_value("calibration_metrics_all_path", cal_all_path)
safe_set_task_value("simulation_metrics_all_path", sim_all_path)
safe_set_task_value("run_summary_path", run_summary_path)

message("Consolidation complete.")
message(sprintf("Run summary: %s", run_summary_path))
print(cal_status_summary)
print(sim_status_summary_by_year)
print(sim_perf_by_year)
print(run_summary)
