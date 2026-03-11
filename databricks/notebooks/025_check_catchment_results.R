# Databricks notebook source
# MAGIC %md
# MAGIC # Check per-catchment results and parameters
# MAGIC Simple check step between simulation and merge.
# MAGIC - verifies per-catchment output files exist
# MAGIC - extracts fitted parameters (if fit file exists)
# MAGIC - writes two summary CSV files for review

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text("run_config_path", "", "Run config path from setup task")
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 1) Load config and paths

# COMMAND ----------

run_config_path_input <- get_widget_or_default("run_config_path", "")
cfg <- read_run_config_or_stop(run_config_path_input)
run_config_path <- as.character(if (is.null(cfg$resolved_run_config_path)) run_config_path_input else cfg$resolved_run_config_path)
message(sprintf("Using run_config_path: %s", run_config_path))

ihacres_output_dir <- as.character(cfg_value(cfg, "ihacres_output_dir", ""))
catchment_manifest_path <- as.character(cfg_value(cfg, "catchment_manifest_path", ""))

if (!nzchar(ihacres_output_dir)) stop("ihacres_output_dir missing in run config")
if (!nzchar(catchment_manifest_path)) stop("catchment_manifest_path missing in run config")
if (!file.exists(catchment_manifest_path)) stop(sprintf("catchment manifest not found: %s", catchment_manifest_path))

summary_dir <- file.path(ihacres_output_dir, "summaries")
safe_dir_create(summary_dir)

library(readr)
library(dplyr)
library(tibble)
library(jsonlite)

# COMMAND ----------

# MAGIC %md
# MAGIC ## 2) Build check table + parameter table

# COMMAND ----------

manifest_df <- readr::read_csv(catchment_manifest_path, show_col_types = FALSE)
if (!"catchment_id" %in% names(manifest_df)) stop("catchment manifest must include catchment_id")

catchment_ids <- unique(as.character(manifest_df$catchment_id))
catchment_ids <- catchment_ids[nzchar(catchment_ids)]
if (length(catchment_ids) == 0) stop("No catchments in manifest")

rows <- lapply(catchment_ids, function(cid) {
  fit_path <- file.path(ihacres_output_dir, "calibration_models", paste0(cid, "_fit.rds"))
  cal_metrics_path <- file.path(ihacres_output_dir, "calibration_metrics", paste0(cid, "_calibration_metrics.csv"))
  sim_metrics_path <- file.path(ihacres_output_dir, "simulation_metrics", paste0(cid, "_simulation_metrics.csv"))

  fit_exists <- file.exists(fit_path)
  cal_exists <- file.exists(cal_metrics_path)
  sim_exists <- file.exists(sim_metrics_path)

  coef_json <- NA_character_
  coef_count <- NA_integer_

  if (fit_exists) {
    coef_vals <- tryCatch(
      stats::coef(readRDS(fit_path)),
      error = function(e) NULL
    )
    if (!is.null(coef_vals)) {
      coef_json <- jsonlite::toJSON(as.list(coef_vals), auto_unbox = TRUE)
      coef_count <- length(coef_vals)
    }
  }

  tibble::tibble(
    catchment_id = cid,
    fit_exists = fit_exists,
    calibration_metrics_exists = cal_exists,
    simulation_metrics_exists = sim_exists,
    parameters_count = coef_count,
    parameters_json = coef_json
  )
})

check_df <- dplyr::bind_rows(rows)

param_df <- check_df %>%
  dplyr::filter(!is.na(parameters_json)) %>%
  dplyr::select(catchment_id, parameters_count, parameters_json)

check_path <- file.path(summary_dir, "catchment_results_check.csv")
params_path <- file.path(summary_dir, "catchment_parameter_summary.csv")

readr::write_csv(check_df, check_path)
readr::write_csv(param_df, params_path)

safe_set_task_value("catchment_results_check_path", check_path)
safe_set_task_value("catchment_parameter_summary_path", params_path)

# COMMAND ----------

# MAGIC %md
# MAGIC ## 3) Display quick summary

# COMMAND ----------

status_counts <- check_df %>%
  dplyr::summarise(
    total = dplyr::n(),
    fit_exists = sum(fit_exists, na.rm = TRUE),
    calibration_metrics_exists = sum(calibration_metrics_exists, na.rm = TRUE),
    simulation_metrics_exists = sum(simulation_metrics_exists, na.rm = TRUE)
  )

print(status_counts)
print(utils::head(check_df, 20))
message(sprintf("Saved check file: %s", check_path))
message(sprintf("Saved parameter file: %s", params_path))
