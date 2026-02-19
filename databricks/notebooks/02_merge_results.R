# Databricks notebook source
# MAGIC %md
# MAGIC # Consolidate IHACRES outputs
# MAGIC Combines all per-catchment metric CSV files into one run summary.

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text("ihacres_output_dir", "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/ihacres_results/KOR_1000y/", "IHACRES output directory")
  dbutils.widgets.text("sub_region", "KOR", "Sub-region")
}

# COMMAND ----------

get_param <- function(name, default) {
  if (exists("dbutils")) return(dbutils.widgets.get(name))
  default
}

ihacres_output_dir <- get_param("ihacres_output_dir", "/tmp/ihacres/results")
sub_region <- get_param("sub_region", "KOR")

metrics_dir <- file.path(ihacres_output_dir, "metrics")
if (!dir.exists(metrics_dir)) stop(sprintf("Metrics directory does not exist: %s", metrics_dir))

ensure_packages_installed()
library(readr)
library(dplyr)
library(tibble)
library(purrr)

metric_files <- list.files(metrics_dir, pattern = "_metrics\\.csv$", full.names = TRUE)
if (length(metric_files) == 0) stop(sprintf("No metric files found in: %s", metrics_dir))

all_metrics <- purrr::map_dfr(metric_files, ~ readr::read_csv(.x, show_col_types = FALSE))

summary_by_status <- all_metrics %>%
  dplyr::count(status, name = "catchment_count") %>%
  dplyr::arrange(dplyr::desc(catchment_count))

run_summary <- all_metrics %>%
  dplyr::summarise(
    total_catchments = dplyr::n(),
    successful = sum(status == "ok", na.rm = TRUE),
    skipped = sum(status == "skip", na.rm = TRUE),
    errors = sum(status == "error", na.rm = TRUE),
    mean_KGE = mean(KGE, na.rm = TRUE),
    median_KGE = median(KGE, na.rm = TRUE),
    mean_NSE = mean(NSE, na.rm = TRUE),
    median_NSE = median(NSE, na.rm = TRUE),
    mean_RMSE = mean(RMSE, na.rm = TRUE),
    generated_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ) %>%
  dplyr::mutate(sub_region = sub_region)

all_metrics_path <- file.path(ihacres_output_dir, "ihacres_metrics_all_catchments.csv")
status_summary_path <- file.path(ihacres_output_dir, "ihacres_status_summary.csv")
run_summary_path <- file.path(ihacres_output_dir, "ihacres_run_summary.csv")

readr::write_csv(all_metrics, all_metrics_path)
readr::write_csv(summary_by_status, status_summary_path)
readr::write_csv(run_summary, run_summary_path)

safe_set_task_value("all_metrics_path", all_metrics_path)
safe_set_task_value("run_summary_path", run_summary_path)

message("Consolidation complete.")
message(sprintf("All-metrics file: %s", all_metrics_path))
message(sprintf("Run-summary file: %s", run_summary_path))
print(summary_by_status)
print(run_summary)
