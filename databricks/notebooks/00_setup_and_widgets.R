# Databricks notebook source
# MAGIC %md
# MAGIC # IHACRES Setup: widgets + catchment fan-out list
# MAGIC This notebook prepares run configuration for split calibration/simulation workflow.

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
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
  dbutils.widgets.text("calibration_years", "100", "Calibration period (years)")
  dbutils.widgets.text("simulation_years_csv", "100,1000", "Simulation years (CSV)")
  dbutils.widgets.text("days_per_year", "365", "Days per year for windowing")

  dbutils.widgets.dropdown("write_csv_out", "true", c("true", "false"), "Write PEQ CSV output")
  dbutils.widgets.text("calibration_samples", "1000", "fitByOptim samples")
  dbutils.widgets.dropdown("optimization_method", "PORT", c("PORT", "NLOPT_LN_COBYLA"), "Calibration optimization method")
  dbutils.widgets.dropdown("objective", "kge", c("kge", "NSE"), "Objective")
  dbutils.widgets.dropdown("model_type", "snow", c("snow", "cmd"), "Model type (snow/cmd)")
  dbutils.widgets.text("catchment_limit", "110", "Max catchments")
  dbutils.widgets.text("min_obs", "365", "Minimum complete rows")
}

# COMMAND ----------

get_param <- function(name, default) {
  if (exists("dbutils")) return(dbutils.widgets.get(name))
  default
}

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
calibration_years <- parse_int_or_stop(get_param("calibration_years", "100"), "calibration_years", min_value = 1L)
simulation_years <- parse_years_csv(get_param("simulation_years_csv", "1000"), default = c(1000L))
days_per_year <- parse_int_or_stop(get_param("days_per_year", "365"), "days_per_year", min_value = 1L)
write_csv_out <- parse_bool(get_param("write_csv_out", "true"), default = TRUE)
calibration_samples <- parse_int_or_stop(get_param("calibration_samples", "1000"), "calibration_samples", min_value = 1L)
optimization_method <- get_param("optimization_method", "PORT")
objective <- normalize_objective(get_param("objective", "kge"))
model_type <- tolower(get_param("model_type", "snow"))
catchment_limit <- parse_int_or_stop(get_param("catchment_limit", "110"), "catchment_limit", min_value = 1L)
min_obs <- parse_int_or_stop(get_param("min_obs", "365"), "min_obs", min_value = 30L)

if (!(model_type %in% c("snow", "cmd"))) stop("model_type must be one of: snow, cmd")
if (!(tolower(objective) %in% c("kge", "nse"))) stop("objective must be kge or NSE")

calibration_end_date <- window_end_from_years(start_date, calibration_years, days_per_year = days_per_year)
simulation_end_dates <- vapply(
  simulation_years,
  function(y) as.character(window_end_from_years(start_date, y, days_per_year = days_per_year)),
  character(1)
)

safe_dir_create(ihacres_output_dir)
safe_dir_create(file.path(ihacres_output_dir, "peq"))
safe_dir_create(file.path(ihacres_output_dir, "calibration_models"))
safe_dir_create(file.path(ihacres_output_dir, "calibration_metrics"))
safe_dir_create(file.path(ihacres_output_dir, "calibration_timeseries"))
safe_dir_create(file.path(ihacres_output_dir, "calibration_logs"))
safe_dir_create(file.path(ihacres_output_dir, "simulation_metrics"))
safe_dir_create(file.path(ihacres_output_dir, "simulation_timeseries"))
safe_dir_create(file.path(ihacres_output_dir, "simulation_logs"))
safe_dir_create(file.path(ihacres_output_dir, "summaries"))
safe_dir_create(file.path(ihacres_output_dir, "manifests"))

if (!use_existing_peq) {
  safe_dir_create(ptq_output_dir)
}

message("Installing and validating package dependencies...")
ensure_packages_installed()

library(dplyr)
library(readr)
library(jsonlite)

catalog <- prepare_source_catalog(
  use_existing_peq = use_existing_peq,
  sub_region = sub_region,
  weights_file = weights_file,
  peq_dir = peq_dir,
  precip_dir = precip_dir,
  temp_dir = temp_dir,
  river_dir = river_dir
)

eligible <- eligible_catchments_from_catalog(catalog, catchment_limit = catchment_limit)
if (length(eligible) == 0) stop("No eligible catchments were found for the selected configuration")

runtime_catalog <- subset_catalog_for_catchments(catalog, eligible)
runtime_catalog_path <- file.path(ihacres_output_dir, "manifests", "runtime_catalog.rds")
catchment_manifest_path <- file.path(ihacres_output_dir, "manifests", "catchment_manifest.csv")
saveRDS(runtime_catalog, runtime_catalog_path)

catchment_manifest <- if (identical(runtime_catalog$mode, "existing_peq")) {
  tibble::tibble(
    catchment_id = eligible,
    source_mode = "existing_peq",
    source_path = unname(runtime_catalog$peq_index[match(eligible, names(runtime_catalog$peq_index))])
  )
} else {
  op_count <- vapply(
    eligible,
    function(cid) sum(runtime_catalog$weights_df$catchment_id == cid, na.rm = TRUE),
    integer(1)
  )
  tibble::tibble(
    catchment_id = eligible,
    source_mode = "build_from_forcing",
    source_path = NA_character_,
    op_count = op_count
  )
}
readr::write_csv(catchment_manifest, catchment_manifest_path)

catchment_ids_json <- jsonlite::toJSON(as.list(eligible), auto_unbox = TRUE)

run_config <- list(
  sub_region = sub_region,
  use_existing_peq = use_existing_peq,
  peq_dir = peq_dir,
  weights_file = weights_file,
  precip_dir = precip_dir,
  temp_dir = temp_dir,
  river_dir = river_dir,
  ptq_output_dir = ptq_output_dir,
  ihacres_output_dir = ihacres_output_dir,
  start_date = as.character(start_date),
  calibration_years = calibration_years,
  calibration_end_date = as.character(calibration_end_date),
  simulation_years = as.integer(simulation_years),
  simulation_end_dates = as.list(simulation_end_dates),
  days_per_year = days_per_year,
  write_csv_out = write_csv_out,
  calibration_samples = calibration_samples,
  optimization_method = optimization_method,
  objective = objective,
  model_type = model_type,
  min_obs = min_obs,
  catchment_count = length(eligible),
  runtime_catalog_path = runtime_catalog_path,
  catchment_manifest_path = catchment_manifest_path
)

config_path <- file.path(ihacres_output_dir, "run_config.json")
writeLines(jsonlite::toJSON(run_config, auto_unbox = TRUE, pretty = TRUE), config_path)

safe_set_task_value("catchment_ids_json", catchment_ids_json)
safe_set_task_value("catchment_count", as.character(length(eligible)))
safe_set_task_value("run_config_path", config_path)
safe_set_task_value("calibration_end_date", as.character(calibration_end_date))
safe_set_task_value("simulation_years_csv_normalized", paste(simulation_years, collapse = ","))
safe_set_task_value("runtime_catalog_path", runtime_catalog_path)
safe_set_task_value("catchment_manifest_path", catchment_manifest_path)

message("Setup complete.")
message(sprintf("Region: %s", sub_region))
message(sprintf("Mode: %s", ifelse(use_existing_peq, "Use existing PEQ files", "Build PEQ from forcing files")))
message(sprintf("Model type: %s", model_type))
message(sprintf("Calibration years: %s (end: %s)", calibration_years, as.character(calibration_end_date)))
message(sprintf("Simulation years: %s", paste(simulation_years, collapse = ", ")))
message(sprintf("Eligible catchments for this run: %s", length(eligible)))
message(sprintf("Config written to: %s", config_path))
message(sprintf("Runtime catalog written to: %s", runtime_catalog_path))

print(utils::head(eligible, n = min(10, length(eligible))))
