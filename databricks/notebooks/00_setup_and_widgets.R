# Databricks notebook source
# MAGIC %md
# MAGIC # IHACRES Setup: libraries + parameters + catchment fan-out list
# MAGIC This notebook:
# MAGIC 1. Installs required R packages (including hydromad from GitHub if missing)
# MAGIC 2. Creates/reads workflow parameters using Databricks widgets
# MAGIC 3. Builds a validated Korea catchment list and exposes it as task values

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
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
  dbutils.widgets.text("catchment_limit", "110", "Max catchments")
  dbutils.widgets.text("min_obs", "365", "Minimum complete rows")
}

# COMMAND ----------

get_param <- function(name, default) {
  if (exists("dbutils")) return(dbutils.widgets.get(name))
  default
}

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
catchment_limit <- as.integer(get_param("catchment_limit", "110"))
min_obs <- as.integer(get_param("min_obs", "365"))

if (!file.exists(weights_file)) stop(sprintf("Weights file does not exist: %s", weights_file))
if (!dir.exists(precip_dir)) stop(sprintf("Precip directory does not exist: %s", precip_dir))
if (!dir.exists(temp_dir)) stop(sprintf("Temp directory does not exist: %s", temp_dir))
if (!dir.exists(river_dir)) stop(sprintf("River directory does not exist: %s", river_dir))
if (!is.finite(calibration_samples) || calibration_samples <= 0) stop("calibration_samples must be > 0")
if (!is.finite(min_obs) || min_obs < 30) stop("min_obs must be >= 30")
if (end_date < start_date) stop("end_date must be >= start_date")

safe_dir_create(ptq_output_dir)
safe_dir_create(ihacres_output_dir)
safe_dir_create(file.path(ihacres_output_dir, "metrics"))
safe_dir_create(file.path(ihacres_output_dir, "timeseries"))
safe_dir_create(file.path(ihacres_output_dir, "models"))
safe_dir_create(file.path(ihacres_output_dir, "logs"))

message("Installing and validating package dependencies...")
ensure_packages_installed()

library(dplyr)
library(readr)
library(jsonlite)

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

forcing_ids <- union(names(precip_index), names(temp_index))
catchments <- weights %>%
  dplyr::distinct(catchment_id) %>%
  dplyr::arrange(catchment_id) %>%
  dplyr::pull(catchment_id)

has_inputs <- function(cid) {
  opids <- weights$op.id[weights$catchment_id == cid]
  has_forcing <- any(opids %in% forcing_ids, na.rm = TRUE)
  has_q <- cid %in% names(river_index)
  has_forcing && has_q
}

eligible <- catchments[vapply(catchments, has_inputs, logical(1))]

if (is.finite(catchment_limit) && catchment_limit > 0) {
  eligible <- utils::head(eligible, catchment_limit)
}

if (length(eligible) == 0) stop("No eligible catchments were found for the selected configuration")

catchment_ids_json <- jsonlite::toJSON(as.list(eligible), auto_unbox = TRUE)

run_config <- list(
  sub_region = sub_region,
  weights_file = weights_file,
  precip_dir = precip_dir,
  temp_dir = temp_dir,
  river_dir = river_dir,
  ptq_output_dir = ptq_output_dir,
  ihacres_output_dir = ihacres_output_dir,
  start_date = as.character(start_date),
  end_date = as.character(end_date),
  write_csv_out = write_csv_out,
  calibration_samples = calibration_samples,
  optimization_method = optimization_method,
  objective = objective,
  model_type = model_type,
  min_obs = min_obs,
  catchment_count = length(eligible)
)

config_path <- file.path(ihacres_output_dir, "run_config.json")
writeLines(jsonlite::toJSON(run_config, auto_unbox = TRUE, pretty = TRUE), config_path)

safe_set_task_value("catchment_ids_json", catchment_ids_json)
safe_set_task_value("catchment_count", as.character(length(eligible)))
safe_set_task_value("run_config_path", config_path)

message("Setup complete.")
message(sprintf("Region: %s", sub_region))
message(sprintf("Eligible catchments for this run: %s", length(eligible)))
message(sprintf("Task values set: catchment_ids_json, catchment_count, run_config_path"))
message(sprintf("Config written to: %s", config_path))

print(utils::head(eligible, n = min(10, length(eligible))))
