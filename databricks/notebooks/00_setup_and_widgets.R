# Databricks notebook source
# MAGIC %md
# MAGIC # IHACRES Setup (simple inputs)
# MAGIC User-facing widgets are intentionally minimal:
# MAGIC - country
# MAGIC - model_years
# MAGIC - model_type (snow/cmd)
# MAGIC - start_date
# MAGIC Optional advanced overrides can be supplied as one JSON string.

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text("country", "KOR", "Country code (e.g. KOR)")
  dbutils.widgets.text("model_years", "1000", "Model years (e.g. 1000)")
  dbutils.widgets.dropdown("model_type", "snow", c("snow", "cmd"), "Model type")
  dbutils.widgets.text("start_date", "0000-01-01", "Start date (YYYY-MM-DD)")
  dbutils.widgets.text("advanced_config_json", "", "Optional advanced JSON overrides")
}

# COMMAND ----------

country <- toupper(trimws(get_widget_or_default("country", "KOR")))
model_years <- parse_int_or_stop(get_widget_or_default("model_years", "1000"), "model_years", min_value = 1L)
model_type <- tolower(get_widget_or_default("model_type", "snow"))
start_date <- parse_date_or_stop(get_widget_or_default("start_date", "0000-01-01"), "start_date")
advanced_cfg <- parse_optional_json_list(get_widget_or_default("advanced_config_json", ""), "advanced_config_json")

if (!(model_type %in% c("snow", "cmd"))) stop("model_type must be one of: snow, cmd")

country_defaults <- function(country_code, years, mtype) {
  cc <- toupper(trimws(country_code))

  if (cc == "KOR") {
    return(list(
      sub_region = "KOR",
      use_existing_peq = TRUE,
      peq_dir = "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/catchments_daily_PTQ_by_RiverID/KOR/",
      weights_file = "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Catchmentweights/R02_precip_ops_per_catchment.csv",
      precip_dir = "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.precip.data/",
      temp_dir = "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.temp.data/",
      river_dir = "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.river.data/",
      ptq_output_dir = "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/catchments_daily_PTQ_by_RiverID/KOR/",
      ihacres_output_dir = sprintf(
        "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/ihacres_results/%s_%sy_%s/",
        cc, years, mtype
      ),
      calibration_years = min(100L, years),
      simulation_years = as.integer(c(years)),
      days_per_year = 365L,
      write_csv_out = TRUE,
      calibration_samples = 1000L,
      optimization_method = "PORT",
      objective = "kge",
      catchment_limit = 110L,
      min_obs = 365L
    ))
  }

  list(
    sub_region = cc,
    use_existing_peq = FALSE,
    peq_dir = "",
    weights_file = "",
    precip_dir = "",
    temp_dir = "",
    river_dir = "",
    ptq_output_dir = sprintf("/tmp/ihacres/%s/peq", cc),
    ihacres_output_dir = sprintf("/tmp/ihacres/%s/results_%sy_%s", cc, years, mtype),
    calibration_years = min(100L, years),
    simulation_years = as.integer(c(years)),
    days_per_year = 365L,
    write_csv_out = TRUE,
    calibration_samples = 1000L,
    optimization_method = "PORT",
    objective = "kge",
    catchment_limit = 110L,
    min_obs = 365L
  )
}

cfg <- merge_named_lists(country_defaults(country, model_years, model_type), advanced_cfg)

sub_region <- toupper(as.character(if (is.null(cfg$sub_region)) country else cfg$sub_region))
use_existing_peq <- parse_bool(cfg$use_existing_peq, default = TRUE)
peq_dir <- as.character(if (is.null(cfg$peq_dir)) "" else cfg$peq_dir)
weights_file <- as.character(if (is.null(cfg$weights_file)) "" else cfg$weights_file)
precip_dir <- as.character(if (is.null(cfg$precip_dir)) "" else cfg$precip_dir)
temp_dir <- as.character(if (is.null(cfg$temp_dir)) "" else cfg$temp_dir)
river_dir <- as.character(if (is.null(cfg$river_dir)) "" else cfg$river_dir)
ptq_output_dir <- as.character(if (is.null(cfg$ptq_output_dir)) "/tmp/ihacres/ptq" else cfg$ptq_output_dir)
ihacres_output_dir <- as.character(if (is.null(cfg$ihacres_output_dir)) "/tmp/ihacres/results" else cfg$ihacres_output_dir)
days_per_year <- parse_int_or_stop(as.character(if (is.null(cfg$days_per_year)) "365" else cfg$days_per_year), "days_per_year", min_value = 1L)
write_csv_out <- parse_bool(cfg$write_csv_out, default = TRUE)
calibration_samples <- parse_int_or_stop(as.character(if (is.null(cfg$calibration_samples)) "1000" else cfg$calibration_samples), "calibration_samples", min_value = 1L)
optimization_method <- as.character(if (is.null(cfg$optimization_method)) "PORT" else cfg$optimization_method)
objective <- normalize_objective(if (is.null(cfg$objective)) "kge" else cfg$objective)
model_type <- tolower(as.character(if (is.null(cfg$model_type)) model_type else cfg$model_type))
catchment_limit <- parse_int_or_stop(as.character(if (is.null(cfg$catchment_limit)) "110" else cfg$catchment_limit), "catchment_limit", min_value = 1L)
min_obs <- parse_int_or_stop(as.character(if (is.null(cfg$min_obs)) "365" else cfg$min_obs), "min_obs", min_value = 30L)
calibration_years <- parse_int_or_stop(as.character(if (is.null(cfg$calibration_years)) min(100L, model_years) else cfg$calibration_years), "calibration_years", min_value = 1L)

simulation_years <- if (!is.null(cfg$simulation_years_csv)) {
  parse_years_csv(cfg$simulation_years_csv, default = c(model_years))
} else if (!is.null(cfg$simulation_years)) {
  parse_years_csv(paste(unlist(cfg$simulation_years), collapse = ","), default = c(model_years))
} else {
  as.integer(c(model_years))
}

if (!(model_type %in% c("snow", "cmd"))) stop("model_type must be one of: snow, cmd")
if (!(tolower(objective) %in% c("kge", "nse"))) stop("objective must be kge or NSE")

if (sub_region != "KOR") {
  if (use_existing_peq && !nzchar(peq_dir)) {
    stop("For non-KOR runs with existing PEQ, set peq_dir in advanced_config_json")
  }
  if (!use_existing_peq && (!nzchar(weights_file) || !nzchar(precip_dir) || !nzchar(temp_dir) || !nzchar(river_dir))) {
    stop("For non-KOR runs building from forcing, set weights_file/precip_dir/temp_dir/river_dir in advanced_config_json")
  }
}

calibration_end_date <- window_end_from_years(start_date, calibration_years, days_per_year = days_per_year)
simulation_end_dates <- vapply(simulation_years, function(y) as.character(window_end_from_years(start_date, y, days_per_year = days_per_year)), character(1))

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
library(tibble)

catalog <- prepare_source_catalog(
  use_existing_peq = use_existing_peq,
  sub_region = sub_region,
  weights_file = weights_file,
  peq_dir = peq_dir,
  precip_dir = precip_dir,
  temp_dir = temp_dir,
  river_dir = river_dir
)

inventory <- catchment_inventory_from_catalog(catalog)
eligible <- inventory$eligible_ids
if (is.finite(catchment_limit) && catchment_limit > 0) {
  eligible <- utils::head(eligible, catchment_limit)
}
if (length(eligible) == 0) stop("No eligible catchments were found for the selected configuration")

runtime_catalog <- subset_catalog_for_catchments(catalog, eligible)
runtime_catalog_path <- file.path(ihacres_output_dir, "manifests", "runtime_catalog.rds")
catchment_manifest_path <- file.path(ihacres_output_dir, "manifests", "catchment_manifest.csv")
saveRDS(runtime_catalog, runtime_catalog_path)

parallel_concurrency_limit <- 110L
parallel_tasks_this_run <- min(length(eligible), parallel_concurrency_limit)
region_total_catchments <- as.integer(inventory$region_total)
region_eligible_catchments <- as.integer(inventory$eligible_total)

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
  country = country,
  model_years = model_years,
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
  region_total_catchments = region_total_catchments,
  region_eligible_catchments = region_eligible_catchments,
  parallel_concurrency_limit = parallel_concurrency_limit,
  parallel_tasks_this_run = parallel_tasks_this_run,
  runtime_catalog_path = runtime_catalog_path,
  catchment_manifest_path = catchment_manifest_path,
  start_date_format = "YYYY-MM-DD"
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
safe_set_task_value("region_total_catchments", as.character(region_total_catchments))
safe_set_task_value("region_eligible_catchments", as.character(region_eligible_catchments))
safe_set_task_value("parallel_tasks_this_run", as.character(parallel_tasks_this_run))
safe_set_task_value("parallel_concurrency_limit", as.character(parallel_concurrency_limit))

message("Setup complete.")
message(sprintf("Country/region: %s", sub_region))
message(sprintf("Mode: %s", ifelse(use_existing_peq, "Use existing PEQ files", "Build PEQ from forcing files")))
message(sprintf("Model type: %s", model_type))
message(sprintf("Start date: %s (format: YYYY-MM-DD)", as.character(start_date)))
message(sprintf("Calibration years: %s (end: %s)", calibration_years, as.character(calibration_end_date)))
message(sprintf("Simulation years: %s", paste(simulation_years, collapse = ", ")))
message(sprintf("Eligible catchments for this run: %s", length(eligible)))
message(sprintf("Config written to: %s", config_path))
message(sprintf("Runtime catalog written to: %s", runtime_catalog_path))

message("=== PARALLEL RUN NOTIFICATION ===")
message(sprintf("Region catchments (total): %s", region_total_catchments))
message(sprintf("Region catchments (eligible): %s", region_eligible_catchments))
message(sprintf("Catchments selected for this run: %s", length(eligible)))
message(sprintf("Parallel tasks running at once: %s", parallel_tasks_this_run))
message(sprintf("Workflow concurrency limit: %s", parallel_concurrency_limit))

print(utils::head(eligible, n = min(10, length(eligible))))
