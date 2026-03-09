# Databricks notebook source
# MAGIC %md
# MAGIC # Discover and Prepare Catchments
# MAGIC Utility notebook to:
# MAGIC 1) Discover catchments from input `.rds` files
# MAGIC 2) Report catchment count
# MAGIC 3) Optionally set task values for downstream workflow steps
# MAGIC 4) Convert files to standardized `P/E/Q/Date` structure

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

# MAGIC %md
# MAGIC ## 1) Inputs

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text(
    "input_path",
    "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Input/",
    "Input directory (.rds catchments)"
  )
  dbutils.widgets.text(
    "output_path",
    "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Input/",
    "Output directory (standardized .rds)"
  )
  dbutils.widgets.dropdown("save_as_prefixed", "true", c("true", "false"), "Save as catchment_<id>.rds")
  dbutils.widgets.dropdown("write_task_values", "true", c("true", "false"), "Write task values")
}

input_path <- get_widget_or_default(
  "input_path",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Input/"
)
output_path <- get_widget_or_default(
  "output_path",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Input/"
)
save_as_prefixed <- parse_bool(get_widget_or_default("save_as_prefixed", "true"), default = TRUE)
write_task_values <- parse_bool(get_widget_or_default("write_task_values", "true"), default = TRUE)

if (!dir.exists(input_path)) stop(sprintf("Input path does not exist: %s", input_path))
safe_dir_create(output_path)

# COMMAND ----------

# MAGIC %md
# MAGIC ## 2) Discover catchments and show count

# COMMAND ----------

ensure_packages_installed()
library(jsonlite)

rds_files <- list.files(input_path, pattern = "\\.rds$", full.names = TRUE)
catchment_list <- sort(tools::file_path_sans_ext(basename(rds_files)))
catchment_count <- length(catchment_list)

if (catchment_count == 0) {
  stop(sprintf("No .rds files found in input path: %s", input_path))
}

catchment_list_json <- jsonlite::toJSON(as.list(catchment_list), auto_unbox = TRUE)
catchment_list_csv <- paste(catchment_list, collapse = ",")

message("=== CATCHMENT DISCOVERY ===")
message(sprintf("Input folder: %s", input_path))
message(sprintf("Catchments discovered: %s", catchment_count))

if (write_task_values) {
  safe_set_task_value("catchment_count", as.character(catchment_count))
  safe_set_task_value("catchment_list_json", catchment_list_json)
  safe_set_task_value("catchment_list_csv", catchment_list_csv)
  message("Task values set: catchment_count, catchment_list_json, catchment_list_csv")
}

catchment_df <- data.frame(catchment = catchment_list, stringsAsFactors = FALSE)
if (exists("display")) {
  display(catchment_df)
} else {
  print(utils::head(catchment_df, 30))
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 3) Convert to standardized `P/E/Q/Date` inputs

# COMMAND ----------

library(dplyr)

processed <- 0L
failed <- list()

for (cid in catchment_list) {
  in_rds <- file.path(input_path, paste0(cid, ".rds"))

  out_name <- if (save_as_prefixed) {
    paste0("catchment_", cid, ".rds")
  } else {
    paste0(cid, ".rds")
  }
  out_rds <- file.path(output_path, out_name)

  ok <- tryCatch({
    catchment_data <- readRDS(in_rds)
    standardized <- normalize_peq_df(catchment_data, catchment_id = cid) %>%
      dplyr::select(P, E, Q, Date)

    save_rds_verified(standardized, out_rds, label = "standardized catchment RDS")
    processed <<- processed + 1L
    TRUE
  }, error = function(e) {
    failed[[cid]] <<- as.character(e$message)
    FALSE
  })

  if (ok) {
    message(sprintf("Processed: %s -> %s", cid, out_rds))
  } else {
    message(sprintf("Failed: %s", cid))
  }
}

message("=== CONVERSION SUMMARY ===")
message(sprintf("Processed successfully: %s", processed))
message(sprintf("Failed: %s", length(failed)))

if (length(failed) > 0) {
  fail_df <- data.frame(catchment_id = names(failed), reason = unlist(failed), stringsAsFactors = FALSE)
  print(utils::head(fail_df, 20))
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 4) Preview one converted file

# COMMAND ----------

example_cid <- catchment_list[[1]]
example_name <- if (save_as_prefixed) paste0("catchment_", example_cid, ".rds") else paste0(example_cid, ".rds")
example_rds <- file.path(output_path, example_name)

example_df <- readRDS(example_rds)

if (exists("display")) {
  display(example_df)
} else {
  print(utils::head(example_df, 20))
}

str(example_df)
