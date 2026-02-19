# Databricks notebook source
# MAGIC %md
# MAGIC # 01 — Build Daily Catchment Mean Precipitation, Temperature & Discharge
# MAGIC
# MAGIC Reads raw simulation RDS files, applies catchment weights, and produces
# MAGIC one output file (RDS + CSV) per catchment containing daily P, T, Q.

# COMMAND ----------

# MAGIC %md
# MAGIC ## Widgets — Configurable Parameters

# COMMAND ----------

dbutils.widgets.text("weights_file",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Catchmentweights/R02_precip_ops_per_catchment.csv",
  "Weights CSV path")

dbutils.widgets.text("precip_dir",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.precip.data/",
  "Precip RDS directory")

dbutils.widgets.text("temp_dir",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.temp.data/",
  "Temp RDS directory")

dbutils.widgets.text("river_dir",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_2016R1/sim.river.data/",
  "River RDS directory")

dbutils.widgets.text("output_dir",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/catchments_daily_PTQ_by_RiverID/KOR/",
  "Output directory")

dbutils.widgets.text("sub_region", "KOR", "Sub-region filter")
dbutils.widgets.text("start_date", "0000-01-01", "Time-series start date (YYYY-MM-DD)")
dbutils.widgets.dropdown("write_csv", "TRUE", c("TRUE", "FALSE"), "Also write CSV?")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Read Widget Values

# COMMAND ----------

library(dplyr)
library(readr)

weights_file  <- dbutils.widgets.get("weights_file")
precip_dir    <- dbutils.widgets.get("precip_dir")
temp_dir      <- dbutils.widgets.get("temp_dir")
river_dir     <- dbutils.widgets.get("river_dir")
output_dir    <- dbutils.widgets.get("output_dir")
sub_region    <- dbutils.widgets.get("sub_region")
start_date    <- as.Date(dbutils.widgets.get("start_date"))
write_csv_out <- as.logical(dbutils.widgets.get("write_csv"))

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# COMMAND ----------

# MAGIC %md
# MAGIC ## Helper: extract numeric vector from RDS

# COMMAND ----------

get_numeric_series_from_rds <- function(rds_path) {
  obj <- readRDS(rds_path)
  if (is.numeric(obj) && is.vector(obj)) return(as.numeric(obj))
  if (is.data.frame(obj)) {
    numcols <- which(sapply(obj, is.numeric))
    if (length(numcols) >= 1) return(as.numeric(obj[[numcols[1]]]))
    if (ncol(obj) == 1) return(as.numeric(obj[[1]]))
  }
  if (is.list(obj)) {
    for (el in obj) {
      if (is.numeric(el)) return(as.numeric(el))
      if (is.data.frame(el)) {
        numcols <- which(sapply(el, is.numeric))
        if (length(numcols) >= 1) return(as.numeric(el[[numcols[1]]]))
      }
    }
  }
  stop(sprintf("Cannot extract numeric data from %s", rds_path))
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Index Available Files

# COMMAND ----------

precip_files <- list.files(precip_dir, pattern = "\\.rds$", full.names = TRUE)
temp_files   <- list.files(temp_dir,   pattern = "\\.rds$", full.names = TRUE)
river_files  <- list.files(river_dir,  pattern = "\\.rds$", full.names = TRUE)

precip_ids <- sub(".*_(G[0-9]+_[0-9]+)\\.rds$", "\\1", basename(precip_files))
temp_ids   <- sub(".*_(G[0-9]+_[0-9]+)\\.rds$", "\\1", basename(temp_files))
river_ids  <- sub(".*_(R[0-9]+_[^\\.]+)\\.rds$", "\\1", basename(river_files))

names(precip_files) <- precip_ids
names(temp_files)   <- temp_ids
names(river_files)  <- river_ids

message(sprintf("Found %d precip, %d temp, and %d river files.",
                length(precip_files), length(temp_files), length(river_files)))

# COMMAND ----------

# MAGIC %md
# MAGIC ## Load Weights & Filter by Sub-region

# COMMAND ----------

weights <- read_csv(weights_file, col_types = cols(.default = "c")) %>%
  filter(sub.region == sub_region) %>%
  mutate(weight = as.numeric(weight))

stopifnot(all(c("catchment_id", "op.id", "weight") %in% names(weights)))

catchment_list <- unique(weights$catchment_id)
message(sprintf("Processing %d catchments for sub-region '%s'.", length(catchment_list), sub_region))

# COMMAND ----------

# MAGIC %md
# MAGIC ## Process All Catchments

# COMMAND ----------

trim_pad <- function(vec, len) {
  if (is.null(vec)) return(rep(NA_real_, len))
  if (length(vec) >= len) return(vec[1:len])
  c(vec, rep(NA_real_, len - length(vec)))
}

processed <- 0
skipped   <- list()

for (cid in catchment_list) {
  tryCatch({
    subw <- weights %>% filter(catchment_id == cid)
    opids <- subw$op.id
    wts   <- as.numeric(subw$weight)

    available <- opids[opids %in% union(names(precip_files), names(temp_files))]
    if (length(available) == 0) {
      skipped[[cid]] <- "No precip/temp files found"
      next
    }

    wts <- wts[match(available, opids)]
    wts_norm <- wts / sum(wts, na.rm = TRUE)

    precip_series <- lapply(available, function(op) {
      if (op %in% names(precip_files)) get_numeric_series_from_rds(precip_files[[op]]) else NULL
    })
    names(precip_series) <- available

    temp_series <- lapply(available, function(op) {
      if (op %in% names(temp_files)) get_numeric_series_from_rds(temp_files[[op]]) else NULL
    })
    names(temp_series) <- available

    lens <- c(sapply(precip_series, length), sapply(temp_series, length))
    lens <- lens[!is.na(lens) & lens > 0]
    if (length(lens) == 0) {
      skipped[[cid]] <- "No valid time series"
      next
    }
    common_len <- min(lens)

    Q_vec <- NULL
    if (cid %in% names(river_files)) {
      Q_vec <- get_numeric_series_from_rds(river_files[[cid]])
      if (!is.null(Q_vec)) common_len <- min(common_len, length(Q_vec))
    }

    precip_mat <- do.call(cbind, lapply(precip_series, trim_pad, len = common_len))
    temp_mat   <- do.call(cbind, lapply(temp_series, trim_pad, len = common_len))

    ncols_p <- ncol(precip_mat)
    ncols_t <- ncol(temp_mat)
    weight_mat_p <- matrix(rep(wts_norm, each = common_len), nrow = common_len, ncol = ncols_p)
    weight_mat_t <- matrix(rep(wts_norm, each = common_len), nrow = common_len, ncol = ncols_t)

    precip_mat_filled <- replace(precip_mat, is.na(precip_mat), 0)
    precip_sum  <- rowSums(precip_mat_filled * weight_mat_p, na.rm = TRUE)
    denom_p     <- rowSums(!is.na(precip_mat) * weight_mat_p)
    precip_mean <- ifelse(denom_p > 0, precip_sum, NA_real_)

    temp_mat_filled <- replace(temp_mat, is.na(temp_mat), 0)
    temp_sum  <- rowSums(temp_mat_filled * weight_mat_t, na.rm = TRUE)
    denom_t   <- rowSums(!is.na(temp_mat) * weight_mat_t)
    temp_mean <- ifelse(denom_t > 0, temp_sum, NA_real_)

    Q_final <- if (!is.null(Q_vec)) trim_pad(Q_vec, common_len) else rep(NA_real_, common_len)

    dates <- seq.Date(start_date, by = "day", length.out = common_len)

    df_out <- tibble(
      Date         = dates,
      precip_mean  = precip_mean,
      temp_mean    = temp_mean,
      Q            = Q_final,
      catchment_id = cid
    )

    saveRDS(df_out, file.path(output_dir, paste0(cid, ".rds")))
    if (write_csv_out)
      write_csv(df_out, file.path(output_dir, paste0(cid, ".csv")))

    processed <- processed + 1
    if (processed %% 10 == 0)
      message(sprintf("Processed %d / %d catchments...", processed, length(catchment_list)))

  }, error = function(e) {
    skipped[[cid]] <<- conditionMessage(e)
    message(sprintf("ERROR on catchment %s: %s", cid, conditionMessage(e)))
  })
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Summary

# COMMAND ----------

message(sprintf("Processing complete.  Catchments processed: %d  |  Skipped: %d",
                processed, length(skipped)))

if (length(skipped) > 0) {
  skip_df <- data.frame(
    catchment_id = names(skipped),
    reason       = unlist(skipped),
    stringsAsFactors = FALSE
  )
  print(skip_df)
}

message("Output directory: ", output_dir)

# Write the catchment list for downstream tasks
catchment_list_path <- file.path(output_dir, "catchment_list.csv")
write_csv(
  tibble(catchment_id = catchment_list[!catchment_list %in% names(skipped)]),
  catchment_list_path
)
message("Catchment list written to: ", catchment_list_path)
