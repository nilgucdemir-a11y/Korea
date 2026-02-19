# Databricks notebook source
as_bool <- function(x) tolower(trimws(as.character(x))) %in% c("true", "1", "t", "yes", "y")

dbfs_to_local <- function(path) {
  path <- trimws(path)
  if (grepl("^dbfs:/", path)) sub("^dbfs:/", "/dbfs/", path) else path
}

get_param <- function(name, default = NULL) {
  val <- NULL
  if (exists("dbutils")) {
    suppressWarnings(try(dbutils.widgets.text(name, if (is.null(default)) "" else as.character(default)), silent = TRUE))
    val <- suppressWarnings(tryCatch(dbutils.widgets.get(name), error = function(e) NULL))
  }
  if (is.null(val) || identical(val, "")) {
    env <- Sys.getenv(name, unset = "")
    if (!identical(env, "")) val <- env
  }
  if ((is.null(val) || identical(val, "")) && !is.null(default)) val <- as.character(default)
  val
}

ensure_packages <- function(pkgs) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p)
    }
  }
  invisible(TRUE)
}

ensure_packages(c("dplyr", "readr", "tibble", "jsonlite", "zoo"))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(jsonlite)
})

# COMMAND ----------
# Parameters (widgets for interactive runs; job parameters for Workflow runs)
sub_region <- get_param("sub_region", "KOR")
weights_file <- dbfs_to_local(get_param("weights_file"))
precip_dir <- dbfs_to_local(get_param("precip_dir"))
temp_dir <- dbfs_to_local(get_param("temp_dir"))
river_dir <- dbfs_to_local(get_param("river_dir"))
output_dir <- dbfs_to_local(get_param("output_dir"))
write_csv_out <- as_bool(get_param("write_csv_out", "true"))
start_date_str <- get_param("start_date", "0000-01-01")

precip_id_regex <- get_param("precip_id_regex", ".*_(G[0-9]+_[0-9]+)\\.rds$")
temp_id_regex <- get_param("temp_id_regex", ".*_(G[0-9]+_[0-9]+)\\.rds$")
river_id_regex <- get_param("river_id_regex", ".*_(R[0-9A-Za-z]+_.+)\\.rds$")

start_date <- suppressWarnings(as.Date(start_date_str))
if (is.na(start_date)) {
  message("⚠️ start_date '", start_date_str, "' could not be parsed by as.Date(). Falling back to 0001-01-01.")
  start_date <- as.Date("0001-01-01")
}

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# COMMAND ----------
extract_id <- function(file_path, regex) {
  bn <- basename(file_path)
  m <- regexec(regex, bn, perl = TRUE)
  r <- regmatches(bn, m)[[1]]
  if (length(r) >= 2 && !is.na(r[2]) && nzchar(r[2])) return(r[2])
  sub("\\.rds$", "", bn)
}

get_numeric_series_from_rds <- function(rds_path) {
  obj <- readRDS(rds_path)
  if (is.numeric(obj) && is.vector(obj)) return(as.numeric(obj))
  if (is.data.frame(obj)) {
    numcols <- which(vapply(obj, is.numeric, logical(1)))
    if (length(numcols) >= 1) return(as.numeric(obj[[numcols[1]]]))
    if (ncol(obj) == 1) return(as.numeric(obj[[1]]))
  }
  if (is.list(obj)) {
    for (el in obj) {
      if (is.numeric(el) && is.vector(el)) return(as.numeric(el))
      if (is.data.frame(el)) {
        numcols <- which(vapply(el, is.numeric, logical(1)))
        if (length(numcols) >= 1) return(as.numeric(el[[numcols[1]]]))
      }
    }
  }
  stop(sprintf("Cannot extract numeric vector from %s", rds_path))
}

weighted_mean_na <- function(mat, w) {
  if (is.null(mat) || ncol(mat) == 0) return(rep(NA_real_, nrow(mat)))
  n <- nrow(mat)
  w <- as.numeric(w)
  if (length(w) != ncol(mat)) stop("weights length mismatch")
  w <- w / sum(w, na.rm = TRUE)
  wmat <- matrix(rep(w, each = n), nrow = n, ncol = length(w))
  ok <- !is.na(mat)
  num <- rowSums(ifelse(ok, mat, 0) * wmat)
  den <- rowSums(ifelse(ok, 1, 0) * wmat)
  ifelse(den > 0, num / den, NA_real_)
}

# COMMAND ----------
# Index files
precip_files <- list.files(precip_dir, pattern = "\\.rds$", full.names = TRUE)
temp_files <- list.files(temp_dir, pattern = "\\.rds$", full.names = TRUE)
river_files <- list.files(river_dir, pattern = "\\.rds$", full.names = TRUE)

names(precip_files) <- vapply(precip_files, extract_id, character(1), regex = precip_id_regex)
names(temp_files) <- vapply(temp_files, extract_id, character(1), regex = temp_id_regex)
names(river_files) <- vapply(river_files, extract_id, character(1), regex = river_id_regex)

message("Found ", length(precip_files), " precip, ", length(temp_files), " temp, and ", length(river_files), " river files.")

# COMMAND ----------
# Load weights
weights <- read_csv(weights_file, col_types = cols(.default = "c")) %>%
  filter(.data$sub.region == sub_region) %>%
  mutate(weight = as.numeric(.data$weight))

req_cols <- c("catchment_id", "op.id", "weight")
missing_cols <- setdiff(req_cols, names(weights))
if (length(missing_cols) > 0) stop("Weights file missing columns: ", paste(missing_cols, collapse = ", "))

catchment_list <- sort(unique(weights$catchment_id))
message("Catchments in region ", sub_region, ": ", length(catchment_list))

# COMMAND ----------
# Main loop
processed <- 0
skipped <- list()

for (cid in catchment_list) {
  subw <- weights %>% filter(.data$catchment_id == cid)
  opids <- subw$op.id
  wts_all <- as.numeric(subw$weight)

  ops_p <- opids[opids %in% names(precip_files)]
  ops_t <- opids[opids %in% names(temp_files)]

  if (length(ops_p) == 0 && length(ops_t) == 0) {
    skipped[[cid]] <- "No precip or temp files for any op.id"
    next
  }

  w_p <- if (length(ops_p) > 0) wts_all[match(ops_p, opids)] else numeric(0)
  w_t <- if (length(ops_t) > 0) wts_all[match(ops_t, opids)] else numeric(0)

  precip_series <- if (length(ops_p) > 0) lapply(ops_p, function(op) get_numeric_series_from_rds(precip_files[[op]])) else list()
  temp_series <- if (length(ops_t) > 0) lapply(ops_t, function(op) get_numeric_series_from_rds(temp_files[[op]])) else list()

  lens <- c(
    if (length(precip_series) > 0) vapply(precip_series, length, integer(1)) else integer(0),
    if (length(temp_series) > 0) vapply(temp_series, length, integer(1)) else integer(0)
  )
  if (length(lens) == 0) {
    skipped[[cid]] <- "No valid precip/temp time series extracted"
    next
  }
  common_len <- min(lens)

  Q_vec <- NULL
  if (cid %in% names(river_files)) {
    Q_vec <- tryCatch(get_numeric_series_from_rds(river_files[[cid]]), error = function(e) NULL)
    if (!is.null(Q_vec)) common_len <- min(common_len, length(Q_vec))
  }

  if (common_len <= 0) {
    skipped[[cid]] <- "Common length <= 0"
    next
  }

  trim_to <- function(v, len) v[seq_len(min(len, length(v)))]

  if (length(precip_series) > 0) {
    precip_mat <- do.call(cbind, lapply(precip_series, trim_to, len = common_len))
    precip_mean <- weighted_mean_na(precip_mat, w_p)
  } else {
    precip_mean <- rep(NA_real_, common_len)
  }

  if (length(temp_series) > 0) {
    temp_mat <- do.call(cbind, lapply(temp_series, trim_to, len = common_len))
    temp_mean <- weighted_mean_na(temp_mat, w_t)
  } else {
    temp_mean <- rep(NA_real_, common_len)
  }

  Q_final <- if (!is.null(Q_vec)) trim_to(Q_vec, common_len) else rep(NA_real_, common_len)
  dates <- seq.Date(start_date, by = "day", length.out = common_len)

  df_out <- tibble(
    Date = dates,
    precip_mean = precip_mean,
    temp_mean = temp_mean,
    Q = Q_final,
    catchment_id = cid
  )

  saveRDS(df_out, file.path(output_dir, paste0(cid, ".rds")))
  if (write_csv_out) write_csv(df_out, file.path(output_dir, paste0(cid, ".csv")))

  processed <- processed + 1
  if (processed %% 25 == 0) message("Processed ", processed, " catchments...")
}

message("✅ Daily P/T/Q build complete.")
message("Catchments processed: ", processed)
if (length(skipped) > 0) {
  message("Skipped catchments: ", length(skipped))
  skip_path <- file.path(output_dir, "_skipped_catchments.csv")
  write_csv(tibble(catchment_id = names(skipped), reason = unlist(skipped)), skip_path)
  message("Skip report: ", skip_path)
}

# Expose catchment IDs for the downstream for-each task
catchment_ids_json <- jsonlite::toJSON(catchment_list, auto_unbox = TRUE)
if (exists("dbutils")) {
  suppressWarnings(try(dbutils.jobs.taskValues.set(key = "catchment_ids", value = catchment_ids_json, overwrite = TRUE), silent = TRUE))
}

