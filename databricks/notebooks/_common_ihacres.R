# Databricks notebook source

# COMMAND ----------

# MAGIC %md
# MAGIC ## Common helpers: package install and parsing utilities

# COMMAND ----------

ensure_packages_installed <- function() {
  required_cran <- c(
    "zoo", "latticeExtra", "polynom", "car", "Hmisc", "reshape",
    "DEoptim", "dream", "ggplot2", "nloptr", "dplyr",
    "readr", "tibble", "jsonlite", "purrr"
  )

  missing_cran <- required_cran[!vapply(required_cran, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_cran) > 0) {
    install.packages(missing_cran, repos = "https://cloud.r-project.org")
  }

  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools", repos = "https://cloud.r-project.org")
  }

  if (!requireNamespace("hydromad", quietly = TRUE)) {
    devtools::install_github("hydromad/hydromad", upgrade = "never")
  }

  invisible(TRUE)
}

# COMMAND ----------

parse_bool <- function(x, default = FALSE) {
  if (is.logical(x) && length(x) == 1 && !is.na(x)) return(x)
  if (is.null(x) || is.na(x) || !nzchar(as.character(x))) return(default)
  tolower(trimws(as.character(x))) %in% c("1", "true", "yes", "y")
}

get_widget_or_default <- function(name, default = "") {
  if (!exists("dbutils")) return(default)
  tryCatch({
    v <- dbutils.widgets.get(name)
    if (is.null(v) || !nzchar(as.character(v))) default else v
  }, error = function(e) default)
}

parse_date_or_stop <- function(x, field_name) {
  d <- as.Date(x)
  if (is.na(d)) stop(sprintf("Invalid date for %s: %s", field_name, x))
  d
}

parse_int_or_stop <- function(x, field_name, min_value = 1L, default = NA_integer_) {
  if ((is.null(x) || !nzchar(as.character(x))) && !is.na(default)) return(default)
  v <- suppressWarnings(as.integer(x))
  if (!is.finite(v) || is.na(v) || v < min_value) {
    stop(sprintf("%s must be an integer >= %s", field_name, min_value))
  }
  v
}

parse_optional_int <- function(x, field_name, min_value = 1L, default = NA_integer_) {
  if (is.null(x) || is.na(x) || !nzchar(as.character(x))) return(default)
  parse_int_or_stop(x, field_name = field_name, min_value = min_value, default = default)
}

parse_years_csv <- function(x, default = c(1000L)) {
  txt <- if (is.null(x) || !nzchar(trimws(as.character(x)))) {
    paste(default, collapse = ",")
  } else {
    as.character(x)
  }

  parts <- unlist(strsplit(txt, "[,;\\s]+"))
  parts <- parts[nzchar(parts)]
  years <- suppressWarnings(as.integer(parts))
  years <- years[is.finite(years) & !is.na(years) & years > 0]

  if (length(years) == 0) {
    stop("simulation_years_csv did not contain any positive integer year values")
  }

  sort(unique(years))
}

normalize_objective <- function(x) {
  if (is.null(x) || !nzchar(as.character(x))) return("kge")
  x_l <- tolower(as.character(x))
  if (x_l == "kge") return("kge")
  if (x_l == "nse") return("NSE")
  as.character(x)
}

parse_optional_json_list <- function(x, field_name = "advanced_config_json") {
  if (is.null(x) || !nzchar(trimws(as.character(x)))) return(list())

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(as.character(x), simplifyVector = FALSE),
    error = function(e) stop(sprintf("Invalid JSON in %s: %s", field_name, e$message))
  )

  if (is.null(parsed)) return(list())
  if (!is.list(parsed)) stop(sprintf("%s must decode to a JSON object", field_name))
  parsed
}

merge_named_lists <- function(base_list, override_list) {
  out <- base_list
  if (length(override_list) == 0) return(out)

  for (nm in names(override_list)) {
    out[[nm]] <- override_list[[nm]]
  }
  out
}

window_end_from_years <- function(start_date, years, days_per_year = 365L) {
  start_year <- suppressWarnings(as.integer(format(start_date, "%Y")))
  start_md <- format(start_date, "%m-%d")

  if (is.finite(start_year) && !is.na(start_year) && identical(start_md, "01-01")) {
    candidate <- as.Date(sprintf("%04d-12-31", start_year + years))
    if (!is.na(candidate)) return(candidate)
  }

  start_date + as.integer(years * days_per_year) - 1L
}

format_date_ymd <- function(x) {
  d <- as.Date(x)
  if (is.na(d)) return(as.character(d))
  yr <- suppressWarnings(as.integer(format(d, "%Y")))
  md <- format(d, "%m-%d")
  if (is.finite(yr) && !is.na(yr)) {
    return(sprintf("%04d-%s", yr, md))
  }
  as.character(d)
}

is_zero_year_start <- function(x) {
  d <- as.Date(x)
  yr <- suppressWarnings(as.integer(format(d, "%Y")))
  md <- format(d, "%m-%d")
  is.finite(yr) && !is.na(yr) && yr == 0L && identical(md, "01-01")
}

date_range_from_df <- function(df, date_col = "Date") {
  if (!is.data.frame(df) || !(date_col %in% names(df))) {
    return(list(has_dates = FALSE, min_date = as.Date(NA), max_date = as.Date(NA)))
  }

  dates <- as.Date(df[[date_col]])
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(list(has_dates = FALSE, min_date = as.Date(NA), max_date = as.Date(NA)))
  }

  list(
    has_dates = TRUE,
    min_date = min(dates),
    max_date = max(dates)
  )
}

resolve_analysis_window <- function(
  requested_start_date,
  years,
  days_per_year,
  data_min_date,
  data_max_date,
  auto_align = TRUE
) {
  requested_start <- as.Date(requested_start_date)
  requested_end <- window_end_from_years(requested_start, years, days_per_year = days_per_year)

  # Keep the synthetic zero-year timeline strict when explicitly requested.
  if (is_zero_year_start(requested_start)) {
    return(list(
      start_date = requested_start,
      end_date = requested_end,
      adjusted = FALSE,
      strict_window = TRUE,
      requested_start = requested_start,
      requested_end = requested_end
    ))
  }

  has_overlap <- !(requested_end < data_min_date || requested_start > data_max_date)
  if (has_overlap || !auto_align) {
    return(list(
      start_date = requested_start,
      end_date = requested_end,
      adjusted = FALSE,
      strict_window = FALSE,
      requested_start = requested_start,
      requested_end = requested_end
    ))
  }

  aligned_start <- as.Date(data_min_date)
  aligned_end <- window_end_from_years(aligned_start, years, days_per_year = days_per_year)

  list(
    start_date = aligned_start,
    end_date = aligned_end,
    adjusted = TRUE,
    strict_window = FALSE,
    requested_start = requested_start,
    requested_end = requested_end
  )
}

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

verify_saved_file <- function(
  path,
  label = "file",
  require_non_empty = TRUE,
  min_bytes = 1L,
  max_attempts = 8L,
  initial_wait_seconds = 0.15,
  wait_backoff = 1.6
) {
  p <- as.character(path)
  if (length(p) == 0 || is.na(p[[1]]) || !nzchar(p[[1]])) {
    stop(sprintf("Invalid save path for %s", label))
  }
  p <- p[[1]]

  attempts <- as.integer(max(1L, max_attempts))
  wait_s <- as.numeric(max(0, initial_wait_seconds))
  last_reason <- "unknown"

  for (attempt in seq_len(attempts)) {
    if (!file.exists(p)) {
      last_reason <- sprintf("file missing: %s", p)
    } else {
      info <- file.info(p)
      if (nrow(info) == 0 || is.na(info$size[[1]])) {
        last_reason <- sprintf("unable to read file info: %s", p)
      } else if (require_non_empty && info$size[[1]] < as.integer(min_bytes)) {
        last_reason <- sprintf("file is empty (size=%s): %s", info$size[[1]], p)
      } else {
        return(invisible(TRUE))
      }
    }

    if (attempt < attempts && wait_s > 0) {
      Sys.sleep(wait_s)
      wait_s <- wait_s * as.numeric(wait_backoff)
    }
  }

  warning(sprintf(
    "%s save verification warning after %s attempts: %s",
    label,
    attempts,
    last_reason
  ))
  invisible(FALSE)
}

save_rds_verified <- function(object, path, label = "RDS file") {
  saveRDS(object, path)
  try(verify_saved_file(path, label = label, require_non_empty = TRUE, min_bytes = 1L), silent = TRUE)
  invisible(path)
}

write_csv_verified <- function(df, path, label = "CSV file", ...) {
  readr::write_csv(df, path, ...)
  try(verify_saved_file(path, label = label, require_non_empty = TRUE, min_bytes = 1L), silent = TRUE)
  invisible(path)
}

write_text_verified <- function(text, path, label = "text file") {
  p <- as.character(path)
  if (length(p) == 0 || is.na(p[[1]]) || !nzchar(p[[1]])) {
    warning(sprintf("Invalid save path for %s", label))
    return(invisible(path))
  }
  p <- p[[1]]

  tmp <- sprintf(
    "%s.__tmp__%s_%s",
    p,
    Sys.getpid(),
    format(Sys.time(), "%Y%m%d%H%M%OS6")
  )

  writeLines(text, tmp, useBytes = TRUE)
  try(
    verify_saved_file(tmp, label = sprintf("%s temp file", label), require_non_empty = TRUE, min_bytes = 1L),
    silent = TRUE
  )

  renamed <- tryCatch(file.rename(tmp, p), error = function(e) FALSE)
  if (!isTRUE(renamed)) {
    copied <- tryCatch(file.copy(tmp, p, overwrite = TRUE), error = function(e) FALSE)
    if (!isTRUE(copied)) {
      warning(sprintf("Unable to move temp file into final path for %s: %s", label, p))
      return(invisible(path))
    }
    unlink(tmp, force = TRUE)
  }

  try(verify_saved_file(path, label = label, require_non_empty = TRUE, min_bytes = 1L), silent = TRUE)
  invisible(path)
}

safe_set_task_value <- function(key, value) {
  tryCatch({
    dbutils.jobs.taskValues.set(key = key, value = value)
    TRUE
  }, error = function(e) {
    message(sprintf("Unable to set task value '%s': %s", key, e$message))
    FALSE
  })
}

is_job_run_context <- function() {
  env_hits <- c(
    Sys.getenv("DATABRICKS_JOB_ID", ""),
    Sys.getenv("DATABRICKS_RUN_ID", ""),
    Sys.getenv("DB_IS_JOB_CLUSTER", "")
  )
  any(nzchar(env_hits))
}

set_required_task_value <- function(key, value) {
  ok <- safe_set_task_value(key, value)
  if (!ok) {
    if (!is_job_run_context()) {
      message(sprintf(
        "Skipping required task value '%s' because this does not appear to be a Databricks Job run.",
        key
      ))
      return(invisible(FALSE))
    }
    stop(sprintf(
      "Failed to set required task value '%s'. Ensure notebook runs as a Databricks Job task with taskValues support.",
      key
    ))
  }
  invisible(TRUE)
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Common helpers: data extraction and PEQ/catalog utilities

# COMMAND ----------

get_numeric_series_from_rds <- function(rds_path) {
  obj <- readRDS(rds_path)

  if (inherits(obj, "zoo")) {
    return(as.numeric(zoo::coredata(obj)[, 1]))
  }

  if (is.numeric(obj) && is.vector(obj)) {
    return(as.numeric(obj))
  }

  if (is.data.frame(obj)) {
    numcols <- which(vapply(obj, is.numeric, logical(1)))
    if (length(numcols) >= 1) return(as.numeric(obj[[numcols[1]]]))
    if (ncol(obj) == 1) return(as.numeric(obj[[1]]))
  }

  if (is.matrix(obj) && is.numeric(obj)) {
    return(as.numeric(obj[, 1]))
  }

  if (is.list(obj)) {
    for (el in obj) {
      if (is.numeric(el)) return(as.numeric(el))
      if (inherits(el, "zoo")) return(as.numeric(zoo::coredata(el)[, 1]))
      if (is.data.frame(el)) {
        numcols <- which(vapply(el, is.numeric, logical(1)))
        if (length(numcols) >= 1) return(as.numeric(el[[numcols[1]]]))
      }
    }
  }

  stop(sprintf("Cannot extract numeric data from %s", rds_path))
}

index_rds_files <- function(dir_path, id_pattern) {
  files <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(stats::setNames(character(0), character(0)))

  infer_id <- function(fname) {
    base <- basename(fname)
    candidate <- sub(id_pattern, "\\1", base)
    if (!identical(candidate, base)) return(candidate)

    fallback_match <- regmatches(base, regexpr("G[0-9]+_[0-9]+|R[0-9]+_[^\\.]+", base, perl = TRUE))
    if (length(fallback_match) == 1 && nzchar(fallback_match)) return(fallback_match)

    tools::file_path_sans_ext(base)
  }

  ids <- vapply(files, infer_id, character(1))
  stats::setNames(files, ids)
}

index_peq_files <- function(peq_dir) {
  files <- list.files(peq_dir, pattern = "\\.(rds|csv)$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) return(stats::setNames(character(0), character(0)))

  ext <- tolower(tools::file_ext(files))
  ids <- tools::file_path_sans_ext(basename(files))
  priority <- ifelse(ext == "rds", 1L, 2L)
  ord <- order(priority, ids, basename(files))

  files <- files[ord]
  ids <- ids[ord]
  keep <- !duplicated(ids)
  stats::setNames(files[keep], ids[keep])
}

normalize_peq_df <- function(df, catchment_id = NA_character_) {
  if (inherits(df, "zoo")) {
    df <- data.frame(Date = as.Date(zoo::index(df)), zoo::coredata(df), check.names = FALSE)
  }

  if (is.matrix(df)) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  }

  if (!is.data.frame(df)) {
    stop("PEQ object must be a data.frame, matrix, or zoo object")
  }

  names_lower <- tolower(names(df))
  pick_col <- function(candidates) {
    idx <- which(names_lower %in% tolower(candidates))
    if (length(idx) == 0) return(NA_integer_)
    idx[1]
  }

  date_idx <- pick_col(c("Date", "date", "datetime", "time"))
  p_idx <- pick_col(c("P", "precip_mean", "precip", "rain", "rainfall"))
  e_idx <- pick_col(c("E", "temp_mean", "temp", "temperature", "evap", "et"))
  q_idx <- pick_col(c("Q", "q", "flow", "discharge", "streamflow", "q_obs"))
  c_idx <- pick_col(c("catchment_id", "catchment", "river_id"))

  if (is.na(date_idx) || is.na(p_idx) || is.na(e_idx) || is.na(q_idx)) {
    stop("PEQ data must contain Date/P/E/Q-compatible columns")
  }

  out <- tibble::tibble(
    Date = as.Date(df[[date_idx]]),
    P = as.numeric(df[[p_idx]]),
    E = as.numeric(df[[e_idx]]),
    Q = as.numeric(df[[q_idx]])
  )

  out <- dplyr::filter(out, !is.na(Date))
  out <- dplyr::arrange(out, Date)

  if (is.na(catchment_id) || !nzchar(as.character(catchment_id))) {
    if (!is.na(c_idx)) {
      unique_ids <- unique(as.character(df[[c_idx]]))
      unique_ids <- unique_ids[nzchar(unique_ids)]
      catchment_id <- if (length(unique_ids) > 0) unique_ids[1] else NA_character_
    }
  }

  out$catchment_id <- as.character(catchment_id)
  out
}

read_peq_file <- function(file_path, catchment_id = NA_character_) {
  ext <- tolower(tools::file_ext(file_path))

  obj <- if (ext == "rds") {
    readRDS(file_path)
  } else if (ext == "csv") {
    readr::read_csv(file_path, show_col_types = FALSE)
  } else {
    stop(sprintf("Unsupported PEQ file extension: %s", ext))
  }

  normalize_peq_df(obj, catchment_id = catchment_id)
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Common helpers: source catalogs and run config readers

# COMMAND ----------

load_weights_for_region <- function(weights_file, sub_region) {
  if (!file.exists(weights_file)) {
    stop(sprintf("Weights file does not exist: %s", weights_file))
  }

  weights <- readr::read_csv(weights_file, show_col_types = FALSE, col_types = readr::cols(.default = "c")) %>%
    dplyr::filter(sub.region == sub_region) %>%
    dplyr::mutate(weight = as.numeric(weight))

  required_cols <- c("catchment_id", "op.id", "weight")
  if (!all(required_cols %in% names(weights))) {
    stop("Weights file must contain columns: catchment_id, op.id, weight")
  }

  weights
}

build_daily_ptq_for_catchment <- function(
  catchment_id,
  weights_df,
  precip_files_index,
  temp_files_index,
  river_files_index,
  start_date
) {
  subw <- weights_df[weights_df$catchment_id == catchment_id, , drop = FALSE]

  if (nrow(subw) == 0) {
    return(list(status = "skip", reason = "No weights found for catchment"))
  }

  op_ids <- subw$op.id
  weights <- as.numeric(subw$weight)
  available <- op_ids[op_ids %in% union(names(precip_files_index), names(temp_files_index))]

  if (length(available) == 0) {
    return(list(status = "skip", reason = "No precip/temp files found for op IDs"))
  }

  weights <- weights[match(available, op_ids)]
  weights[is.na(weights) | !is.finite(weights) | weights < 0] <- 0
  if (sum(weights, na.rm = TRUE) <= 0) {
    return(list(status = "skip", reason = "Invalid non-positive weights"))
  }
  weights_norm <- weights / sum(weights, na.rm = TRUE)

  precip_series <- lapply(available, function(op) {
    if (op %in% names(precip_files_index)) get_numeric_series_from_rds(precip_files_index[[op]]) else NULL
  })
  names(precip_series) <- available

  temp_series <- lapply(available, function(op) {
    if (op %in% names(temp_files_index)) get_numeric_series_from_rds(temp_files_index[[op]]) else NULL
  })
  names(temp_series) <- available

  lens <- c(vapply(precip_series, length, numeric(1)), vapply(temp_series, length, numeric(1)))
  lens <- lens[is.finite(lens) & lens > 0]
  if (length(lens) == 0) {
    return(list(status = "skip", reason = "No valid precip/temp time series"))
  }

  common_len <- min(lens)

  q_vec <- NULL
  if (catchment_id %in% names(river_files_index)) {
    q_vec <- get_numeric_series_from_rds(river_files_index[[catchment_id]])
    if (!is.null(q_vec) && length(q_vec) > 0) {
      common_len <- min(common_len, length(q_vec))
    }
  }

  if (!is.finite(common_len) || common_len <= 0) {
    return(list(status = "skip", reason = "Invalid common series length"))
  }

  trim_pad <- function(vec, len) {
    if (is.null(vec)) return(rep(NA_real_, len))
    if (length(vec) >= len) return(as.numeric(vec[1:len]))
    c(as.numeric(vec), rep(NA_real_, len - length(vec)))
  }

  precip_mat <- do.call(cbind, lapply(precip_series, trim_pad, len = common_len))
  temp_mat <- do.call(cbind, lapply(temp_series, trim_pad, len = common_len))

  if (is.null(dim(precip_mat))) precip_mat <- matrix(precip_mat, ncol = 1)
  if (is.null(dim(temp_mat))) temp_mat <- matrix(temp_mat, ncol = 1)

  weight_mat_p <- matrix(rep(weights_norm, each = common_len), nrow = common_len, ncol = ncol(precip_mat))
  weight_mat_t <- matrix(rep(weights_norm, each = common_len), nrow = common_len, ncol = ncol(temp_mat))

  precip_filled <- replace(precip_mat, is.na(precip_mat), 0)
  precip_num <- rowSums(precip_filled * weight_mat_p, na.rm = TRUE)
  precip_den <- rowSums((!is.na(precip_mat)) * weight_mat_p)
  precip_mean <- ifelse(precip_den > 0, precip_num / precip_den, NA_real_)

  temp_filled <- replace(temp_mat, is.na(temp_mat), 0)
  temp_num <- rowSums(temp_filled * weight_mat_t, na.rm = TRUE)
  temp_den <- rowSums((!is.na(temp_mat)) * weight_mat_t)
  temp_mean <- ifelse(temp_den > 0, temp_num / temp_den, NA_real_)

  q_final <- if (!is.null(q_vec)) trim_pad(q_vec, common_len) else rep(NA_real_, common_len)
  dates <- seq.Date(start_date, by = "day", length.out = common_len)

  out <- tibble::tibble(
    Date = dates,
    P = precip_mean,
    E = temp_mean,
    Q = q_final,
    catchment_id = catchment_id
  )

  list(status = "ok", data = out, reason = NA_character_)
}

prepare_source_catalog <- function(
  use_existing_peq,
  sub_region,
  weights_file,
  peq_dir,
  precip_dir,
  temp_dir,
  river_dir
) {
  if (use_existing_peq) {
    if (!dir.exists(peq_dir)) stop(sprintf("PEQ directory does not exist: %s", peq_dir))
    peq_index <- index_peq_files(peq_dir)
    return(list(mode = "existing_peq", peq_index = peq_index))
  }

  if (!dir.exists(precip_dir)) stop(sprintf("Precip directory does not exist: %s", precip_dir))
  if (!dir.exists(temp_dir)) stop(sprintf("Temp directory does not exist: %s", temp_dir))
  if (!dir.exists(river_dir)) stop(sprintf("River directory does not exist: %s", river_dir))

  weights <- load_weights_for_region(weights_file, sub_region)
  precip_index <- index_rds_files(precip_dir, ".*_(G[0-9]+_[0-9]+)\\.rds$")
  temp_index <- index_rds_files(temp_dir, ".*_(G[0-9]+_[0-9]+)\\.rds$")
  river_index <- index_rds_files(river_dir, ".*_(R[0-9]+_[^\\.]+)\\.rds$")

  list(
    mode = "build_from_forcing",
    weights_df = weights,
    precip_files_index = precip_index,
    temp_files_index = temp_index,
    river_files_index = river_index
  )
}

subset_catalog_for_catchments <- function(catalog, catchment_ids) {
  catchment_ids <- unique(as.character(catchment_ids))

  if (identical(catalog$mode, "existing_peq")) {
    keep <- names(catalog$peq_index) %in% catchment_ids
    return(list(mode = "existing_peq", peq_index = catalog$peq_index[keep]))
  }

  if (!identical(catalog$mode, "build_from_forcing")) {
    stop(sprintf("Unsupported catalog mode for subsetting: %s", as.character(catalog$mode)))
  }

  weights_sub <- catalog$weights_df[catalog$weights_df$catchment_id %in% catchment_ids, , drop = FALSE]
  op_ids_needed <- unique(weights_sub$op.id)

  precip_sub <- catalog$precip_files_index[names(catalog$precip_files_index) %in% op_ids_needed]
  temp_sub <- catalog$temp_files_index[names(catalog$temp_files_index) %in% op_ids_needed]
  river_sub <- catalog$river_files_index[names(catalog$river_files_index) %in% catchment_ids]

  list(
    mode = "build_from_forcing",
    weights_df = weights_sub,
    precip_files_index = precip_sub,
    temp_files_index = temp_sub,
    river_files_index = river_sub
  )
}

load_catalog_from_rds <- function(catalog_rds_path) {
  if (is.null(catalog_rds_path) || is.na(catalog_rds_path) || !nzchar(catalog_rds_path)) return(NULL)
  if (!file.exists(catalog_rds_path)) return(NULL)

  obj <- readRDS(catalog_rds_path)
  if (!is.list(obj) || is.null(obj$mode)) {
    stop(sprintf("Invalid catalog file format: %s", catalog_rds_path))
  }
  obj
}

resolve_catalog <- function(
  catalog_rds_path,
  use_existing_peq,
  sub_region,
  weights_file,
  peq_dir,
  precip_dir,
  temp_dir,
  river_dir
) {
  from_disk <- load_catalog_from_rds(catalog_rds_path)
  if (!is.null(from_disk)) return(from_disk)

  prepare_source_catalog(
    use_existing_peq = use_existing_peq,
    sub_region = sub_region,
    weights_file = weights_file,
    peq_dir = peq_dir,
    precip_dir = precip_dir,
    temp_dir = temp_dir,
    river_dir = river_dir
  )
}

read_run_config_or_stop <- function(run_config_path) {
  get_task_value_or_default <- function(task_key, key, default = "") {
    if (!exists("dbutils")) return(default)
    tryCatch({
      as.character(dbutils.jobs.taskValues.get(taskKey = task_key, key = key, debugValue = default))
    }, error = function(e) default)
  }

  pick_latest_existing_path <- function(paths) {
    paths <- unique(as.character(paths))
    paths <- paths[nzchar(paths) & file.exists(paths)]
    if (length(paths) == 0) return("")

    info <- file.info(paths)
    ord <- order(info$mtime, decreasing = TRUE, na.last = NA)
    if (length(ord) == 0) return("")
    paths[ord[[1]]]
  }

  discover_run_config_paths <- function() {
    patterns <- c(
      "/tmp/ihacres/*/results_*/run_config.json",
      "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/ihacres_results/*/run_config.json",
      "/dbfs/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/ihacres_results/*/run_config.json"
    )
    candidates <- unlist(lapply(patterns, Sys.glob), use.names = FALSE)
    unique(candidates[file.exists(candidates)])
  }

  resolve_run_config_path_or_stop <- function(run_config_path_input) {
    requested <- trimws(as.character(if (is.null(run_config_path_input)) "" else run_config_path_input))
    if (nzchar(requested)) {
      if (!file.exists(requested)) {
        stop(sprintf("run_config_path does not exist: %s", requested))
      }
      return(requested)
    }

    upstream_task_keys <- c("setup_environment", "setup")
    for (task_key in upstream_task_keys) {
      from_task_value <- trimws(get_task_value_or_default(task_key, "run_config_path", default = ""))
      if (nzchar(from_task_value) && file.exists(from_task_value)) {
        message(sprintf("run_config_path not provided; using task value from '%s': %s", task_key, from_task_value))
        return(from_task_value)
      }
    }

    from_env <- trimws(Sys.getenv("IHACRES_RUN_CONFIG_PATH", ""))
    if (nzchar(from_env) && file.exists(from_env)) {
      message(sprintf("run_config_path not provided; using IHACRES_RUN_CONFIG_PATH: %s", from_env))
      return(from_env)
    }

    discovered <- discover_run_config_paths()
    latest <- pick_latest_existing_path(discovered)
    if (nzchar(latest)) {
      message(sprintf("run_config_path not provided; auto-detected latest run config: %s", latest))
      return(latest)
    }

    stop(
      "run_config_path must be provided. ",
      "Pass setup output '{{tasks.setup_environment.values.run_config_path}}', ",
      "or set IHACRES_RUN_CONFIG_PATH, or run setup first so auto-discovery can find run_config.json."
    )
  }
  resolved_path <- resolve_run_config_path_or_stop(run_config_path)

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
  }

  cfg <- jsonlite::fromJSON(resolved_path, simplifyVector = FALSE)
  if (!is.list(cfg)) stop("Invalid run config JSON format")
  cfg$resolved_run_config_path <- resolved_path
  cfg
}

cfg_value <- function(cfg, name, default = NULL) {
  if (is.null(cfg[[name]])) return(default)
  v <- cfg[[name]]

  if (is.list(v) && length(v) == 1 && !is.list(v[[1]])) {
    return(v[[1]])
  }

  v
}

load_catchment_ids_from_manifest <- function(manifest_path) {
  if (is.null(manifest_path) || !nzchar(as.character(manifest_path))) {
    stop("catchment_manifest_path is missing")
  }
  if (!file.exists(manifest_path)) {
    stop(sprintf("catchment manifest does not exist: %s", manifest_path))
  }

  manifest_df <- readr::read_csv(manifest_path, show_col_types = FALSE)
  if (!"catchment_id" %in% names(manifest_df)) {
    stop("catchment manifest does not include catchment_id column")
  }

  ids <- unique(as.character(manifest_df$catchment_id))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) {
    stop("No catchments found in catchment manifest")
  }
  ids
}

resolve_requested_catchments <- function(
  catchment_id_input = "",
  catchment_idx_input = "",
  catchment_manifest_path
) {
  ids <- load_catchment_ids_from_manifest(catchment_manifest_path)

  cid_txt <- trimws(as.character(catchment_id_input))
  cidx_txt <- trimws(as.character(catchment_idx_input))

  if (nzchar(cid_txt)) {
    cid_parts <- unlist(strsplit(cid_txt, "[,;\\s]+"))
    cid_parts <- cid_parts[nzchar(cid_parts)]
    if (length(cid_parts) == 0) stop("catchment_id input did not contain valid IDs")
    return(cid_parts)
  }

  if (nzchar(cidx_txt)) {
    idx_parts <- unlist(strsplit(cidx_txt, "[,;\\s]+"))
    idx_parts <- idx_parts[nzchar(idx_parts)]
    idx <- suppressWarnings(as.integer(idx_parts))
    if (any(is.na(idx) | idx < 1 | idx > length(ids))) {
      stop(sprintf("catchment_idx must be between 1 and %s", length(ids)))
    }
    return(ids[idx])
  }

  # Default: all catchments from manifest
  ids
}

eligible_catchments_from_catalog <- function(catalog, catchment_limit = NA_integer_) {
  inventory <- catchment_inventory_from_catalog(catalog)
  catchments <- inventory$eligible_ids

  if (is.finite(catchment_limit) && catchment_limit > 0) {
    catchments <- utils::head(catchments, catchment_limit)
  }

  catchments
}

catchment_inventory_from_catalog <- function(catalog) {
  if (identical(catalog$mode, "existing_peq")) {
    ids <- sort(names(catalog$peq_index))
    return(list(
      region_total = length(ids),
      eligible_total = length(ids),
      eligible_ids = ids
    ))
  }

  if (identical(catalog$mode, "build_from_forcing")) {
    forcing_ids <- union(names(catalog$precip_files_index), names(catalog$temp_files_index))
    catchments_all <- sort(unique(catalog$weights_df$catchment_id))

    has_inputs <- function(cid) {
      opids <- catalog$weights_df$op.id[catalog$weights_df$catchment_id == cid]
      has_forcing <- any(opids %in% forcing_ids, na.rm = TRUE)
      has_q <- cid %in% names(catalog$river_files_index)
      has_forcing && has_q
    }

    eligible <- catchments_all[vapply(catchments_all, has_inputs, logical(1))]
    return(list(
      region_total = length(catchments_all),
      eligible_total = length(eligible),
      eligible_ids = eligible
    ))
  }

  stop(sprintf("Unsupported catalog mode: %s", as.character(catalog$mode)))
}

resolve_peq_for_catchment <- function(catchment_id, catalog, start_date) {
  if (identical(catalog$mode, "existing_peq")) {
    peq_path <- catalog$peq_index[[catchment_id]]
    if (is.null(peq_path) || !nzchar(peq_path)) {
      return(list(status = "skip", reason = "No PEQ file found for catchment"))
    }

    peq_df <- read_peq_file(peq_path, catchment_id = catchment_id)
    return(list(status = "ok", data = peq_df, source = peq_path, reason = NA_character_))
  }

  built <- build_daily_ptq_for_catchment(
    catchment_id = catchment_id,
    weights_df = catalog$weights_df,
    precip_files_index = catalog$precip_files_index,
    temp_files_index = catalog$temp_files_index,
    river_files_index = catalog$river_files_index,
    start_date = start_date
  )

  if (!identical(built$status, "ok")) {
    return(list(status = built$status, reason = built$reason))
  }

  list(status = "ok", data = built$data, source = "built_from_forcing", reason = NA_character_)
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Common helpers: IHACRES model preparation, calibration, simulation

# COMMAND ----------

prepare_model_ts <- function(peq_df, start_date, end_date, min_obs = 365L, require_q = TRUE) {
  df0 <- peq_df %>%
    dplyr::transmute(
      Date = as.Date(Date),
      P = as.numeric(P),
      E = as.numeric(E),
      Q = as.numeric(Q)
    ) %>%
    dplyr::filter(!is.na(Date), Date >= start_date, Date <= end_date)

  if (nrow(df0) == 0) {
    data_range <- date_range_from_df(peq_df, date_col = "Date")
    reason <- if (isTRUE(data_range$has_dates)) {
      sprintf(
        "No rows in selected date window (%s..%s); available dates are %s..%s",
        format_date_ymd(start_date),
        format_date_ymd(end_date),
        format_date_ymd(data_range$min_date),
        format_date_ymd(data_range$max_date)
      )
    } else {
      sprintf(
        "No rows in selected date window (%s..%s); PEQ has no valid dates",
        format_date_ymd(start_date),
        format_date_ymd(end_date)
      )
    }
    return(list(status = "skip", reason = reason))
  }

  df <- df0
  if (require_q) {
    df <- dplyr::filter(df, stats::complete.cases(P, E, Q))
  } else {
    df <- dplyr::filter(df, stats::complete.cases(P, E))
  }

  if (nrow(df) < min_obs) {
    return(list(
      status = "skip",
      reason = sprintf(
        "Not enough rows after filtering (%s of %s in %s..%s; min_obs=%s)",
        nrow(df),
        nrow(df0),
        format_date_ymd(start_date),
        format_date_ymd(end_date),
        min_obs
      )
    ))
  }

  model_ts <- zoo::zoo(df[, c("P", "E", "Q")], order.by = df$Date)
  list(status = "ok", model_ts = model_ts, n_rows = nrow(df), reason = NA_character_)
}

build_hydromad_model <- function(model_ts, model_type = "snow", objective = "kge") {
  model_type <- tolower(model_type)

  if (model_type == "snow") {
    hydromad::hydromad(
      model_ts,
      sma = "snow",
      routing = "expuh",
      objective = objective,
      tau_s = c(10, 300),
      tau_q = c(0.1, 10),
      v_s = c(0, 1),
      v_q = c(0, 0.8),
      delay = c(0, 10),
      d = c(200, 200),
      e = c(0.01, 2),
      f = c(0, 2),
      shape = c(0, 0),
      M_0 = 85,
      Tmax = c(0, 5),
      Tmin = c(-12, 0),
      Tmelt = c(-3, 5),
      kd = c(1, 5),
      rcap = c(0, 1),
      cr = c(0.8, 1.2),
      cs = c(0.8, 1.2)
    )
  } else {
    hydromad::hydromad(
      model_ts,
      sma = "cmd",
      routing = "expuh",
      objective = objective,
      tau_s = c(10, 300),
      tau_q = c(0.1, 10),
      v_s = c(0, 1),
      v_q = c(0, 0.8),
      d = c(200, 200),
      e = c(0.01, 2),
      f = c(0, 2),
      shape = c(0, 0),
      M_0 = 85
    )
  }
}

calibrate_hydromad_model <- function(
  model_ts,
  samples = 1000L,
  optimization_method = "PORT",
  objective = "kge",
  model_type = "snow"
) {
  model <- build_hydromad_model(model_ts = model_ts, model_type = model_type, objective = objective)
  fit <- hydromad::fitByOptim(model, samples = samples, method = optimization_method)
  list(status = "ok", fit = fit, optimizer_used = optimization_method)
}

extract_sim_vector <- function(sim_obj) {
  if (is.null(sim_obj)) return(NULL)

  if (inherits(sim_obj, "zoo")) {
    core <- zoo::coredata(sim_obj)
    if (is.matrix(core)) return(as.numeric(core[, 1]))
    return(as.numeric(core))
  }

  if (is.numeric(sim_obj)) {
    return(as.numeric(sim_obj))
  }

  if (is.matrix(sim_obj) || is.data.frame(sim_obj)) {
    return(as.numeric(sim_obj[, 1]))
  }

  NULL
}

simulate_with_fit <- function(fit, model_ts, model_type = "snow", objective = "kge") {
  sim_obj <- tryCatch(
    stats::predict(fit, newdata = model_ts),
    error = function(e) {
      return(structure(list(error_message = as.character(e$message)), class = "predict_error"))
    }
  )
  method_used <- "predict_fit_newdata"

  if (inherits(sim_obj, "predict_error")) {
    return(list(
      status = "error",
      reason = sprintf("predict(fit, newdata=model_ts) failed: %s", sim_obj$error_message),
      method_used = method_used
    ))
  }

  sim_q <- extract_sim_vector(sim_obj)
  if (is.null(sim_q)) {
    return(list(status = "error", reason = "Unable to extract simulated streamflow", method_used = method_used))
  }

  obs_q <- as.numeric(zoo::coredata(model_ts[, "Q"]))
  dates <- as.Date(zoo::index(model_ts))

  len <- min(length(sim_q), length(obs_q), length(dates))
  if (len <= 0) {
    return(list(status = "error", reason = "No overlapping simulation/observation rows", method_used = method_used))
  }

  list(
    status = "ok",
    sim_q = sim_q[seq_len(len)],
    obs_q = obs_q[seq_len(len)],
    dates = dates[seq_len(len)],
    method_used = method_used
  )
}

evaluate_simulation_metrics <- function(sim_q, obs_q) {
  valid <- is.finite(sim_q) & is.finite(obs_q)
  n_obs <- sum(valid)

  if (n_obs < 20) {
    return(list(KGE = NA_real_, NSE = NA_real_, RMSE = NA_real_, n_obs = n_obs))
  }

  s <- as.numeric(sim_q[valid])
  o <- as.numeric(obs_q[valid])

  # Prefer hydroGOF when available, but keep a base-R fallback to avoid
  # task failures from intermittent package-attach issues on parallel workers.
  if (requireNamespace("hydroGOF", quietly = TRUE)) {
    return(list(
      KGE = hydroGOF::KGE(s, o, na.rm = TRUE),
      NSE = hydroGOF::NSE(s, o, na.rm = TRUE),
      RMSE = hydroGOF::rmse(s, o, na.rm = TRUE),
      n_obs = n_obs
    ))
  }

  rmse <- sqrt(mean((s - o)^2))

  nse_denom <- sum((o - mean(o))^2)
  nse <- if (is.finite(nse_denom) && nse_denom > 0) {
    1 - (sum((s - o)^2) / nse_denom)
  } else {
    NA_real_
  }

  r <- suppressWarnings(stats::cor(s, o))
  alpha <- if (stats::sd(o) > 0) stats::sd(s) / stats::sd(o) else NA_real_
  beta <- if (mean(o) != 0) mean(s) / mean(o) else NA_real_
  kge <- if (all(is.finite(c(r, alpha, beta)))) {
    1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  } else {
    NA_real_
  }

  list(KGE = kge, NSE = nse, RMSE = rmse, n_obs = n_obs)
}
