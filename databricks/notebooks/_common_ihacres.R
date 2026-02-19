# Databricks notebook source

# COMMAND ----------

ensure_packages_installed <- function() {
  required_cran <- c(
    "zoo", "latticeExtra", "polynom", "car", "Hmisc", "reshape",
    "DEoptim", "dream", "hydroGOF", "ggplot2", "nloptr", "dplyr",
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

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

safe_set_task_value <- function(key, value) {
  if (!exists("dbutils")) return(FALSE)
  tryCatch({
    dbutils.jobs.taskValues.set(key = key, value = value)
    TRUE
  }, error = function(e) {
    message(sprintf("Unable to set task value '%s': %s", key, e$message))
    FALSE
  })
}

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
  if (is.null(run_config_path) || !nzchar(as.character(run_config_path))) {
    stop("run_config_path must be provided")
  }
  if (!file.exists(run_config_path)) {
    stop(sprintf("run_config_path does not exist: %s", run_config_path))
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
  }

  cfg <- jsonlite::fromJSON(run_config_path, simplifyVector = FALSE)
  if (!is.list(cfg)) stop("Invalid run config JSON format")
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

eligible_catchments_from_catalog <- function(catalog, catchment_limit = 110L) {
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

prepare_model_ts <- function(peq_df, start_date, end_date, min_obs = 365L, require_q = TRUE) {
  df <- peq_df %>%
    dplyr::transmute(
      Date = as.Date(Date),
      P = as.numeric(P),
      E = as.numeric(E),
      Q = as.numeric(Q)
    ) %>%
    dplyr::filter(!is.na(Date), Date >= start_date, Date <= end_date)

  if (nrow(df) == 0) {
    return(list(status = "skip", reason = "No rows in selected date window"))
  }

  if (require_q) {
    df <- dplyr::filter(df, stats::complete.cases(P, E, Q))
  } else {
    df <- dplyr::filter(df, stats::complete.cases(P, E))
  }

  if (nrow(df) < min_obs) {
    return(list(status = "skip", reason = sprintf("Not enough rows after filtering (%s)", nrow(df))))
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
  optimizer_used <- optimization_method

  fit <- tryCatch(
    hydromad::fitByOptim(model, samples = samples, method = optimization_method),
    error = function(err) {
      if (toupper(optimization_method) != "PORT") {
        message(sprintf("Optimizer '%s' failed (%s). Falling back to PORT.", optimization_method, err$message))
        optimizer_used <<- "PORT"
        return(hydromad::fitByOptim(model, samples = samples, method = "PORT"))
      }
      stop(err)
    }
  )

  list(status = "ok", fit = fit, optimizer_used = optimizer_used)
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
  sim_obj <- tryCatch(stats::predict(fit, newdata = model_ts), error = function(e) NULL)
  method_used <- "predict_fit_newdata"

  if (is.null(sim_obj)) {
    pars <- tryCatch(stats::coef(fit), error = function(e) NULL)
    model <- build_hydromad_model(model_ts = model_ts, model_type = model_type, objective = objective)
    model_with_pars <- if (is.null(pars)) {
      NULL
    } else {
      tryCatch(do.call(stats::update, c(list(object = model), as.list(pars))), error = function(e) NULL)
    }

    sim_obj <- tryCatch(stats::predict(model_with_pars), error = function(e) NULL)
    method_used <- "predict_model_with_fitted_params"
  }

  if (is.null(sim_obj)) {
    sim_obj <- tryCatch(stats::fitted(fit), error = function(e) NULL)
    method_used <- "fitted_fit_fallback"
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

  list(
    KGE = hydroGOF::KGE(sim_q[valid], obs_q[valid], na.rm = TRUE),
    NSE = hydroGOF::NSE(sim_q[valid], obs_q[valid], na.rm = TRUE),
    RMSE = hydroGOF::rmse(sim_q[valid], obs_q[valid], na.rm = TRUE),
    n_obs = n_obs
  )
}
