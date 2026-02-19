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
  if (is.null(x) || is.na(x) || !nzchar(x)) return(default)
  tolower(trimws(as.character(x))) %in% c("1", "true", "yes", "y")
}

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

parse_date_or_stop <- function(x, field_name) {
  d <- as.Date(x)
  if (is.na(d)) stop(sprintf("Invalid date for %s: %s", field_name, x))
  d
}

# COMMAND ----------

get_numeric_series_from_rds <- function(rds_path) {
  obj <- readRDS(rds_path)

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
      if (is.data.frame(el)) {
        numcols <- which(vapply(el, is.numeric, logical(1)))
        if (length(numcols) >= 1) return(as.numeric(el[[numcols[1]]]))
      }
    }
  }

  stop(sprintf("Cannot extract numeric data from %s", rds_path))
}

# COMMAND ----------

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

# COMMAND ----------

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
    if (op %in% names(precip_files_index)) {
      get_numeric_series_from_rds(precip_files_index[[op]])
    } else {
      NULL
    }
  })
  names(precip_series) <- available

  temp_series <- lapply(available, function(op) {
    if (op %in% names(temp_files_index)) {
      get_numeric_series_from_rds(temp_files_index[[op]])
    } else {
      NULL
    }
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

  df_out <- tibble::tibble(
    Date = dates,
    precip_mean = precip_mean,
    temp_mean = temp_mean,
    Q = q_final,
    catchment_id = catchment_id
  )

  list(status = "ok", data = df_out, reason = NA_character_)
}

# COMMAND ----------

fit_ihacres_model <- function(
  daily_df,
  start_date,
  end_date,
  samples = 1000,
  optimization_method = "PORT",
  objective = "kge",
  model_type = "snow",
  min_obs = 365
) {
  model_df <- dplyr::transmute(
    daily_df,
    Date = as.Date(Date),
    P = precip_mean,
    E = temp_mean,
    Q = Q
  )

  model_df <- dplyr::filter(model_df, !is.na(Date))
  model_ts <- zoo::zoo(model_df[, c("P", "E", "Q")], order.by = model_df$Date)
  model_ts <- zoo::window.zoo(model_ts, start = start_date, end = end_date)
  model_ts <- model_ts[stats::complete.cases(model_ts)]

  if (NROW(model_ts) < min_obs) {
    return(list(status = "skip", reason = sprintf("Not enough complete rows (%s)", NROW(model_ts))))
  }

  model_type <- tolower(model_type)

  if (model_type == "snow") {
    model <- hydromad::hydromad(
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
    model <- hydromad::hydromad(
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

  sim <- tryCatch(stats::fitted(fit), error = function(e) NULL)
  if (is.null(sim)) sim <- tryCatch(stats::predict(fit), error = function(e) NULL)

  if (is.null(sim)) {
    return(list(
      status = "error",
      reason = "Unable to extract simulated streamflow from fitted model"
    ))
  }

  if (inherits(sim, "zoo")) {
    sim_q <- as.numeric(zoo::coredata(sim))
  } else if (is.numeric(sim)) {
    sim_q <- as.numeric(sim)
  } else if (is.matrix(sim) || is.data.frame(sim)) {
    sim_q <- as.numeric(sim[, 1])
  } else {
    return(list(status = "error", reason = "Unsupported simulated streamflow object type"))
  }

  obs_q <- as.numeric(zoo::coredata(model_ts[, "Q"]))
  if (length(sim_q) != length(obs_q)) {
    len <- min(length(sim_q), length(obs_q))
    sim_q <- sim_q[seq_len(len)]
    obs_q <- obs_q[seq_len(len)]
  }

  valid <- is.finite(sim_q) & is.finite(obs_q)
  if (sum(valid) < 20) {
    metrics <- list(KGE = NA_real_, NSE = NA_real_, RMSE = NA_real_)
  } else {
    metrics <- list(
      KGE = hydroGOF::KGE(sim_q[valid], obs_q[valid], na.rm = TRUE),
      NSE = hydroGOF::NSE(sim_q[valid], obs_q[valid], na.rm = TRUE),
      RMSE = hydroGOF::rmse(sim_q[valid], obs_q[valid], na.rm = TRUE)
    )
  }

  list(
    status = "ok",
    fit = fit,
    model_ts = model_ts,
    sim_q = sim_q,
    obs_q = obs_q,
    metrics = metrics,
    optimizer_used = optimizer_used,
    n_obs = sum(valid)
  )
}

# COMMAND ----------

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
