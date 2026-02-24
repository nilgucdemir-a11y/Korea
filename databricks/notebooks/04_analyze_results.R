# Databricks notebook source
# MAGIC %md
# MAGIC # Analyze and compare IHACRES results across catchments
# MAGIC This notebook is for interactive analysis only (no result files are written).
# MAGIC It helps you:
# MAGIC - compare catchments by calibration/simulation performance
# MAGIC - inspect distributions by simulation year
# MAGIC - visualize hydrographs for a selected catchment

# COMMAND ----------

# MAGIC %run ./_common_ihacres

# COMMAND ----------

if (exists("dbutils")) {
  dbutils.widgets.text("run_config_path", "", "Run config path from setup task (optional)")
  dbutils.widgets.text("ihacres_output_dir", "", "Override output directory (optional)")
  dbutils.widgets.text("focus_catchments_csv", "", "Comma-separated catchment IDs (optional)")
  dbutils.widgets.text("analysis_year", "", "Simulation year to compare (optional)")
  dbutils.widgets.dropdown("metric_to_rank", "KGE", c("KGE", "NSE", "RMSE"), "Metric for ranking")
  dbutils.widgets.text("top_n", "12", "Top N catchments to highlight")
  dbutils.widgets.text("selected_catchment", "", "Catchment ID for hydrograph (optional)")
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 1) Read inputs and resolve output paths

# COMMAND ----------

run_config_path_input <- trimws(get_widget_or_default("run_config_path", ""))
ihacres_output_override <- trimws(get_widget_or_default("ihacres_output_dir", ""))
focus_catchments_csv <- trimws(get_widget_or_default("focus_catchments_csv", ""))
analysis_year_input <- trimws(get_widget_or_default("analysis_year", ""))
metric_to_rank <- toupper(trimws(get_widget_or_default("metric_to_rank", "KGE")))
top_n <- parse_int_or_stop(get_widget_or_default("top_n", "12"), "top_n", min_value = 1L)
selected_catchment_input <- trimws(get_widget_or_default("selected_catchment", ""))

if (!(metric_to_rank %in% c("KGE", "NSE", "RMSE"))) {
  stop("metric_to_rank must be one of: KGE, NSE, RMSE")
}

cfg <- read_run_config_or_stop(run_config_path_input)
run_config_path <- as.character(if (is.null(cfg$resolved_run_config_path)) run_config_path_input else cfg$resolved_run_config_path)
ihacres_output_dir <- if (nzchar(ihacres_output_override)) {
  ihacres_output_override
} else {
  as.character(cfg_value(cfg, "ihacres_output_dir", ""))
}

if (!nzchar(ihacres_output_dir)) stop("Unable to resolve ihacres_output_dir")
if (!dir.exists(ihacres_output_dir)) stop(sprintf("ihacres_output_dir does not exist: %s", ihacres_output_dir))

analysis_year <- parse_optional_int(analysis_year_input, "analysis_year", min_value = 1L, default = NA_integer_)
focus_catchments <- if (nzchar(focus_catchments_csv)) {
  parts <- unlist(strsplit(focus_catchments_csv, "[,;\\s]+"))
  unique(parts[nzchar(parts)])
} else {
  character(0)
}

summary_dir <- file.path(ihacres_output_dir, "summaries")
cal_metrics_dir <- file.path(ihacres_output_dir, "calibration_metrics")
sim_metrics_dir <- file.path(ihacres_output_dir, "simulation_metrics")
calibration_ts_dir <- file.path(ihacres_output_dir, "calibration_timeseries")
simulation_ts_dir <- file.path(ihacres_output_dir, "simulation_timeseries")

message(sprintf("Using run_config_path: %s", run_config_path))
message(sprintf("Using ihacres_output_dir: %s", ihacres_output_dir))

# COMMAND ----------

# MAGIC %md
# MAGIC ## 2) Load calibration/simulation metrics

# COMMAND ----------

ensure_packages_installed()

library(readr)
library(dplyr)
library(tibble)
library(ggplot2)

read_metrics_with_fallback <- function(summary_path, metrics_dir, metrics_pattern) {
  if (file.exists(summary_path)) {
    return(readr::read_csv(summary_path, show_col_types = FALSE))
  }

  files <- list.files(metrics_dir, pattern = metrics_pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop(sprintf("No metrics files found in %s with pattern %s", metrics_dir, metrics_pattern))
  }

  dplyr::bind_rows(lapply(files, function(p) readr::read_csv(p, show_col_types = FALSE)))
}

cal_all <- read_metrics_with_fallback(
  summary_path = file.path(summary_dir, "calibration_metrics_all_catchments.csv"),
  metrics_dir = cal_metrics_dir,
  metrics_pattern = "_calibration_metrics\\.csv$"
)

sim_all <- read_metrics_with_fallback(
  summary_path = file.path(summary_dir, "simulation_metrics_all_catchments.csv"),
  metrics_dir = sim_metrics_dir,
  metrics_pattern = "_simulation_metrics\\.csv$"
)

required_cal_cols <- c("catchment_id", "status", "KGE", "NSE", "RMSE")
required_sim_cols <- c("catchment_id", "simulation_years", "status", "KGE", "NSE", "RMSE")
if (!all(required_cal_cols %in% names(cal_all))) {
  stop("Calibration metrics are missing required columns")
}
if (!all(required_sim_cols %in% names(sim_all))) {
  stop("Simulation metrics are missing required columns")
}

sim_all$simulation_years <- suppressWarnings(as.integer(sim_all$simulation_years))
cal_ok <- cal_all %>% dplyr::filter(status == "ok")
sim_ok <- sim_all %>% dplyr::filter(status == "ok", is.finite(simulation_years))

if (nrow(cal_ok) == 0) stop("No successful calibration rows found")
if (nrow(sim_ok) == 0) stop("No successful simulation rows found")

available_years <- sort(unique(sim_ok$simulation_years))
year_to_compare <- if (!is.na(analysis_year) && analysis_year %in% available_years) {
  analysis_year
} else {
  if (!is.na(analysis_year) && !(analysis_year %in% available_years)) {
    message(sprintf("Requested analysis_year=%s not found. Using latest available year.", analysis_year))
  }
  max(available_years)
}

sim_year <- sim_ok %>% dplyr::filter(simulation_years == year_to_compare)
if (nrow(sim_year) == 0) stop(sprintf("No successful simulation rows for year %s", year_to_compare))

# COMMAND ----------

# MAGIC %md
# MAGIC ## 3) Build comparison tables

# COMMAND ----------

rank_by_metric <- function(df, metric_name) {
  vals <- suppressWarnings(as.numeric(df[[metric_name]]))
  decreasing <- metric_name %in% c("KGE", "NSE")
  ord <- order(vals, decreasing = decreasing, na.last = NA)
  df[ord, , drop = FALSE]
}

show_table <- function(df, title, n = 20L) {
  message(title)
  print(utils::head(df, n = n))
  if (exists("display", mode = "function")) {
    try(display(df), silent = TRUE)
  }
}

top_sim <- rank_by_metric(sim_year, metric_to_rank)
top_sim <- utils::head(top_sim, n = min(top_n, nrow(top_sim)))

cal_rank <- rank_by_metric(cal_ok, "KGE")
cal_rank <- utils::head(cal_rank, n = min(top_n, nrow(cal_rank)))

cal_compare <- cal_ok %>%
  dplyr::select(catchment_id, cal_KGE = KGE, cal_NSE = NSE, cal_RMSE = RMSE)

sim_compare <- sim_year %>%
  dplyr::select(catchment_id, sim_KGE = KGE, sim_NSE = NSE, sim_RMSE = RMSE)

compare_df <- dplyr::inner_join(cal_compare, sim_compare, by = "catchment_id")

if (length(focus_catchments) > 0) {
  focus_ids <- unique(focus_catchments[focus_catchments %in% sim_year$catchment_id])
  if (length(focus_ids) == 0) {
    message("focus_catchments_csv provided, but no IDs matched simulation rows for selected year.")
  }
} else {
  focus_ids <- unique(top_sim$catchment_id)
}

focus_ids <- utils::head(focus_ids, n = min(10L, length(focus_ids)))

message(sprintf("Comparison year: %s", year_to_compare))
message(sprintf("Catchments available in selected year: %s", nrow(sim_year)))
message(sprintf("Focus catchments used for trajectory plots: %s", paste(focus_ids, collapse = ", ")))

show_table(top_sim, sprintf("Top catchments by %s for simulation year %s", metric_to_rank, year_to_compare), n = top_n)
show_table(cal_rank, "Top catchments by calibration KGE", n = top_n)
show_table(compare_df, "Calibration vs Simulation comparison (joined by catchment)", n = top_n)

# COMMAND ----------

# MAGIC %md
# MAGIC ## 4) Draw cross-catchment comparison plots

# COMMAND ----------

cal_hist_df <- cal_ok
cal_hist_df$KGE <- suppressWarnings(as.numeric(cal_hist_df$KGE))
cal_hist_df <- cal_hist_df[is.finite(cal_hist_df$KGE), , drop = FALSE]
if (nrow(cal_hist_df) > 0) {
  p_cal_hist <- ggplot(cal_hist_df, aes(x = KGE)) +
    geom_histogram(bins = 30, fill = "#4e79a7", color = "white", alpha = 0.9) +
    labs(
      title = "Calibration KGE distribution across catchments",
      x = "Calibration KGE",
      y = "Catchment count"
    ) +
    theme_minimal()
  print(p_cal_hist)
}

sim_box_df <- sim_ok
sim_box_df$metric_value <- suppressWarnings(as.numeric(sim_box_df[[metric_to_rank]]))
sim_box_df <- sim_box_df[is.finite(sim_box_df$metric_value), , drop = FALSE]
if (nrow(sim_box_df) > 0) {
  p_sim_box <- ggplot(sim_box_df, aes(x = factor(simulation_years), y = metric_value)) +
    geom_boxplot(fill = "#59a14f", alpha = 0.7, outlier.alpha = 0.3) +
    labs(
      title = sprintf("Simulation %s distribution by year window", metric_to_rank),
      x = "Simulation years",
      y = metric_to_rank
    ) +
    theme_minimal()
  print(p_sim_box)
}

scatter_df <- compare_df
scatter_df$cal_KGE <- suppressWarnings(as.numeric(scatter_df$cal_KGE))
scatter_df$sim_KGE <- suppressWarnings(as.numeric(scatter_df$sim_KGE))
scatter_df <- scatter_df[is.finite(scatter_df$cal_KGE) & is.finite(scatter_df$sim_KGE), , drop = FALSE]
if (nrow(scatter_df) > 0) {
  scatter_df$focus_group <- ifelse(scatter_df$catchment_id %in% focus_ids, "focus", "other")
  p_scatter <- ggplot(scatter_df, aes(x = cal_KGE, y = sim_KGE, color = focus_group)) +
    geom_point(alpha = 0.75) +
    geom_smooth(method = "lm", se = FALSE, color = "#e15759", linetype = "dashed") +
    scale_color_manual(values = c("focus" = "#f28e2b", "other" = "#76b7b2")) +
    labs(
      title = sprintf("Calibration KGE vs Simulation KGE (%sy window)", year_to_compare),
      x = "Calibration KGE",
      y = "Simulation KGE",
      color = "Catchment group"
    ) +
    theme_minimal()
  print(p_scatter)
}

trajectory_df <- sim_ok %>% dplyr::filter(catchment_id %in% focus_ids)
trajectory_df$metric_value <- suppressWarnings(as.numeric(trajectory_df[[metric_to_rank]]))
trajectory_df <- trajectory_df[is.finite(trajectory_df$metric_value), , drop = FALSE]
if (nrow(trajectory_df) > 0) {
  p_traj <- ggplot(
    trajectory_df,
    aes(x = simulation_years, y = metric_value, color = catchment_id, group = catchment_id)
  ) +
    geom_line(size = 0.9) +
    geom_point(size = 1.7) +
    labs(
      title = sprintf("Catchment trajectories across simulation years (%s)", metric_to_rank),
      x = "Simulation years",
      y = metric_to_rank,
      color = "Catchment ID"
    ) +
    theme_minimal()
  print(p_traj)
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## 5) Draw hydrograph for one selected catchment

# COMMAND ----------

selected_catchment <- if (nzchar(selected_catchment_input)) {
  selected_catchment_input
} else if (length(focus_ids) > 0) {
  focus_ids[[1]]
} else if (nrow(top_sim) > 0) {
  as.character(top_sim$catchment_id[[1]])
} else {
  ""
}

if (!nzchar(selected_catchment)) {
  message("No catchment is available for hydrograph plotting.")
} else {
  message(sprintf("Hydrograph catchment: %s", selected_catchment))

  cal_ts_path <- file.path(
    calibration_ts_dir,
    paste0(selected_catchment, "_calibration_sim_vs_obs.csv")
  )
  sim_ts_path <- file.path(
    simulation_ts_dir,
    paste0(selected_catchment, "_sim_", year_to_compare, "y.csv")
  )

  hydro_parts <- list()

  if (file.exists(cal_ts_path)) {
    cal_ts <- readr::read_csv(cal_ts_path, show_col_types = FALSE)
    if (all(c("Date", "Q_obs", "Q_sim") %in% names(cal_ts))) {
      hydro_parts[[length(hydro_parts) + 1L]] <- tibble::tibble(
        Date = as.Date(cal_ts$Date),
        flow = as.numeric(cal_ts$Q_obs),
        series = "Calibration Q_obs"
      )
      hydro_parts[[length(hydro_parts) + 1L]] <- tibble::tibble(
        Date = as.Date(cal_ts$Date),
        flow = as.numeric(cal_ts$Q_sim),
        series = "Calibration Q_sim"
      )
    }
  } else {
    message(sprintf("Calibration timeseries file not found: %s", cal_ts_path))
  }

  if (file.exists(sim_ts_path)) {
    sim_ts <- readr::read_csv(sim_ts_path, show_col_types = FALSE)
    if (all(c("Date", "Q_obs", "Q_sim") %in% names(sim_ts))) {
      hydro_parts[[length(hydro_parts) + 1L]] <- tibble::tibble(
        Date = as.Date(sim_ts$Date),
        flow = as.numeric(sim_ts$Q_obs),
        series = sprintf("Simulation(%sy) Q_obs", year_to_compare)
      )
      hydro_parts[[length(hydro_parts) + 1L]] <- tibble::tibble(
        Date = as.Date(sim_ts$Date),
        flow = as.numeric(sim_ts$Q_sim),
        series = sprintf("Simulation(%sy) Q_sim", year_to_compare)
      )
    }
  } else {
    message(sprintf("Simulation timeseries file not found: %s", sim_ts_path))
  }

  if (length(hydro_parts) == 0) {
    message("No hydrograph data found for selected catchment.")
  } else {
    hydro_df <- dplyr::bind_rows(hydro_parts)
    hydro_df <- hydro_df[is.finite(hydro_df$flow) & !is.na(hydro_df$Date), , drop = FALSE]

    if (nrow(hydro_df) == 0) {
      message("Hydrograph series was found but contains no finite flow values.")
    } else {
      p_hydro <- ggplot(hydro_df, aes(x = Date, y = flow, color = series)) +
        geom_line(alpha = 0.85) +
        labs(
          title = sprintf("Hydrograph comparison for catchment %s", selected_catchment),
          x = "Date",
          y = "Flow",
          color = "Series"
        ) +
        theme_minimal()
      print(p_hydro)
    }
  }
}

# COMMAND ----------

# MAGIC %md
# MAGIC Analysis notebook complete.
# MAGIC - No outputs are saved.
# MAGIC - Change widgets to inspect different years/catchments/metrics.
