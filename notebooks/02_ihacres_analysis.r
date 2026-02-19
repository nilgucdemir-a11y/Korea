# Databricks notebook source
# MAGIC %md
# MAGIC # 02 — IHACRES Single-Catchment Analysis
# MAGIC
# MAGIC This notebook is parameterized by **catchment_id** and is designed to be
# MAGIC called in parallel (one instance per catchment) by the orchestrator or
# MAGIC Databricks ForEach task.
# MAGIC
# MAGIC **Models fitted:**
# MAGIC 1. CMD (no-snow) SMA + expuh routing — 100-year calibration window
# MAGIC 2. Snow SMA + expuh routing — 100-year calibration window
# MAGIC 3. Snow SMA + expuh routing — 1000-year full run

# COMMAND ----------

# MAGIC %md
# MAGIC ## Widgets — Runtime Parameters

# COMMAND ----------

dbutils.widgets.text("catchment_id", "", "Catchment ID (e.g. R02_U144_28131)")

dbutils.widgets.text("input_dir",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/catchments_daily_PTQ_by_RiverID/KOR/",
  "Input directory (catchment RDS files)")

dbutils.widgets.text("results_dir",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/ihacres_results/KOR/",
  "Results output directory")

dbutils.widgets.text("cal_end_date",  "0100-12-31", "Calibration end date (100yr window)")
dbutils.widgets.text("run_end_date",  "1000-12-31", "Full run end date (1000yr window)")
dbutils.widgets.text("start_date",    "0000-01-01", "Start date")
dbutils.widgets.text("optim_samples", "1000",       "Optimisation sample count")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Load Parameters & Libraries

# COMMAND ----------

library(hydromad)
library(zoo)
library(hydroGOF)
library(ggplot2)
library(dplyr)
library(readr)
library(jsonlite)

catchment_id  <- dbutils.widgets.get("catchment_id")
input_dir     <- dbutils.widgets.get("input_dir")
results_dir   <- dbutils.widgets.get("results_dir")
cal_end_date  <- dbutils.widgets.get("cal_end_date")
run_end_date  <- dbutils.widgets.get("run_end_date")
start_date    <- dbutils.widgets.get("start_date")
optim_samples <- as.integer(dbutils.widgets.get("optim_samples"))

stopifnot(nchar(catchment_id) > 0)

if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

catch_results_dir <- file.path(results_dir, catchment_id)
if (!dir.exists(catch_results_dir)) dir.create(catch_results_dir, recursive = TRUE)

message(sprintf("=== IHACRES analysis for catchment: %s ===", catchment_id))

# COMMAND ----------

# MAGIC %md
# MAGIC ## Load Catchment Data

# COMMAND ----------

rds_path <- file.path(input_dir, paste0(catchment_id, ".rds"))
stopifnot(file.exists(rds_path))

catchment_data <- readRDS(rds_path)

message(sprintf("Loaded %d rows for catchment %s", nrow(catchment_data), catchment_id))

cat("\nFirst rows:\n")
print(head(catchment_data))

numeric_cols <- sapply(catchment_data, is.numeric)
if (any(numeric_cols)) {
  cat("\nRanges for numeric columns:\n")
  ranges <- sapply(catchment_data[, numeric_cols, drop = FALSE], function(x) {
    c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
  })
  print(ranges)
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Prepare Zoo Time Series

# COMMAND ----------

df_hydro <- catchment_data %>%
  select(precip_mean, temp_mean, Q, Date) %>%
  rename(P = precip_mean, E = temp_mean)

ts_zoo <- zoo(df_hydro[, c("P", "E", "Q")], order.by = df_hydro$Date)

ts_cal  <- window(ts_zoo, start = start_date, end = cal_end_date)
ts_full <- window(ts_zoo, start = start_date, end = run_end_date)

message(sprintf("Calibration window: %s to %s  (%d days)",
                start(ts_cal), end(ts_cal), nrow(ts_cal)))
message(sprintf("Full-run window:    %s to %s  (%d days)",
                start(ts_full), end(ts_full), nrow(ts_full)))

# COMMAND ----------

# MAGIC %md
# MAGIC ## Helper: Save Model Results

# COMMAND ----------

save_model_results <- function(fit, model_label, catch_dir, catchment_id) {

  coef_list <- as.list(coef(fit))
  summ      <- summary(fit)

  gof_stats <- tryCatch({
    obs  <- fit$data[, "Q"]
    sim  <- fitted(fit)
    valid <- complete.cases(obs, sim)
    list(
      NSE  = round(NSE(sim[valid], obs[valid]),  4),
      KGE  = round(KGE(sim[valid], obs[valid]),  4),
      RMSE = round(rmse(sim[valid], obs[valid]), 4),
      PBIAS = round(pbias(sim[valid], obs[valid]), 2)
    )
  }, error = function(e) list(NSE = NA, KGE = NA, RMSE = NA, PBIAS = NA))

  result <- list(
    catchment_id = catchment_id,
    model        = model_label,
    coefficients = coef_list,
    gof          = gof_stats
  )

  json_path <- file.path(catch_dir, paste0(model_label, "_results.json"))
  write(toJSON(result, auto_unbox = TRUE, pretty = TRUE), json_path)

  rds_path <- file.path(catch_dir, paste0(model_label, "_fit.rds"))
  saveRDS(fit, rds_path)

  obs_sim <- tryCatch({
    data.frame(
      Date     = index(fit$data),
      Observed = as.numeric(fit$data[, "Q"]),
      Modelled = as.numeric(fitted(fit))
    )
  }, error = function(e) NULL)

  if (!is.null(obs_sim)) {
    csv_path <- file.path(catch_dir, paste0(model_label, "_obs_sim.csv"))
    write_csv(obs_sim, csv_path)
  }

  message(sprintf("  [%s] NSE=%.4f  KGE=%.4f  RMSE=%.4f  PBIAS=%.2f%%",
                  model_label,
                  gof_stats$NSE, gof_stats$KGE, gof_stats$RMSE, gof_stats$PBIAS))
  return(result)
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Model 1: CMD (no-snow) — Calibration Window (100 yr)

# COMMAND ----------

tryCatch({
  message("Fitting CMD (no-snow) model on calibration window...")

  mod_cmd <- hydromad(
    ts_cal, sma = "cmd", routing = "expuh",
    tau_s = c(10, 300), tau_q = c(0.1, 10),
    v_s   = c(0, 1),    v_q   = c(0, 0.8),
    d     = c(200, 200), e = c(0.01, 2),
    f     = c(0, 2),    shape = c(0, 0),
    M_0   = 85
  )

  fit_cmd <- fitByOptim(mod_cmd, samples = optim_samples, method = "PORT")

  print(fit_cmd)
  print(summary(fit_cmd))

  res_cmd <- save_model_results(fit_cmd, "cmd_cal100", catch_results_dir, catchment_id)

  png(file.path(catch_results_dir, "cmd_cal100_hydrograph.png"), width = 1200, height = 600)
  print(xyplot(fit_cmd, with.P = TRUE, xlim = as.Date(c(start_date, cal_end_date))))
  dev.off()

  png(file.path(catch_results_dir, "cmd_cal100_qqplot.png"), width = 800, height = 600)
  print(qqmath(fit_cmd, type = c("l", "g"),
               scales = list(y = list(log = TRUE)),
               xlab = "Standard normal variate",
               ylab = "Flow (mm/day)",
               f.value = ppoints(100), tails.n = 50,
               as.table = TRUE))
  dev.off()

  message("CMD calibration complete.")
}, error = function(e) {
  message(sprintf("ERROR in CMD model for %s: %s", catchment_id, conditionMessage(e)))
})

# COMMAND ----------

# MAGIC %md
# MAGIC ## Model 2: Snow SMA — Calibration Window (100 yr)

# COMMAND ----------

tryCatch({
  message("Fitting Snow model on calibration window...")

  mod_snow_cal <- hydromad(
    ts_cal, sma = "snow", routing = "expuh",
    tau_s = c(10, 300), tau_q = c(0.1, 10),
    v_s   = c(0, 1),    v_q   = c(0, 0.8),
    delay = c(0, 10),
    d     = c(200, 200), e = c(0.01, 2),
    f     = c(0, 2),    shape = c(0, 0),
    M_0   = 85,
    Tmax  = c(0, 5),    Tmin  = c(-12, 0),
    Tmelt = c(-3, 5),   kd    = c(1, 5),
    rcap  = c(0, 1),    cr    = c(0.8, 1.2),
    cs    = c(0.8, 1.2)
  )

  fit_snow_cal <- fitByOptim(mod_snow_cal, samples = optim_samples, method = "PORT")

  print(fit_snow_cal)
  print(summary(fit_snow_cal))

  res_snow_cal <- save_model_results(fit_snow_cal, "snow_cal100", catch_results_dir, catchment_id)

  png(file.path(catch_results_dir, "snow_cal100_hydrograph.png"), width = 1200, height = 600)
  print(xyplot(fit_snow_cal, with.P = TRUE, xlim = as.Date(c(start_date, cal_end_date))))
  dev.off()

  png(file.path(catch_results_dir, "snow_cal100_qqplot.png"), width = 800, height = 600)
  print(qqmath(fit_snow_cal, type = c("l", "g"),
               scales = list(y = list(log = TRUE)),
               xlab = "Standard normal variate",
               ylab = "Flow (mm/day)",
               f.value = ppoints(100), tails.n = 50,
               as.table = TRUE))
  dev.off()

  message("Snow calibration (100yr) complete.")
}, error = function(e) {
  message(sprintf("ERROR in Snow-cal model for %s: %s", catchment_id, conditionMessage(e)))
})

# COMMAND ----------

# MAGIC %md
# MAGIC ## Model 3: Snow SMA — Full 1000-year Run

# COMMAND ----------

tryCatch({
  message("Fitting Snow model on full 1000-year window...")

  mod_snow_full <- hydromad(
    ts_full, sma = "snow", routing = "expuh",
    tau_s = c(10, 300), tau_q = c(0.1, 10),
    v_s   = c(0, 1),    v_q   = c(0, 0.8),
    delay = c(0, 10),
    d     = c(200, 200), e = c(0.01, 2),
    f     = c(0, 2),    shape = c(0, 0),
    M_0   = 85,
    Tmax  = c(0, 5),    Tmin  = c(-12, 0),
    Tmelt = c(-3, 5),   kd    = c(1, 5),
    rcap  = c(0, 1),    cr    = c(0.8, 1.2),
    cs    = c(0.8, 1.2)
  )

  fit_snow_full <- fitByOptim(mod_snow_full, samples = optim_samples, method = "PORT")

  print(fit_snow_full)
  print(summary(fit_snow_full))

  res_snow_full <- save_model_results(fit_snow_full, "snow_full1000", catch_results_dir, catchment_id)

  png(file.path(catch_results_dir, "snow_full1000_hydrograph.png"), width = 1200, height = 600)
  print(xyplot(fit_snow_full, with.P = TRUE, xlim = as.Date(c(start_date, run_end_date))))
  dev.off()

  png(file.path(catch_results_dir, "snow_full1000_qqplot.png"), width = 800, height = 600)
  print(qqmath(fit_snow_full, type = c("l", "g"),
               scales = list(y = list(log = TRUE)),
               xlab = "Standard normal variate",
               ylab = "Flow (mm/day)",
               f.value = ppoints(100), tails.n = 50,
               as.table = TRUE))
  dev.off()

  message("Snow full-run (1000yr) complete.")
}, error = function(e) {
  message(sprintf("ERROR in Snow-full model for %s: %s", catchment_id, conditionMessage(e)))
})

# COMMAND ----------

# MAGIC %md
# MAGIC ## Compile Summary for This Catchment

# COMMAND ----------

summary_list <- list()

for (f in list.files(catch_results_dir, pattern = "_results\\.json$", full.names = TRUE)) {
  summary_list[[length(summary_list) + 1]] <- fromJSON(f)
}

summary_json <- toJSON(summary_list, auto_unbox = TRUE, pretty = TRUE)
write(summary_json, file.path(catch_results_dir, "all_models_summary.json"))

message(sprintf("=== Catchment %s analysis complete. Results in: %s ===",
                catchment_id, catch_results_dir))

# Return catchment_id for orchestrator
dbutils.notebook.exit(catchment_id)
