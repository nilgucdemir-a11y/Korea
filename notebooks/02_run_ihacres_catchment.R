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

ensure_hydromad <- function() {
  if (requireNamespace("hydromad", quietly = TRUE)) return(invisible(TRUE))
  ensure_packages(c("remotes"))

  suppressWarnings(try(install.packages("hydromad"), silent = TRUE))
  if (!requireNamespace("hydromad", quietly = TRUE)) {
    remotes::install_github("hydromad/hydromad")
  }
  invisible(TRUE)
}

ensure_packages(c("zoo", "jsonlite", "hydroGOF", "lattice", "latticeExtra"))
ensure_hydromad()

suppressPackageStartupMessages({
  library(zoo)
  library(jsonlite)
  library(hydroGOF)
  library(hydromad)
  library(lattice)
  library(latticeExtra)
})

# COMMAND ----------
# Parameters
catchment_id <- get_param("catchment_id")
ptq_input_dir <- dbfs_to_local(get_param("ptq_input_dir"))
output_dir <- dbfs_to_local(get_param("output_dir"))

start_date_str <- get_param("start_date", "0000-01-01")
analysis_years <- suppressWarnings(as.numeric(get_param("analysis_years", "1000")))
samples <- suppressWarnings(as.integer(get_param("samples", "1000")))

sma_model <- get_param("sma_model", "snow")
routing_model <- get_param("routing_model", "expuh")
objective <- get_param("objective", "kge")
optim_method <- get_param("optim_method", "PORT")

if (is.na(analysis_years) || analysis_years <= 0) analysis_years <- 1000
if (is.na(samples) || samples <= 0) samples <- 1000L

if (is.null(catchment_id) || !nzchar(catchment_id)) stop("Missing required parameter: catchment_id")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

catch_out_dir <- file.path(output_dir, catchment_id)
if (!dir.exists(catch_out_dir)) dir.create(catch_out_dir, recursive = TRUE)

config_path <- file.path(catch_out_dir, "run_config.json")
writeLines(jsonlite::toJSON(list(
  catchment_id = catchment_id,
  ptq_input_dir = ptq_input_dir,
  output_dir = output_dir,
  start_date = start_date_str,
  analysis_years = analysis_years,
  samples = samples,
  sma_model = sma_model,
  routing_model = routing_model,
  objective = objective,
  optim_method = optim_method
), auto_unbox = TRUE, pretty = TRUE), config_path)

# COMMAND ----------
# Load daily P/T/Q for this catchment
ptq_path <- file.path(ptq_input_dir, paste0(catchment_id, ".rds"))
if (!file.exists(ptq_path)) stop("Missing catchment PTQ file: ", ptq_path)

df <- readRDS(ptq_path)
req_cols <- c("Date", "precip_mean", "temp_mean", "Q")
missing_cols <- setdiff(req_cols, names(df))
if (length(missing_cols) > 0) stop("PTQ file missing columns: ", paste(missing_cols, collapse = ", "))

df <- df[, req_cols]
df$Date <- as.Date(df$Date)
df$P <- as.numeric(df$precip_mean)
df$E <- as.numeric(df$temp_mean)
df$Q <- as.numeric(df$Q)

ts_all <- zoo(df[, c("P", "E", "Q")], order.by = df$Date)

start_date <- suppressWarnings(as.Date(start_date_str))
if (is.na(start_date)) start_date <- index(ts_all)[1]
end_date <- start_date + as.integer(round(analysis_years * 365)) - 1L

ts <- tryCatch(window(ts_all, start = start_date, end = end_date), error = function(e) ts_all)
if (NROW(ts) < 10) stop("Not enough data after windowing for catchment ", catchment_id, " (n=", NROW(ts), ")")

# COMMAND ----------
# Fit IHACRES (hydromad)
model_args <- list(
  data = ts,
  sma = sma_model,
  routing = routing_model,
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

if (identical(sma_model, "snow")) {
  model_args$delay <- c(0, 10)
  model_args$Tmax <- c(0, 5)
  model_args$Tmin <- c(-12, 0)
  model_args$Tmelt <- c(-3, 5)
  model_args$kd <- c(1, 5)
  model_args$rcap <- c(0, 1)
  model_args$cr <- c(0.8, 1.2)
  model_args$cs <- c(0.8, 1.2)
}

event_set <- do.call(hydromad, model_args)

fit <- NULL
fit_err <- NULL
set.seed(1)
fit <- tryCatch(
  fitByOptim(event_set, samples = samples, method = optim_method),
  error = function(e) {
    fit_err <<- conditionMessage(e)
    NULL
  }
)

if (is.null(fit)) {
  err_path <- file.path(catch_out_dir, "ERROR.txt")
  writeLines(c(
    paste0("IHACRES fit failed for catchment_id=", catchment_id),
    paste0("Error: ", fit_err)
  ), err_path)
  stop("IHACRES fit failed for ", catchment_id, ": ", fit_err)
}

saveRDS(fit, file.path(catch_out_dir, "hydromad_fit.rds"))
writeLines(capture.output(summary(fit)), file.path(catch_out_dir, "hydromad_fit_summary.txt"))

coefs <- tryCatch(coef(fit), error = function(e) NULL)
if (!is.null(coefs)) {
  write.csv(as.data.frame(t(coefs)), file.path(catch_out_dir, "hydromad_fit_coefficients.csv"), row.names = FALSE)
}

# COMMAND ----------
# Save simulation + metrics
get_sim <- function(f) {
  z <- tryCatch(fitted(f), error = function(e) NULL)
  if (!is.null(z)) return(z)
  tryCatch(predict(f), error = function(e) NULL)
}

sim <- get_sim(fit)
dates <- index(ts)
Q_obs <- as.numeric(ts[, "Q"])

Q_sim <- NULL
if (!is.null(sim)) {
  if (inherits(sim, "zoo")) {
    # If hydromad returns a zoo object, align by date overlap.
    sim_dates <- index(sim)
    sim_vals <- as.numeric(sim)
    idx <- match(dates, sim_dates)
    Q_sim <- sim_vals[idx]
  } else {
    sim_vals <- suppressWarnings(as.numeric(sim))
    if (length(sim_vals) == length(Q_obs)) Q_sim <- sim_vals
  }
}

out_df <- data.frame(
  Date = dates,
  P = as.numeric(ts[, "P"]),
  E = as.numeric(ts[, "E"]),
  Q_obs = Q_obs,
  Q_sim = Q_sim
)
write.csv(out_df, file.path(catch_out_dir, "timeseries_obs_sim.csv"), row.names = FALSE)

metrics <- list(n = length(Q_obs))
ok <- !is.na(Q_obs) & !is.na(Q_sim)
if (any(ok)) {
  obs <- Q_obs[ok]
  simv <- Q_sim[ok]
  metrics$KGE <- tryCatch(as.numeric(KGE(simv, obs)), error = function(e) NA_real_)
  metrics$NSE <- tryCatch(as.numeric(NSE(simv, obs)), error = function(e) NA_real_)
  metrics$RMSE <- tryCatch(as.numeric(rmse(simv, obs)), error = function(e) NA_real_)
  metrics$PBIAS <- tryCatch(as.numeric(pbias(simv, obs)), error = function(e) NA_real_)
} else {
  metrics$KGE <- NA_real_
  metrics$NSE <- NA_real_
  metrics$RMSE <- NA_real_
  metrics$PBIAS <- NA_real_
}

writeLines(jsonlite::toJSON(metrics, auto_unbox = TRUE, pretty = TRUE), file.path(catch_out_dir, "metrics.json"))

# COMMAND ----------
# Quick plot (saved as PNG)
plot_path <- file.path(catch_out_dir, "hydrograph.png")
png(plot_path, width = 1400, height = 800)
try(
  print(xyplot(fit, with.P = TRUE, xlab = "Date", ylab = "Flow")),
  silent = TRUE
)
dev.off()

