# Databricks notebook source
# MAGIC %md
# MAGIC # 00 — Install R Libraries for IHACRES Workflow
# MAGIC
# MAGIC This notebook installs all required R packages on the cluster.
# MAGIC Run this **once** per cluster start (or attach as an init script).

# COMMAND ----------

# Core CRAN packages
install.packages(
  c("zoo", "latticeExtra", "polynom", "car", "Hmisc", "reshape",
    "DEoptim", "hydroGOF", "ggplot2", "dplyr", "readr", "tidyr",
    "purrr", "jsonlite"),
  repos = "https://cloud.r-project.org"
)

# COMMAND ----------

install.packages("DEoptim", repos = "https://cloud.r-project.org")
install.packages("dream",   repos = "https://cloud.r-project.org")

# COMMAND ----------

install.packages("devtools", repos = "https://cloud.r-project.org")

# COMMAND ----------

# hydromad from GitHub
devtools::install_github("hydromad/hydromad", upgrade = "never")

# COMMAND ----------

# Verify all critical packages load
library(hydromad)
library(zoo)
library(hydroGOF)
library(ggplot2)
library(dplyr)
library(readr)

message("All libraries installed and loaded successfully.")
