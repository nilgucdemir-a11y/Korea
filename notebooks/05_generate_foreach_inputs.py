# Databricks notebook source
# MAGIC %md
# MAGIC # 05 — Generate Catchment List for ForEach Task
# MAGIC
# MAGIC Reads the catchment_list.csv (or scans RDS files) and returns
# MAGIC the list as a task value that the ForEach task can iterate over.

# COMMAND ----------

dbutils.widgets.text("input_dir",
    "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/catchments_daily_PTQ_by_RiverID/KOR/",
    "Catchment data directory")

# COMMAND ----------

import os
import csv

input_dir = dbutils.widgets.get("input_dir")

catchment_list_path = os.path.join(input_dir, "catchment_list.csv")

if os.path.exists(catchment_list_path):
    with open(catchment_list_path, "r") as f:
        reader = csv.DictReader(f)
        catchment_ids = [row["catchment_id"] for row in reader]
    print(f"Loaded {len(catchment_ids)} catchments from catchment_list.csv")
else:
    rds_files = [f for f in os.listdir(input_dir)
                 if f.endswith(".rds") and not f.startswith("catchment_list")]
    catchment_ids = sorted([os.path.splitext(f)[0] for f in rds_files])
    print(f"Discovered {len(catchment_ids)} catchment RDS files in {input_dir}")

print(f"Catchments: {catchment_ids[:10]}...")

dbutils.jobs.taskValues.set(key="catchment_ids", value=catchment_ids)
