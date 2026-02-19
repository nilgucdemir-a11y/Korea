# Databricks notebook source
# MAGIC %md
# MAGIC # 03 — Orchestrator: Run IHACRES on 110 Catchments in Parallel
# MAGIC
# MAGIC This Python notebook reads the catchment list produced by notebook 01 and
# MAGIC launches **02_ihacres_analysis** in parallel using `dbutils.notebook.run()`
# MAGIC with a `ThreadPoolExecutor`.  Each catchment gets its own isolated notebook
# MAGIC run on the same cluster, achieving concurrent execution for all 110 catchments.

# COMMAND ----------

# MAGIC %md
# MAGIC ## Widgets

# COMMAND ----------

dbutils.widgets.text("input_dir",
    "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/catchments_daily_PTQ_by_RiverID/KOR/",
    "Catchment data directory")

dbutils.widgets.text("results_dir",
    "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/ihacres_results/KOR/",
    "Results output directory")

dbutils.widgets.text("analysis_notebook_path",
    "/Workspace/Repos/ihacres-workflow/notebooks/02_ihacres_analysis",
    "Path to analysis notebook in Workspace")

dbutils.widgets.text("max_parallel", "110", "Max parallel notebook runs")
dbutils.widgets.text("timeout_seconds", "14400", "Per-notebook timeout (seconds)")
dbutils.widgets.text("optim_samples", "1000", "Optimisation samples per model fit")
dbutils.widgets.text("start_date",   "0000-01-01", "Time-series start date")
dbutils.widgets.text("cal_end_date", "0100-12-31", "Calibration end date")
dbutils.widgets.text("run_end_date", "1000-12-31", "Full run end date")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Read Parameters

# COMMAND ----------

import os
import csv
import json
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed

input_dir       = dbutils.widgets.get("input_dir")
results_dir     = dbutils.widgets.get("results_dir")
notebook_path   = dbutils.widgets.get("analysis_notebook_path")
max_parallel    = int(dbutils.widgets.get("max_parallel"))
timeout_seconds = int(dbutils.widgets.get("timeout_seconds"))
optim_samples   = dbutils.widgets.get("optim_samples")
start_date      = dbutils.widgets.get("start_date")
cal_end_date    = dbutils.widgets.get("cal_end_date")
run_end_date    = dbutils.widgets.get("run_end_date")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Discover Catchments

# COMMAND ----------

catchment_list_path = os.path.join(input_dir, "catchment_list.csv")

if os.path.exists(catchment_list_path):
    with open(catchment_list_path, "r") as f:
        reader = csv.DictReader(f)
        catchment_ids = [row["catchment_id"] for row in reader]
    print(f"Loaded {len(catchment_ids)} catchments from catchment_list.csv")
else:
    rds_files = [f for f in os.listdir(input_dir) if f.endswith(".rds") and f != "catchment_list.csv"]
    catchment_ids = [os.path.splitext(f)[0] for f in sorted(rds_files)]
    print(f"Discovered {len(catchment_ids)} catchment RDS files in {input_dir}")

print(f"First 10 catchments: {catchment_ids[:10]}")
print(f"Total catchments to process: {len(catchment_ids)}")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Launch Parallel Notebook Runs

# COMMAND ----------

def run_catchment(cid):
    """Run the IHACRES analysis notebook for a single catchment."""
    try:
        result = dbutils.notebook.run(
            notebook_path,
            timeout_seconds,
            {
                "catchment_id":  cid,
                "input_dir":     input_dir,
                "results_dir":   results_dir,
                "optim_samples": optim_samples,
                "start_date":    start_date,
                "cal_end_date":  cal_end_date,
                "run_end_date":  run_end_date,
            }
        )
        return {"catchment_id": cid, "status": "success", "result": result}
    except Exception as e:
        return {"catchment_id": cid, "status": "failed", "error": str(e)}


results = []
failed  = []

print(f"Launching {len(catchment_ids)} catchments with max {max_parallel} parallel workers...")
print(f"Timeout per catchment: {timeout_seconds}s  |  Samples: {optim_samples}")
print(f"Start: {datetime.now().isoformat()}")

with ThreadPoolExecutor(max_workers=max_parallel) as executor:
    futures = {executor.submit(run_catchment, cid): cid for cid in catchment_ids}

    for i, future in enumerate(as_completed(futures), 1):
        res = future.result()
        results.append(res)
        status_icon = "OK" if res["status"] == "success" else "FAIL"
        print(f"  [{i}/{len(catchment_ids)}] {status_icon}  {res['catchment_id']}")
        if res["status"] == "failed":
            failed.append(res)

print(f"\nFinished: {datetime.now().isoformat()}")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Summary Report

# COMMAND ----------

n_success = sum(1 for r in results if r["status"] == "success")
n_failed  = sum(1 for r in results if r["status"] == "failed")

print(f"{'='*60}")
print(f"  IHACRES Parallel Run Summary")
print(f"{'='*60}")
print(f"  Total catchments:  {len(catchment_ids)}")
print(f"  Succeeded:         {n_success}")
print(f"  Failed:            {n_failed}")
print(f"{'='*60}")

if failed:
    print("\nFailed catchments:")
    for f in failed:
        print(f"  - {f['catchment_id']}: {f.get('error', 'unknown')[:200]}")

summary_path = os.path.join(results_dir, "orchestrator_summary.json")
os.makedirs(results_dir, exist_ok=True)
with open(summary_path, "w") as fp:
    json.dump({
        "run_timestamp": datetime.now().isoformat(),
        "total": len(catchment_ids),
        "success": n_success,
        "failed": n_failed,
        "failed_details": failed,
        "parameters": {
            "optim_samples": optim_samples,
            "start_date": start_date,
            "cal_end_date": cal_end_date,
            "run_end_date": run_end_date,
        }
    }, fp, indent=2)

print(f"\nSummary saved to: {summary_path}")
