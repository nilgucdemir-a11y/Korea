# Databricks IHACRES workflow (110 parallel catchments)

This folder contains a complete Databricks workflow scaffold to run IHACRES analysis for Korea catchments in parallel.

## What is included

- `notebooks/00_setup_and_widgets.R`
  - Installs required R libraries (including `hydromad` from GitHub when missing)
  - Defines runtime parameters via widgets
  - Filters and validates catchments for the selected region (`KOR` by default)
  - Exposes catchment IDs as a task value for fan-out execution

- `notebooks/01_run_ihacres_for_catchment.R`
  - Runs one catchment end-to-end:
    - builds weighted daily `P`, `T`, `Q`
    - saves PTQ outputs (`.rds`, optional `.csv`)
    - calibrates IHACRES (`snow` or `cmd`) for the requested date window
    - saves fitted model, simulated-vs-observed time series, and metrics

- `notebooks/02_merge_results.R`
  - Merges all per-catchment metrics files into run-level summary outputs

- `notebooks/_common_ihacres.R`
  - Shared helper functions used by all workflow notebooks

- `workflows/ihacres_110_parallel_workflow.json`
  - Databricks Job definition:
    1. setup
    2. parallel for-each (concurrency = 110)
    3. merge results

## Deploy in Databricks

1. Import/sync notebooks into workspace, e.g.:
   - `/Workspace/Shared/ihacres/notebooks/00_setup_and_widgets`
   - `/Workspace/Shared/ihacres/notebooks/01_run_ihacres_for_catchment`
   - `/Workspace/Shared/ihacres/notebooks/02_merge_results`
   - `/Workspace/Shared/ihacres/notebooks/_common_ihacres`

2. Open `workflows/ihacres_110_parallel_workflow.json` and update:
   - cluster `node_type_id` to your workspace node type
   - notebook paths if you use a different workspace location

3. Create the job using the JSON (Jobs API/UI import) and run.

## Default runtime parameters

- Region: `KOR`
- Catchments: first `110` eligible catchments (`catchment_limit = 110`)
- Time window: `0000-01-01` to `1000-12-31`
- IHACRES model: `snow`
- Objective: `kge`
- Optimizer: `PORT` (fallback when non-PORT optimizer fails)
- Samples: `1000`

## Outputs

Inside `ihacres_output_dir`:

- `metrics/<catchment_id>_metrics.csv`
- `timeseries/<catchment_id>_sim_vs_obs.csv`
- `models/<catchment_id>_fit.rds`
- `logs/<catchment_id>_run_log.json`
- `ihacres_metrics_all_catchments.csv`
- `ihacres_status_summary.csv`
- `ihacres_run_summary.csv`

Inside `ptq_output_dir`:

- `<catchment_id>.rds`
- `<catchment_id>.csv` (optional)
