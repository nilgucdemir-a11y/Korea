# Databricks IHACRES workflow (dynamic parallel catchments)

This folder contains a Databricks workflow to run IHACRES at catchment scale with:

- split stages (setup -> calibration -> simulation -> merge)
- dynamic parallel fan-out based on the selected catchment list
- simple user inputs for daily operation

Notebooks are organized with clear Databricks cells (`# COMMAND ----------`) and markdown section headers.  
`00_setup_and_widgets.R` is intentionally mixed-language: configuration/discovery stays in R, and task value publication runs in a Python cell for higher Databricks job reliability.

---

## 1) Workflow overview

The job definition is in:

- `workflows/ihacres_110_parallel_workflow.json`

Despite the filename, the workflow is now dynamic:

1. **setup_environment** (`00_setup_and_widgets.R`)
   - reads user parameters
   - resolves paths/config by country
   - discovers eligible catchments
   - writes `run_config.json`
   - stages task-value payload in R, then publishes task values in Python (`catchment_indices_json`, parallel count, etc.)

2. **calibrate_catchments_parallel** (`01_calibrate_ihacres_for_catchment.R`)
   - For-Each over `catchment_indices_json`
   - one catchment per task
   - calibrates IHACRES and saves fitted model + calibration metrics

3. **simulate_catchments_parallel** (`02_simulate_ihacres_for_catchment.R`)
   - For-Each over same catchments
   - loads fitted model
   - simulates all requested year windows
   - saves simulation metrics/timeseries

4. **merge_results** (`03_merge_results.R`)
   - merges all per-catchment outputs
   - writes run-level summary CSV files

---

## 2) Notebooks in this folder

- `notebooks/00_setup_and_widgets.R`
- `notebooks/01_calibrate_ihacres_for_catchment.R`
- `notebooks/02_simulate_ihacres_for_catchment.R`
- `notebooks/03_merge_results.R`
- `notebooks/_common_ihacres.R` (shared helpers)
- `notebooks/_discover_catchments.R` (utility notebook for catchment discovery/conversion)

---

## 3) Required inputs

You can run in two data modes:

### A) Existing PEQ files (recommended for Korea)

- each catchment file should contain columns compatible with:
  - `Date`, `P`, `E`, `Q`
  - or `Date`, `precip_mean`, `temp_mean`, `Q`

### B) Build PEQ from forcing/weights (for other countries)

Provide:

- catchment weights CSV (`catchment_id`, `op.id`, `weight`, `sub.region`)
- precipitation directory (RDS by op id)
- temperature directory (RDS by op id)
- river/discharge directory (RDS by catchment)

Use `advanced_config_json` to pass these custom paths.

---

## 4) User parameters (what you set when running job)

The job exposes 5 parameters:

1. `country` (example: `KOR`)
2. `model_years` (example: `1000`)
3. `model_type` (`snow` or `cmd`)
4. `start_date` (must be `YYYY-MM-DD`)
5. `advanced_config_json` (optional JSON string)

### Example: simple Korea run

- `country = KOR`
- `model_years = 1000`
- `model_type = snow`
- `start_date = 0000-01-01`
- `advanced_config_json = ""`

KOR defaults now read existing PEQ files from:

- `/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Input/`

### Example: custom run with advanced overrides

```json
{
  "use_existing_peq": true,
  "peq_dir": "/Volumes/.../MY_COUNTRY_PEQ/",
  "calibration_years": 100,
  "simulation_years": [100, 500, 1000],
  "catchment_limit": 250,
  "parallel_concurrency_limit": 120
}
```

Notes:

- If `catchment_limit` is omitted, all eligible catchments are selected.
- If `parallel_concurrency_limit` is omitted, concurrency = selected catchment count.
- You do **not** need to enter catchment IDs manually in normal workflow runs.
  - Setup generates catchment list automatically.
  - Parallel tasks read from that list.

---

## 5) How to run in Databricks (detailed)

### Step 1: Import notebooks

Import these into Databricks workspace (example path):

- `/Workspace/Shared/ihacres/notebooks/00_setup_and_widgets`
- `/Workspace/Shared/ihacres/notebooks/01_calibrate_ihacres_for_catchment`
- `/Workspace/Shared/ihacres/notebooks/02_simulate_ihacres_for_catchment`
- `/Workspace/Shared/ihacres/notebooks/03_merge_results`
- `/Workspace/Shared/ihacres/notebooks/_common_ihacres`
- (optional) `/Workspace/Shared/ihacres/notebooks/_discover_catchments`

### Step 2: Configure job JSON

Open `workflows/ihacres_110_parallel_workflow.json` and update:

- `job_clusters[0].new_cluster.node_type_id`
- notebook paths if different from `/Workspace/Shared/ihacres/notebooks/...`

### Step 3: Create job

Create the Databricks job using the JSON (Jobs UI code editor or Jobs API).

### Step 4: Start run

When starting the run, set:

- `country`
- `model_years`
- `model_type`
- `start_date`
- optional `advanced_config_json`

### Step 5: Monitor run

In `setup_environment` logs, check:

- total catchments in region
- eligible catchments
- selected catchments
- parallel tasks running at once

Then monitor:

- calibration fan-out task
- simulation fan-out task
- merge task

Important:

- Per-catchment notebooks also support **auto-list mode**:
  - if `catchment_id` and `catchment_idx` are empty, they run the full catchment manifest list one by one.
  - `run_config_path` can be passed explicitly, or left empty to auto-resolve from setup task values / latest discovered run config.

### Step 6: Collect outputs

See output directories and summary files below.

---

## 6) Output layout

Inside `ihacres_output_dir`:

- `run_config.json`
- `manifests/runtime_catalog.rds`
- `manifests/catchment_manifest.csv`
- `peq/<catchment_id>.rds`
- `calibration_models/<catchment_id>_fit.rds`
- `calibration_metrics/<catchment_id>_calibration_metrics.csv`
- `calibration_timeseries/<catchment_id>_calibration_sim_vs_obs.csv`
- `simulation_metrics/<catchment_id>_simulation_metrics.csv`
- `simulation_timeseries/<catchment_id>_sim_<years>y.csv`
- `summaries/calibration_metrics_all_catchments.csv`
- `summaries/simulation_metrics_all_catchments.csv`
- `summaries/simulation_status_summary_by_year.csv`
- `summaries/simulation_performance_by_year.csv`
- `summaries/run_summary.csv`

---

## 7) `_discover_catchments.R` utility notebook

Use this notebook when you need to:

- discover catchments from a folder of `.rds`
- count catchments quickly
- set task values for catchment list/count
- convert source files into standardized `P/E/Q/Date` `.rds` files

Useful for checking country data before running the full workflow.

---

## 8) Troubleshooting

- **Invalid start date**
  - Ensure `start_date` is `YYYY-MM-DD`.

- **No eligible catchments found**
  - verify input directories/files
  - for non-KOR runs, pass required paths in `advanced_config_json`

- **Failed to resolve references: `tasks.setup_environment.values.catchment_indices_json`**
  - setup task could not publish required task values
  - confirm the Python task-values cell in setup (`## 5) Publish task values with Python`) executed successfully
  - check setup logs for task-value errors
  - ensure you are running as a Databricks **Job** task (not as a standalone notebook run)
  - ensure setup task completed successfully before fan-out tasks

- **Hydromad package issues**
  - setup installs dependencies automatically
  - rerun setup task if cluster was restarted

- **Parallelism too high for cluster**
  - set `parallel_concurrency_limit` in `advanced_config_json`

- **Need quick test run**
  - set `catchment_limit` to a small number (for example 5)
