# Databricks IHACRES workflow (110 parallel catchments)

This folder contains a Databricks workflow scaffold with **separate calibration and simulation stages**.

## What is included

- `notebooks/00_setup_and_widgets.R`
  - Installs/validates libraries
  - Defines widgets (including model type and years widgets)
  - Supports two input modes:
    - `use_existing_peq=true` (recommended for Korea, using existing PEQ files)
    - `use_existing_peq=false` (build PEQ from precip/temp/river + weights, for other countries)
  - Builds catchment fan-out list
  - Precomputes and saves a runtime source catalog manifest for faster parallel tasks

- `notebooks/01_calibrate_ihacres_for_catchment.R`
  - One catchment calibration only
  - Saves calibrated model (`.rds`) + calibration metrics

- `notebooks/02_simulate_ihacres_for_catchment.R`
  - One catchment simulation only
  - Loads saved calibration model
  - Runs simulation for all `simulation_years_csv` windows (for comparison)
  - Saves simulation metrics + simulated-vs-observed time series per year-window

- `notebooks/03_merge_results.R`
  - Merges calibration and simulation outputs
  - Produces summaries by status and by simulation years

- `notebooks/_common_ihacres.R`
  - Shared helpers for PEQ loading/building, calibration, simulation, and metrics

- `workflows/ihacres_110_parallel_workflow.json`
  - Databricks Job definition:
    1. setup
    2. calibrate in parallel (for-each, concurrency 110)
    3. simulate in parallel (for-each, concurrency 110)
    4. merge results

## Key widgets/parameters

- `model_type`: `snow` or `cmd`
- `calibration_years`: years used for calibration window
- `simulation_years_csv`: comma-separated simulation windows for comparison (example: `100,500,1000`)
- `use_existing_peq`: if `true`, load PEQ directly by catchment from `peq_dir`
- `catalog_rds_path`: optional; normally auto-generated in setup and passed to child tasks

## Performance improvements included

- Setup writes `manifests/runtime_catalog.rds` once, then child tasks reuse it
  - avoids repeated full directory scans in every parallel catchment task
  - reduces repeated weights/index loading overhead in non-PEQ mode
- Simulation preprocesses PEQ once per catchment and reuses it across multiple year windows

## Deploy in Databricks

1. Import/sync notebooks into workspace:
   - `/Workspace/Shared/ihacres/notebooks/00_setup_and_widgets`
   - `/Workspace/Shared/ihacres/notebooks/01_calibrate_ihacres_for_catchment`
   - `/Workspace/Shared/ihacres/notebooks/02_simulate_ihacres_for_catchment`
   - `/Workspace/Shared/ihacres/notebooks/03_merge_results`
   - `/Workspace/Shared/ihacres/notebooks/_common_ihacres`

2. Open `workflows/ihacres_110_parallel_workflow.json` and update:
   - `node_type_id`
   - notebook paths if needed

3. Import/create the job and run.

## Outputs

Inside `ihacres_output_dir`:

- `peq/<catchment_id>.rds` (canonical PEQ used by tasks)
- `calibration_models/<catchment_id>_fit.rds`
- `calibration_metrics/<catchment_id>_calibration_metrics.csv`
- `calibration_timeseries/<catchment_id>_calibration_sim_vs_obs.csv`
- `simulation_metrics/<catchment_id>_simulation_metrics.csv`
- `simulation_timeseries/<catchment_id>_sim_<years>y.csv`
- `summaries/calibration_metrics_all_catchments.csv`
- `summaries/simulation_metrics_all_catchments.csv`
- `summaries/simulation_performance_by_year.csv`
- `summaries/run_summary.csv`
