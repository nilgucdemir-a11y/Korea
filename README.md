# Korea IHACRES (Databricks Workflow)

This repo contains a **Databricks Workflow** to run **IHACRES (hydromad)** for **~110 South Korea catchments in parallel**.

## What it does

- **Task 1 (`01_build_daily_ptq.R`)**: builds daily **P / T / Q** time series per catchment (weighted by `op.id` weights) and saves one file per catchment.
- **Task 2 (`02_run_ihacres_catchment.R`)**: runs IHACRES for each catchment (fan-out / for-each) for an **analysis window of `analysis_years` (default 1000)** and saves results under one folder per catchment.

## Key files

- `databricks.yml`: Databricks Asset Bundle config
- `resources/ihacres_korea_workflow.yml`: the Workflow (multi-task Job with a for-each fan-out)
- `notebooks/01_build_daily_ptq.R`: build daily P/T/Q per catchment
- `notebooks/02_run_ihacres_catchment.R`: fit hydromad IHACRES per catchment and write outputs

## Deploy and run (Databricks Asset Bundles)

From a machine with the Databricks CLI + bundle support configured:

```bash
databricks bundle validate
databricks bundle deploy
databricks bundle run ihacres_korea_110_catchments
```

## Parameters you’ll likely change

All paths are job parameters (so you can override them at run time):

- `weights_file`: catchment-to-`op.id` weights CSV (must contain `catchment_id`, `op.id`, `weight`, `sub.region`)
- `precip_dir`, `temp_dir`, `river_dir`: folders containing `.rds` files
- `ptq_output_dir`: where to write per-catchment daily P/T/Q (`*.rds` and optional `*.csv`)
- `ihacres_output_dir`: where to write IHACRES outputs (one folder per catchment)
- `analysis_years`: default `1000`
- `samples`: default `1000`
- `sma_model`: default `snow` (use `cmd` for no-snow)
- `optim_method`: default `PORT` (you can change to an NLopt method if available in your runtime)

### Parallelism

The fan-out concurrency is controlled by bundle variable `ihacres_concurrency` (default **110**). If your workspace limits prevent 110 concurrent single-node clusters, reduce it.
