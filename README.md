# IHACRES Databricks Workflow — South Korea 110 Catchments

Databricks multi-task workflow for running **IHACRES** (hydromad) hydrological
analysis on **110 South Korea catchments** in parallel, using 1000-year
simulation data.

## Repository Structure

```
├── notebooks/
│   ├── 00_install_libraries.r          # Install R packages (hydromad, zoo, etc.)
│   ├── 01_build_catchment_data.r       # Build daily P/T/Q per catchment from raw RDS
│   ├── 02_ihacres_analysis.r           # Single-catchment IHACRES analysis (parameterized)
│   ├── 03_orchestrator.py              # Python orchestrator — launches 110 parallel runs
│   ├── 04_aggregate_results.r          # Merge all catchment results into master table
│   └── 05_generate_foreach_inputs.py   # Emit catchment list for ForEach task variant
├── config/
│   ├── workflow_definition.json        # Databricks workflow (orchestrator-based)
│   ├── workflow_foreach.json           # Databricks workflow (native ForEach-based)
│   └── default_parameters.json         # Default parameter values & hydromad ranges
├── scripts/
│   ├── deploy_workflow.py              # Deploy workflow via Databricks Jobs API
│   └── validate_notebooks.py           # Pre-deploy validation checks
└── README.md
```

## Workflow Overview

The workflow has four sequential stages:

| Step | Notebook | Description |
|------|----------|-------------|
| 1 | `00_install_libraries` | Install all R dependencies on the cluster |
| 2 | `01_build_catchment_data` | Read raw simulation RDS files, apply catchment weights, produce daily P/T/Q per catchment |
| 3 | `03_orchestrator` | Launch `02_ihacres_analysis` for each of the 110 catchments in parallel |
| 4 | `04_aggregate_results` | Collect per-catchment JSON results into a master CSV/RDS summary |

### Models Fitted Per Catchment

Each catchment runs three model configurations:

1. **CMD (no-snow)** — 100-year calibration window  
2. **Snow SMA** — 100-year calibration window  
3. **Snow SMA** — Full 1000-year window  

All use `expuh` routing and `fitByOptim` with 1000 samples (PORT method).

### Outputs Per Catchment

- `{model}_results.json` — Coefficients + goodness-of-fit (NSE, KGE, RMSE, PBIAS)
- `{model}_fit.rds` — Full fitted hydromad object
- `{model}_obs_sim.csv` — Observed vs modelled time series
- `{model}_hydrograph.png` — Hydrograph plot
- `{model}_qqplot.png` — Q-Q flow duration plot

## Two Workflow Variants

### Option A: Orchestrator-based (recommended)

Uses `03_orchestrator.py` with Python `ThreadPoolExecutor` to run 110 catchments
concurrently on the same cluster. Simpler setup, single cluster.

**Workflow definition:** `config/workflow_definition.json`

### Option B: Native ForEach task

Uses Databricks Jobs `for_each_task` to launch independent task runs per catchment.
Better isolation and per-catchment retry, but requires Databricks Jobs API v2.1+.

**Workflow definition:** `config/workflow_foreach.json`

## Deployment

### Prerequisites

1. Databricks workspace with R support enabled
2. Unity Catalog volumes mounted at the expected paths (or update paths in widgets)
3. `DATABRICKS_HOST` and `DATABRICKS_TOKEN` environment variables set

### Deploy via script

```bash
# Deploy the orchestrator-based workflow
python scripts/deploy_workflow.py --config config/workflow_definition.json

# Deploy and immediately trigger a run
python scripts/deploy_workflow.py --config config/workflow_definition.json --run-now
```

### Deploy manually

1. Import the `notebooks/` folder into your Databricks Workspace
2. In the Databricks Workflows UI, create a new job
3. Add four tasks with the dependencies shown in the workflow definition
4. Configure widgets/parameters as needed
5. Run

## Configurable Parameters (Widgets)

All notebooks use Databricks widgets so parameters can be overridden at runtime:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `weights_file` | `/Volumes/.../R02_precip_ops_per_catchment.csv` | Catchment weight CSV |
| `precip_dir` | `/Volumes/.../sim.precip.data/` | Raw precipitation RDS files |
| `temp_dir` | `/Volumes/.../sim.temp.data/` | Raw temperature RDS files |
| `river_dir` | `/Volumes/.../sim.river.data/` | Raw discharge RDS files |
| `output_dir` | `/Volumes/.../catchments_daily_PTQ_by_RiverID/KOR/` | Processed catchment data |
| `results_dir` | `/Volumes/.../ihacres_results/KOR/` | IHACRES model results |
| `sub_region` | `KOR` | Sub-region filter for weights file |
| `start_date` | `0000-01-01` | Simulation start date |
| `cal_end_date` | `0100-12-31` | Calibration window end (100 years) |
| `run_end_date` | `1000-12-31` | Full run window end (1000 years) |
| `optim_samples` | `1000` | Number of optimisation samples |
| `max_parallel` | `110` | Max concurrent catchment runs |

## Cluster Requirements

- **Runtime:** Databricks Runtime with R support (e.g., 15.4 LTS)
- **Node type:** Minimum `Standard_DS4_v2` (or equivalent with 28+ GB RAM)
- **Workers:** 4+ workers recommended for 110 parallel R sessions
- **Storage:** Sufficient disk for 110 × 3 model fits + plots

## Validation

Run the validation script before deploying:

```bash
python scripts/validate_notebooks.py
```
