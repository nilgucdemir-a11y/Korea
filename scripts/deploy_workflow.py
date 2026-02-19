#!/usr/bin/env python3
"""
Deploy the IHACRES workflow to Databricks as a multi-task job.

Usage:
    python deploy_workflow.py --config ../config/workflow_definition.json
    python deploy_workflow.py --config ../config/workflow_foreach.json

Requires:
    - DATABRICKS_HOST env var  (e.g. https://adb-xxxx.azuredatabricks.net)
    - DATABRICKS_TOKEN env var (PAT or OAuth token)
    - `requests` Python package
"""

import argparse
import json
import os
import sys

try:
    import requests
except ImportError:
    print("Install requests: pip install requests")
    sys.exit(1)


def get_env():
    host = os.environ.get("DATABRICKS_HOST", "").rstrip("/")
    token = os.environ.get("DATABRICKS_TOKEN", "")
    if not host or not token:
        print("ERROR: Set DATABRICKS_HOST and DATABRICKS_TOKEN environment variables.")
        sys.exit(1)
    return host, token


def create_or_reset_job(host, token, job_spec):
    headers = {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}
    api = f"{host}/api/2.1/jobs"

    list_resp = requests.get(
        f"{api}/list",
        headers=headers,
        params={"name": job_spec["name"], "limit": 25},
    )
    list_resp.raise_for_status()

    existing = [
        j for j in list_resp.json().get("jobs", [])
        if j["settings"]["name"] == job_spec["name"]
    ]

    if existing:
        job_id = existing[0]["job_id"]
        print(f"Updating existing job {job_id} ...")
        reset_resp = requests.post(
            f"{api}/reset",
            headers=headers,
            json={"job_id": job_id, "new_settings": job_spec},
        )
        reset_resp.raise_for_status()
        print(f"Job updated: {job_id}")
    else:
        print("Creating new job ...")
        create_resp = requests.post(f"{api}/create", headers=headers, json=job_spec)
        create_resp.raise_for_status()
        job_id = create_resp.json()["job_id"]
        print(f"Job created: {job_id}")

    print(f"View at: {host}/#job/{job_id}")
    return job_id


def main():
    parser = argparse.ArgumentParser(description="Deploy IHACRES Databricks workflow")
    parser.add_argument(
        "--config",
        default=os.path.join(os.path.dirname(__file__), "..", "config", "workflow_definition.json"),
        help="Path to workflow JSON definition",
    )
    parser.add_argument("--run-now", action="store_true", help="Trigger a run immediately after deploy")
    args = parser.parse_args()

    with open(args.config) as f:
        job_spec = json.load(f)

    host, token = get_env()
    job_id = create_or_reset_job(host, token, job_spec)

    if args.run_now:
        print("Triggering run ...")
        headers = {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}
        run_resp = requests.post(
            f"{host}/api/2.1/jobs/run-now",
            headers=headers,
            json={"job_id": job_id},
        )
        run_resp.raise_for_status()
        run_id = run_resp.json()["run_id"]
        print(f"Run triggered: {run_id}")
        print(f"Monitor at: {host}/#job/{job_id}/run/{run_id}")


if __name__ == "__main__":
    main()
