#!/usr/bin/env python3
"""
Validate that all notebooks and config files are present and well-formed.
Run before deploying to catch obvious issues.
"""

import json
import os
import sys

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

REQUIRED_FILES = [
    "notebooks/00_install_libraries.r",
    "notebooks/01_build_catchment_data.r",
    "notebooks/02_ihacres_analysis.r",
    "notebooks/03_orchestrator.py",
    "notebooks/04_aggregate_results.r",
    "notebooks/05_generate_foreach_inputs.py",
    "config/workflow_definition.json",
    "config/workflow_foreach.json",
    "config/default_parameters.json",
]

errors = []

print("Checking required files...")
for rel in REQUIRED_FILES:
    path = os.path.join(REPO_ROOT, rel)
    if not os.path.isfile(path):
        errors.append(f"  MISSING: {rel}")
    else:
        size = os.path.getsize(path)
        if size == 0:
            errors.append(f"  EMPTY:   {rel}")
        else:
            print(f"  OK  {rel}  ({size:,} bytes)")

print("\nValidating JSON configs...")
for rel in REQUIRED_FILES:
    if rel.endswith(".json"):
        path = os.path.join(REPO_ROOT, rel)
        if os.path.isfile(path):
            try:
                with open(path) as f:
                    json.load(f)
                print(f"  OK  {rel} (valid JSON)")
            except json.JSONDecodeError as e:
                errors.append(f"  INVALID JSON: {rel} — {e}")

print("\nChecking R notebooks have COMMAND separators...")
for rel in REQUIRED_FILES:
    if rel.endswith(".r"):
        path = os.path.join(REPO_ROOT, rel)
        if os.path.isfile(path):
            with open(path) as f:
                content = f.read()
            n_cmds = content.count("# COMMAND ----------")
            if n_cmds > 0:
                print(f"  OK  {rel}  ({n_cmds} command separators)")
            else:
                errors.append(f"  WARNING: {rel} has no COMMAND separators (may not split into cells)")

if errors:
    print(f"\n{'='*50}")
    print(f"ERRORS/WARNINGS ({len(errors)}):")
    for e in errors:
        print(e)
    sys.exit(1)
else:
    print(f"\nAll checks passed.")
    sys.exit(0)
