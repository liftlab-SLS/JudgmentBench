#!/usr/bin/env bash
#
# Reproduce all JudgmentBench paper figures and tables.
#
# Expects the released dataset at ./result-dataset (or set EAH_JB_DIR).
# Writes outputs to ./outputs (or set OUT_DIR).

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA_DIR="${EAH_JB_DIR:-$ROOT_DIR/result-dataset}"
OUT_DIR="${OUT_DIR:-$ROOT_DIR/outputs}"

if [ ! -d "$DATA_DIR" ]; then
  echo "Dataset not found at $DATA_DIR." >&2
  echo "Set EAH_JB_DIR=/path/to/result-dataset, or place the released dataset at ./result-dataset." >&2
  exit 1
fi

mkdir -p "$OUT_DIR"

export EAH_JB_DIR="$DATA_DIR"
export EAH_ANALYSIS_OUTPUT_DIR="$OUT_DIR"
export EAH_TABLE_OUTPUT_DIR="$OUT_DIR"

echo "Running analysis (this will run ${EAH_N_BOOT:-2000} bootstrap replicates)..."
Rscript "$ROOT_DIR/analysis/analysis_vSubmit.R"

echo "Running descriptive tables..."
Rscript "$ROOT_DIR/tables/tables_vSubmit.R"

echo
echo "Done. Outputs in: $OUT_DIR"
