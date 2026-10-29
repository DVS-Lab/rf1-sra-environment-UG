#!/usr/bin/env bash
# Shared read-only input paths for the downstream RF1-SRA BHE UGR group
# analysis in rf1-sra-environment-UG.
# Source this file; do not execute it directly.

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." >/dev/null 2>&1 && pwd)"

# Authoritative preprocessing and validated paper handoff. Never write here.
UPSTREAM_ROOT="${RF1_SRA_UPSTREAM_ROOT:-/ZPOOL/data/projects/rf1-sra-linux2}"
BIDS_ROOT="${BIDS_ROOT:-${UPSTREAM_ROOT}/bids}"
HANDOFF_ROOT="${RF1_SRA_HANDOFF_ROOT:-${UPSTREAM_ROOT}/work/coverage95-handoff-20261021-204525}"
SELECTED_INPUTS="${SELECTED_INPUTS:-${HANDOFF_ROOT}/ugr/selected_inputs.tsv}"
RUN_DECISIONS="${RUN_DECISIONS:-${HANDOFF_ROOT}/run_decisions.tsv}"

