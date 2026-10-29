#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Build repository-local L3 artifacts from the public-safe fixed analysis file
# and the validated imaging handoff on the Linux analysis machine.
source "${SCRIPT_DIR}/project_config.sh"

session="01"
analysis_file="${ANALYSIS_FILE:-${PROJECT_ROOT}/data/derived/UG_analysis_data_2026-10-26.xlsx}"
helper="${SCRIPT_DIR}/build_L3_covariates.py"

l3_dir="${PROJECT_ROOT}/data/derived/l3"
mkdir -p "$l3_dir"
subjects_file="${l3_dir}/L3-act-subjects.txt"
selected_file="${l3_dir}/L3-act-selected-inputs.tsv"
contrast_dir="${l3_dir}/L3-act-contrast-inputs"
final_qc_file="${l3_dir}/L3-act-final-qc.csv"
covariates_file="${l3_dir}/L3-act-covariates.xlsx"
exclusions_file="${l3_dir}/L3-act-exclusions.tsv"

for required in "$analysis_file" "$SELECTED_INPUTS" "$RUN_DECISIONS" "$helper"; do
    [[ -r "$required" ]] || {
        echo "ERROR: required input is not readable: $required" >&2
        exit 1
    }
done

[[ -d "$BIDS_ROOT" ]] || {
    echo "ERROR: BIDS root not found: $BIDS_ROOT" >&2
    exit 1
}

python3 -c 'import openpyxl' >/dev/null 2>&1 || {
    echo "ERROR: Python package 'openpyxl' is required." >&2
    exit 1
}

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

python3 "$helper" inputs \
    --analysis "$analysis_file" \
    --selected-inputs "$SELECTED_INPUTS" \
    --run-decisions "$RUN_DECISIONS" \
    --subjects-output "${tmpdir}/subjects.txt" \
    --selected-output "${tmpdir}/selected-inputs.tsv" \
    --contrast-output-dir "${tmpdir}/contrast-inputs" \
    --final-qc-output "${tmpdir}/final-qc.csv" \
    --exclusions-output "${tmpdir}/exclusions.tsv"

python3 "$helper" workbook \
    --analysis "$analysis_file" \
    --subjects "${tmpdir}/subjects.txt" \
    --final-qc "${tmpdir}/final-qc.csv" \
    --bids-root "$BIDS_ROOT" \
    --output "${tmpdir}/covariates.xlsx"

subject_n="$(wc -l < "${tmpdir}/subjects.txt")"
selected_n="$(awk 'NR > 1 {print $1}' "${tmpdir}/selected-inputs.tsv" | sort -u | wc -l)"
[[ "$subject_n" -eq "$selected_n" ]] || {
    echo "ERROR: subject and selected-input participant counts do not match." >&2
    exit 1
}

for contrast_file in "${tmpdir}"/contrast-inputs/contrast-*.tsv; do
    row_n="$(( $(wc -l < "$contrast_file") - 1 ))"
    [[ "$subject_n" -eq "$row_n" ]] || {
        echo "ERROR: $(basename "$contrast_file") has $row_n rows; expected $subject_n." >&2
        exit 1
    }
done

mv "${tmpdir}/subjects.txt" "$subjects_file"
mv "${tmpdir}/selected-inputs.tsv" "$selected_file"
rm -rf -- "$contrast_dir"
mv "${tmpdir}/contrast-inputs" "$contrast_dir"
mv "${tmpdir}/final-qc.csv" "$final_qc_file"
mv "${tmpdir}/covariates.xlsx" "$covariates_file"
mv "${tmpdir}/exclusions.tsv" "$exclusions_file"

echo "Sophia UGR L3 inputs complete (session ${session}; n=${subject_n})."
echo "Subjects:   $subjects_file"
echo "Inputs:     $selected_file"
echo "Contrasts:  $contrast_dir"
echo "Covariates: $covariates_file"
echo "Exclusions: $exclusions_file"
