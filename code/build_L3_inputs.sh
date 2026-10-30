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
subjects_file="${SCRIPT_DIR}/L3-act-subjects.txt"
paths_file="${SCRIPT_DIR}/L3-act-input-paths.txt"
selected_file="${SCRIPT_DIR}/L3-act-selected-inputs.tsv"
final_qc_file="${SCRIPT_DIR}/L3-act-final-qc.csv"
covariates_file="${SCRIPT_DIR}/L3-act-covariates.xlsx"
exclusions_file="${SCRIPT_DIR}/L3-act-exclusions.tsv"

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

# Keep one generic UGR cope path per subject in the same order as the subject
# list. L3stats.sh replaces COPENUM with the requested contrast number. For L2
# fixed-effects outputs, the contrast number belongs to the copeN.feat
# directory and the image inside that directory is always stats/cope1. For L1
# outputs, the contrast number belongs to the stats/copeN image itself.
awk -F '\t' '
NR == 1 {
    for (column = 1; column <= NF; column++) header[$column] = column
    next
}
$(header["contrast"]) == 1 {
    path = $(header["cope"])
    level = $(header["level"])
    if (level == "L2") {
        replaced = sub(/\/cope1\.feat\/stats\/cope1\.nii\.gz$/, "/copeCOPENUM.feat/stats/cope1.nii.gz", path)
    } else if (level == "L1") {
        replaced = sub(/\/stats\/cope1\.nii\.gz$/, "/stats/copeCOPENUM.nii.gz", path)
    } else {
        replaced = 0
    }
    if (!replaced) {
        print "ERROR: could not generalize " level " cope path: " path > "/dev/stderr"
        exit 1
    }
    print path
}' "${tmpdir}/selected-inputs.tsv" > "${tmpdir}/input-paths.txt"

path_n="$(wc -l < "${tmpdir}/input-paths.txt")"
[[ "$subject_n" -eq "$path_n" ]] || {
    echo "ERROR: L3 input-path count does not match the subject count." >&2
    exit 1
}

grep -q 'COPENUM' "${tmpdir}/input-paths.txt" || {
    echo "ERROR: could not create generic UGR cope paths." >&2
    exit 1
}

mv "${tmpdir}/subjects.txt" "$subjects_file"
mv "${tmpdir}/input-paths.txt" "$paths_file"
mv "${tmpdir}/selected-inputs.tsv" "$selected_file"
mv "${tmpdir}/final-qc.csv" "$final_qc_file"
mv "${tmpdir}/covariates.xlsx" "$covariates_file"
mv "${tmpdir}/exclusions.tsv" "$exclusions_file"

# Remove outputs created by the earlier layout after the replacement files are
# safely in place. The per-contrast manifests duplicated selected-inputs.tsv.
rm -rf -- "${l3_dir}/L3-act-contrast-inputs"
rm -f -- \
    "${l3_dir}/L3-act-subjects.txt" \
    "${l3_dir}/L3-act-input-paths.txt" \
    "${l3_dir}/L3-act-selected-inputs.tsv" \
    "${l3_dir}/L3-act-final-qc.csv" \
    "${l3_dir}/L3-act-covariates.xlsx" \
    "${l3_dir}/L3-act-exclusions.tsv"

echo "Sophia UGR L3 inputs complete (session ${session}; n=${subject_n})."
echo "Subjects:   $subjects_file"
echo "Paths:      $paths_file"
echo "Inputs:     $selected_file"
echo "Covariates: $covariates_file"
echo "Exclusions: $exclusions_file"
