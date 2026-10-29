#!/bin/bash

# This script will perform Level 3 statistics in FSL.

set -euo pipefail

if [ "$#" -ne 3 ]; then
	echo "Usage: $0 <cope-number> <cope-name> <analysis-type>" >&2
	exit 2
fi

# ensure paths are correct irrespective from where user runs the script
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
maindir="$(dirname "$scriptdir")"
subjects="${scriptdir}/L3-act-subjects.txt"
covariates_file="${scriptdir}/L3-act-covariates.xlsx"

# study-specific inputs and general output folder

copenum=$1
copename=$2
analysis=$3
REPLACEME=$3 # act  ppi, nppi # this defines the parts of the path that differ across analyses
type=${REPLACEME} # For output template

# Variables that change per analysis. Check carefully!
covariate=brainhealth-environment

for required in "$subjects" "$covariates_file"; do
	if [ ! -r "$required" ]; then
		echo "Missing required L3 builder output: $required" >&2
		exit 1
	fi
done
N=$(wc -l < "$subjects")

if [[ $analysis == "act" ]]; then
	template=L3_task-ugr_model-3_type-act_group-${covariate}_n${N}_flame1.fsf
else
	template=L3_task-ugr_model-3_type-ppi_group-${covariate}_n${N}_flame1.fsf
fi


# Set once and then forget.
model=3 # or model 2
task=ugr
modeltype=flame1
templatedir="${maindir}/templates"
MAINOUTPUT=${maindir}/derivatives/fsl/L3-act/L3_model-${model}_task-${task}_type-${type}-n${N}-cov-${covariate}-${modeltype}


mkdir -p $MAINOUTPUT

# set outputs and check for existing
cnum_pad=$(printf "%02d" "$copenum")
OUTPUT=${MAINOUTPUT}/L3_model-${model}_task-${task}_n${N}-cov-${covariate}-cope-${copenum}_cname-${copename}-${modeltype}

	echo "[$(date)] re-doing: ${OUTPUT}" >> re-runL3.log
	rm -rf ${OUTPUT}.gfeat

	# create template and run FEAT analyses
	ITEMPLATE=${templatedir}/${template}
	if [ ! -r "$ITEMPLATE" ]; then
		echo "Missing current UGR design template: $ITEMPLATE" >&2
		echo "Do not reuse the archived 258-participant FEVS template." >&2
		exit 1
	fi
	OTEMPLATE=${MAINOUTPUT}/L3_task-${task}_model-${model}_type-${type}_cope-${copenum}_cname-${copename}_${covariate}_n${N}_${modeltype}.fsf
	sed -e 's@OUTPUT@'$OUTPUT'@g' \
	-e 's@COPENUM@'$copenum'@g' \
	-e 's@REPLACEME@'$REPLACEME'@g' \
	-e 's@BASEDIR@'$maindir'@g' \
	<$ITEMPLATE> $OTEMPLATE
	/usr/local/fsl/bin/feat $OTEMPLATE
	# feat $OTEMPLATE

# delete unused files
 rm -rf ${OUTPUT}.gfeat/cope${copenum}.feat/stats/res4d.nii.gz
 rm -rf ${OUTPUT}.gfeat/cope${copenum}.feat/stats/corrections.nii.gz
 rm -rf ${OUTPUT}.gfeat/cope${copenum}.feat/stats/threshac1.nii.gz
#rm -rf ${OUTPUT}.gfeat/cope${copenum}.feat/filtered_func_data.nii.gz
# rm -rf ${OUTPUT}.gfeat/cope${copenum}.feat/var_filtered_func_data.nii.gz
