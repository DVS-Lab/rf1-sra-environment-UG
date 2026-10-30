#!/bin/bash

# This run_* script is a wrapper for L3stats.sh, so it can loop over selected
# copes and models. Contrast N for PPI is always PHYS in these models.

set -euo pipefail

# Ensure paths are correct irrespective of where the user runs this script.
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Change the type of analysis in the loop below if needed.
for analysis in act; do # "act" "ppi_seed-pTPJ"
# for analysis in ppi_seed-aIns ppi_seed-AI; do

	analysistype=${analysis}

	# Active prespecified contrasts for this analysis.
	for copeinfo in "11 offer-unfairness_pmod" "12 social-nonsocial_pmod"; do
	# To run every task contrast instead, comment the line above and uncomment this line:
	# for copeinfo in "1 nonsocial_high_c" "2 nonsocial_high_pmod" "3 nonsocial_low_c" "4 nonsocial_low_pmod" "5 social_high_c" "6 social_high_pmod" "7 social_low_c" "8 social_low_pmod" "9 endowment_high-low_c" "10 social-nonsocial_c" "11 offer-unfairness_pmod" "12 social-nonsocial_pmod" "13 nonsocial_pmod" "14 social_pmod" "15 endowment_high-low_pmod" "16 nonsocial_high-low_pmod" "17 social_high-low_pmod"; do

		# Split the contrast number and name.
		set -- $copeinfo
		copenum=$1
		copename=$2

		# Manage the number of simultaneous L3 jobs.
		NCORES=50
		SCRIPTNAME=${scriptdir}/L3stats.sh
		echo "$SCRIPTNAME"
		while [ "$(ps -ef | grep -v grep | grep "$SCRIPTNAME" | wc -l)" -ge "$NCORES" ]; do
			sleep 1s
		done

		bash "$SCRIPTNAME" "$copenum" "$copename" "$analysistype" &
		echo "$SCRIPTNAME $copenum $copename $analysistype"
	done
done

wait
