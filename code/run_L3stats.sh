#!/bin/bash

# ensure paths are correct irrespective from where user runs the script
set -euo pipefail

scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ncopes=17
analysis=act # edit if necessary

for copenum in `seq $ncopes`; do

cnum_pad=$(printf "%02d" "$copenum")
	copename=contrast-${cnum_pad}

	# Manages the number of jobs and cores
	SCRIPTNAME=${scriptdir}/L3stats.sh
	NCORES=50
	while [ $(ps -ef | grep -v grep | grep $SCRIPTNAME | wc -l) -ge $NCORES ]; do
			sleep 5s
	done
	bash "$SCRIPTNAME" "$copenum" "$copename" "$analysis" &
	sleep 1s
done

wait
