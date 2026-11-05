# directory (borrowed style from L1stats.sh)
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
basedir="$(dirname "$scriptdir")"

# Define input variables
sublist=${basedir}/code/L3-act-subjects.txt
task="ugr"
model="3"
sm="5"
mask=${basedir}/masks/pm25xmspss_cope12_mask.nii.gz
upstreamdir=/ZPOOL/data/projects/rf1-sra-ugr/derivatives/fsl-no-tedana-age55

# Output directory for the meants text files
outputdir=${basedir}/derivatives/extractions/pm25xmspss_cope12_ugr_meants
log=${basedir}/code/pm25xmspss_cope12_ugr_meants_log.txt
mkdir -p $outputdir

# Loop through each subject
for sub in `cat ${basedir}/code/L3-act-subjects.txt`; do

  # Define the path to the filtered functional data
  DATA=${upstreamdir}/sub-${sub}/ses-01/L2_task-${task}_ses-01_model-${model}_type-act_sm-${sm}.gfeat/cope12.feat/stats/zstat1.nii.gz

  DATArun1=${upstreamdir}/sub-${sub}/ses-01/L1_task-${task}_ses-01_model-${model}_type-act_run-1_sm-${sm}.feat/stats/zstat12.nii.gz
  
  DATArun2=${upstreamdir}/sub-${sub}/ses-01/L1_task-${task}_ses-01_model-${model}_type-act_run-2_sm-${sm}.feat/stats/zstat12.nii.gz
 

  # Check if the data file exists
  if [ -f "$DATA" ]; then   
     echo "Extracting L2 signal for subject: ${sub}" >> $log
    OUTPUT="${outputdir}/sub-${sub}_L2_pm25xmspss_meants.txt" 
    fslmeants -i $DATA -o $OUTPUT -m $mask
   	
    chmod 777 $OUTPUT
  else
    echo "sub${sub} ${DATA} doesn't exist" >> $log
  fi
  	
   if [ -f "$DATArun1" ]; then
   	 echo "Extracting run1 signal for subject: ${sub}" >> $log
   	 OUTPUT="${outputdir}/sub-${sub}_run-1_pm25xmspss_meants.txt"
   	 fslmeants -i $DATArun1 -o $OUTPUT -m $mask


     chmod 777 $OUTPUT
   else 
   	 echo "WARNING: Run-1 ${DATArun1} not found for subject ${sub}. Skipping." >> $log
   fi
 	if [ -f "$DATArun2" ]; then
   	 echo "Extracting run2 signal for subject: ${sub}" >> $log
   	 OUTPUT="${outputdir}/sub-${sub}_run-2_pm25xmspss_meants.txt"
   	 fslmeants -i $DATArun2 -o $OUTPUT -m $mask
 

     chmod 777 $OUTPUT
   else 
   	 echo "WARNING: Run-2 ${DATArun2} not found for subject ${sub}. Skipping." >> $log
   fi


done

echo "Extraction complete. All output files saved to: $outputdir"

