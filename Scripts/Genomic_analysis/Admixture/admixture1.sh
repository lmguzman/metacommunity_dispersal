#!/bin/bash


#Paths - To be set
#admix="/home/lmguzman/Sequencing/software/admixture_linux-1.3.0"
#base="/home/lmguzman/Sequencing/Tipulid/SNP_calls"
#scripts="/home/lmguzman/Sequencing/Tipulid"

admix="/Users/lmguzman/Documents/UBC/GBS/General_analysis/Admixture"
base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1"
scripts="/Users/lmguzman/Documents/UBC/GBS/General_analysis/bash_scripts"


for f in $base/*/*/
	do

			for r in 2 3 4 5

				do

					mkdir $f/admixture_results/run_${r}

						for K in 1 2 3 4 5

							do $admix/admixture -s time --cv $f/plink_other.bed $K | tee $f/admixture_results/run_${r}/log${K}.out 

								mv $scripts/plink_other.${K}.P $f/admixture_results/run_${r}/admix_${K}.P
								mv $scripts/plink_other.${K}.Q $f/admixture_results/run_${r}/admix_${K}.Q

							done

				done

	done

