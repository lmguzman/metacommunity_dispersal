#!/bin/bash


#Paths - To be set
#admix="/home/lmguzman/Sequencing/software/admixture_linux-1.3.0"
#base="/home/lmguzman/Sequencing/Tipulid/SNP_calls"
#scripts="/home/lmguzman/Sequencing/Tipulid"

admix="/Users/lmguzman/Documents/UBC/GBS/General_analysis/"
base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1"
scripts="/Users/lmguzman/Documents/UBC/GBS/General_analysis/bash_scripts"


for f in $base/*/*/
	do
		for c in c1 c2

			do

			for K in 1 2 3 4 5

				do 

				$admix/NGSadmix -likes input.gz -K 3 -P 4 -o myoutfiles -minMaf 0.05



				/admixture --cv $f/$c/plink_other.bed $K | tee $f/$c/admixture_results/log${K}.out 

				mv $scripts/plink_other.${K}.P $f/$c/admixture_results/admix_${K}.P
				mv $scripts/plink_other.${K}.Q $f/$c/admixture_results/admix_${K}.Q

				done

			done

	done


