#!/bin/bash


#Paths - To be set
#base="/home/lmguzman/Sequencing/Tipulid/SNP_calls"
base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1"

for f in $base/*/*/

	do

	for c in c1 c2
		
		do
			grep -h CV $f/$c/admixture_results/log*.out > $f/$c/admixture_results/cv_results.txt

		done

	done