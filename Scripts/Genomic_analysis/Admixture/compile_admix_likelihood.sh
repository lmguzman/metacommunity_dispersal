#!/bin/bash


#Paths - To be set
#base="/home/lmguzman/Sequencing/Tipulid/SNP_calls"
base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1"


for f in $base/*/*/

	do

		for r in 1 2 3 4 5

			do 

				grep -h -e "K=" -e "^Loglikelihood" $f/admixture_results/run_${r}/log*.out > $f/admixture_results/likelihood_results_${r}.txt

			done

	done



for f in $base/*/*/

	do

	for c in c1 c2

		do

		for r in 1 2 3 4 5

			do

				grep -h -e "K=" -e "^Loglikelihood" $f/$c/admixture_results/run_${r}/log*.out > $f/$c/admixture_results/likelihood_results_${r}.txt

			done

		done

	done