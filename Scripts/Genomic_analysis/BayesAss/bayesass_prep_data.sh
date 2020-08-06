


#!/bin/bash


#Paths - To be set
#base="/home/lmguzman/Sequencing/Tipulid/SNP_calls"
base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1"

for f in $base/*/*/c1/bayesass_outputs/*

	do

		grep 'm\[' $f/BA3out.txt > $f/migration_rates.txt

		grep '\[' $f/BA3indiv.txt > $f/migrant_ancestries.txt

		grep 'Individual:' $f/BA3indiv.txt > $f/individuals.txt
		

	done

