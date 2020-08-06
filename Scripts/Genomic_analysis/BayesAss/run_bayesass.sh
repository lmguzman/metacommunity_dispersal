#!/bin/bash


#Paths - To be set
ba="/Users/lmguzman/Documents/UBC/GBS/General_analysis/BA3-3.04/distribution/OSX"
#base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/Damselfly/SNP_calls"
f="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/"

counter="1"

for s in $f/bayesass_files/

	do
		mkdir $f/bayesass_outputs/shuffle_${counter}

		for n in 1 2 3 4 5 6 7 8 9 10

		do

			$ba/BA3X86_64 -o $f/bayesass_outputs/shuffle_${counter}/out_${n}.txt $s -i10000000 -b1000000 -n1000 -t -g -m0.3 -a0.6 -f0.4

			mv BA3trace.txt $f/bayesass_outputs/shuffle_${counter}/trace_${n}.txt

			mv BA3indiv.txt $f/bayesass_outputs/shuffle_${counter}/ind_${n}.txt

		done

		let "counter++"

	done


g="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/"

counter="1"

for s in $g/bayesass_files/

	do
		mkdir $g/bayesass_outputs/shuffle_${counter}

		for n in 1 2 3 4 5 6 7 8 9 10

		do

			$ba/BA3 -o $g/bayesass_outputs/shuffle_${counter}/out_${n}.txt $s -i10000000 -b1000000 -n1000 -t -g -m0.3 -a0.6 -f0.4

			mv BA3trace.txt $g/bayesass_outputs/shuffle_${counter}/trace_${n}.txt

			mv BA3indiv.txt $g/bayesass_outputs/shuffle_${counter}/ind_${n}.txt

		done

		let "counter++"

	done