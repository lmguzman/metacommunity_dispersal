#!/bin/bash


#Paths - To be set
ba="/Users/lmguzman/Documents/UBC/GBS/General_analysis/BA3-3.04/distribution/OSX"
#base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/Damselfly/SNP_calls"
f="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1/Damselfly/mxc_2000_mis_40/c1/"


for n in 1 2 3 4 5 

	do

		$ba/BA3X86_64 $f/all_bayesass.txt -o $f/bayesass_outputs/out_${n}.txt -i10000000 -b1000000 -n1000 -t -g -m0.3 -a0.6 -f0.4

		mv BA3trace.txt $f/bayesass_outputs/trace_${n}.txt

		mv BA3indiv.txt $f/bayesass_outputs/ind_${n}.txt

	done


g="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c1/"


for n in 1 2 3 4 5 

	do

		$ba/BA3X86_64 $g/all_bayesass.txt -o $g/bayesass_outputs/out_${n}.txt -i10000000 -b1000000 -n1000 -t -g -m0.3 -a0.6 -f0.4

		mv BA3trace.txt $g/bayesass_outputs/trace_${n}.txt

		mv BA3indiv.txt $g/bayesass_outputs/ind_${n}.txt

	done

### Damselfly $ba/BA3X86_64 -v all_bayesass.txt -i10000000 -b1000000 -n1000 -t -g -m0.3 -a0.6 -f0.1
### Tipulid $ba/BA3X86_64 -v all_bayesass.txt -i10000000 -b1000000 -n1000 -t -g -m0.3 -a0.6 -f0.2