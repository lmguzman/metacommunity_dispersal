#!/bin/bash


#Paths - To be set
base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/Damselfly/SNP_calls"

nfield="something"

for f in $base/*/*/
	do
		mkdir $f/tmp_files

		cut -f12-107 $f/HapMap.hmp.txt > $f/tmp_files/SNP.txt

		awk -f $base/transpose.awk $f/tmp_files/SNP.txt > $f/tmp_files/tSNP.txt

		nfield=$(awk '{print NF}' $f/tmp_files/tSNP.txt | uniq)

		cut -f2-${nfield} $f/tmp_files/tSNP.txt > $f/tmp_files/only_nd_SNP_letter.txt

		sed -e 's/A/AA/g' -e 's/T/TT/g' -e 's/C/CC/g' -e 's/G/GG/g' -e 's/M/AC/g' -e 's/R/AG/g' -e 's/W/AT/g' -e 's/S/CG/g' -e 's/Y/CT/g' -e 's/K/GT/g' -e 's/N/NA/g' $f/tmp_files/only_nd_SNP_letter.txt > $f/tmp_files/letter_no_tab_SNP.txt

		paste -d'\t' $base/pops_ordered $f/tmp_files/letter_no_tab_SNP.txt > $f/final_R_SNP.txt

	done
