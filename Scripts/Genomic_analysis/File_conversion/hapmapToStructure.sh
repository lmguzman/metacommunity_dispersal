
#!/bin/bash


#Paths - To be set
base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/output_c_15_mnc_0.1/Tipulid/mxc_2000_mis_40/c1"

mkdir $base/tmp_files

cut -f12-107 HapMap.hmp.txt > $base/tmp_files/SNP.txt

# Transpose the table using transpose.awk

awk -f /Users/lmguzman/Documents/UBC/GBS/General_analysis/Damselfly/SNP_calls/transpose.awk $base/tmp_files/SNP.txt > $base/tmp_files/tSNP.txt


##### CORRECT

sed -e 's/A/1_1/g' -e 's/T/2_2/g' -e 's/C/3_3/g' -e 's/G/4_4/g' -e 's/M/1_3/g' -e 's/R/1_4/g' -e 's/W/1_2/g' -e 's/S/3_4/g' -e 's/Y/3_2/g' -e 's/K/4_2/g' -e 's/N/0_0/g' $base/tmp_files/tSNP.txt > $base/tmp_files/number_diploid_SNP.txt

nfield=$(awk '{print NF}' $base/tmp_files/number_diploid_SNP.txt | uniq)

# cut sample identifyer colums

cut -f2-${nfield} $base/tmp_files/number_diploid_SNP.txt > $base/tmp_files/only_nd_SNP.txt

#Add proper sample identifyer and populations

paste -d'\t' pops.txt $base/tmp_files/only_nd_SNP.txt > $base/structureSNP.txt

