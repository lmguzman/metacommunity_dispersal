#!/bin/bash


#Paths - To be set
tassel="/Users/lmguzman/Documents/UBC/GBS/General_analysis/tassel-5-standalone"
base="/Users/lmguzman/Documents/UBC/GBS/General_analysis/Tipulid/SNP_calls"


for f in $base/*/*/
	do

		perl $tassel/run_pipeline.pl -fork1 -h $f/HapMap.hmp.txt -export $f/plink -exportType Plink -runfork1

	done
