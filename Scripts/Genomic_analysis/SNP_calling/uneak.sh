#!/bin/bash


#Paths - To be set
tassel="/SciBorg/array0/guzman/Sequencing/Tassel/tassel3-standalone"
base="/SciBorg/array0/guzman/Sequencing/Tipulid/Uneak_filt"
illumina="/SciBorg/array0/guzman/Sequencing/Tipulid/UKEAK_tipulid/Illumina/CBRY8ANXX_6_GBSPstI.fastq"
key="/SciBorg/array0/guzman/Sequencing/Tipulid/UKEAK_tipulid/key/GBS-PstI_barcodes_flow.txt"
enzyme="PstI-MspI"
#Filters for the last plug-in

#UNEAK command lines
perl $tassel/run_pipeline.pl -Xms512m -Xmx48g -fork1 -UCreatWorkingDirPlugin -w $base -endPlugin -runfork1 
cp $illumina $base/Illumina
cp $key $base/key
perl $tassel/run_pipeline.pl -Xms512m -Xmx48g -fork1 -UFastqToTagCountPlugin -w $base -e $enzyme -endPlugin -runfork1 


for mnC in 0.08 0.1

do
	for c in 10 15 50
	do
		perl $tassel/run_pipeline.pl -Xms512m -Xmx48g -fork1 -UMergeTaxaTagCountPlugin -w $base -c $c -endPlugin -runfork1 
		perl $tassel/run_pipeline.pl -Xms512m -Xmx48g -fork1 -UTagCountToTagPairPlugin -w $base -e 0.03 -endPlugin -runfork1 
		perl $tassel/run_pipeline.pl -Xms512m -Xmx48g -fork1 -UTagPairToTBTPlugin -w $base -endPlugin -runfork1 
		perl $tassel/run_pipeline.pl -Xms512m -Xmx48g -fork1 -UTBTToMapInfoPlugin -w $base -endPlugin -runfork1 
		perl $tassel/run_pipeline.pl -Xms512m -Xmx48g -fork1 -UMapInfoToHapMapPlugin -w $base -mnMAF 0.05 -mxMAF 0.5 -mnC $mnC -mxC 1 -endPlugin -runfork1
		tr '|' '\t' < $base/hapMap/HapMap.hmc.txt > $base/hapMap/HapMap.hmc_mod.txt
		mkdir /SciBorg/array0/guzman/Sequencing/Tipulid/all_hapmap_outputs/output_c_${c}_mnc_${mnC}/
		mv $base/hapMap/* /SciBorg/array0/guzman/Sequencing/Tipulid/all_hapmap_outputs/output_c_${c}_mnc_${mnC}/

	done
done



