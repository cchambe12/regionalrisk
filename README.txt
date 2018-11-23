Regional Risk

<><><><><><><><><><> 
Cleaning sequence...
<><><><><><><><><><>

## Github
https://github.com/cchambe12/regionalrisk

## Cleaning starts...
With analyses/scripts/species

analyses/scripts/regrisk/allspecies_climateprep.R
This script cleans the PEP data and gets climate data

<><><><><><><><><><><><
Prep Data for Model
<><><><><><><><><><><><

analyses/scripts/FalseSpring/FS_allspp.R
uses the above script to combine into a master list
	
	Meta:
	lat - latitude
	long - longitude
	PEP_ID - site ID for observation
	fs.count - number of times temps fell below -2.2degC for that year for that species
	year - year
	species - species code
	fs - if there was at least one day of freezing temperatures for that year for that species


analyses/scripts/MAT.R
collects mean annual temperature data and makes new output file...
mat_compressed.csv

analyses/scripts/gDistance.R
Gathers distance from coast data for each site

analyses/scripts/NAO.R
Gathers NAO data from Nov-Apr for each site and year

analyses/scripts/SpacePrep.R
Gathers all of the above information and collates MST, NAO, elevation, distance and false spring information and prepares for finding the space parameter
<><> output file is fs_allspp_orig_allpred.csv

<><><><><><><><><><><><
Spatial Autocorrelation
<><><><><><><><><><><><
analyses/scripts/Newspace.r
Sends off to Odyssey to get space parameter information based on Baumen et al. 2017
<><><> output file is fs_space_orig.csv

<><><><><><><><><><><><
Run the Model!
<><><><><><><><><><><><
analyses/scripts/Bern_original.R
Sends off to Odyssey for final model using fs_space_orig.csv


fs_space_orig.csv

	Meta:
	lat - latitude
	long - longitude
	year - year
	lat.long - latitude and longitude pasted together
	mat - mean annual temperature
	PEP_ID - site ID for observation
	fs.count - number of times temps fell below -2.2degC for that year for that species
	species - species code
	fs - if there was at least one day of freezing temperatures for that year for that species
	cc - if 0 then year is before 1983, if 1 then year is after 1983
	nao - NAO average index from Nov to Apr for each site and year
	mst - mean spring temperature from March 1st to May 30th
	elev - elevation
	distkm - distance from coast in km
	space - space parameter value


