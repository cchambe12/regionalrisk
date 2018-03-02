Regional Risk

<><><><><><><><><><> 
Cleaning sequence...
<><><><><><><><><><>

## Github
https://github.com/cchambe12/regionalrisk

## Cleaning starts...
analyses/scripts/regrisk
These scripts clean the PEP data in order to prepare for further analysis

analyses/scripts/FalseSpring/cleaning_fscount.R
uses the above scripts to combine into a master list
	
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

analyses/scripts/Prep_testing.R
makes final .csv file - fs_matspsite.csv

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


<><><><><><><><><><><><
Spatial Autocorrelation
<><><><><><><><><><><><

Use analyses/scripts/Eigenvectors.R
for spatial autocorrelation stuff
