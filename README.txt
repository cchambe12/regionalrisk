Regional Risk

<><><><><><><><><><> 
Cleaning sequence...
<><><><><><><><><><>

## Github
https://github.com/cchambe12/regionalrisk

## Cleaning starts...
** Step 1: regionalrisk/analyses/speciesdata_pep
	* Run through each species you wish to get an output file for
	* Repo currently only supports Aesculus hippocastenum, Alnus glutinosa, Betula pendula, Fagus sylvatica, Fraxinus excelsior, and Quercus robber

** Step 2: analyses/climatedata
	* Depending on model used you will need a different script
		a) allspecies_climateprep.R: prepares all species for the original and -5degC models
		b) allspecies_climateprep_dvr.R: prepares for varying durations of vegetative risk
		c) allspecies_climateprep_fullleaf.R: prepares for BBCH 11 being defined as budburst instead of leafout and adding 12 days to find leafout

** Step 3: analyses/countfalsesprings
	* Again, choose script that matches model of interest
	* This takes a long time and should be run in cluster

** Step 4: analyses/addin_allpreds
	** First, must run gdistance.R, meanspringtemp.R, and nao.R
		******** NOTE! ggdistance.R, meanspringtemp.R and nao.R are already prepared and do not have to be rerun, regardless of new model, unless you decide to 				use different months for MST!
	** Then, run spaceprep.R 
	** Then, go to the `mir' folder
		* run whichever model you are interested in
		* This takes a long time and should be run in cluster

** Step 5: analyses/models
	* Now you can run your model
	* This takes a long time and should be run in cluster

<><><><><><><><><><><><><><><><
Details on files for model prep
<><><><><><><><><><><><><><><><

analyses/scripts/countfalsesprings/FS_allspp.R
uses the above script to combine into a master list
	
	Meta:
	lat - latitude
	long - longitude
	PEP_ID - site ID for observation
	fs.count - number of times temps fell below -2.2degC for that year for that species
	year - year
	species - species code
	fs - if there was at least one day of freezing temperatures for that year for that species


analyses/addin_allpreds/meanspringtemp.R
collects mean annual temperature data and makes new output file...
mat_MAM.csv

analyses/addin_allpreds/gdistance.R
Gathers distance from coast data for each site

analyses/addin_allpreds/nao.R
Gathers NAO data from Nov-Apr for each site and year

analyses/addin_allpreds/spaceprep.R
Gathers all of the above information and collates MST, NAO, elevation, distance and false spring information and prepares for finding the space parameter
<><> output file is fs_allspp_orig_allpred.csv

<><><><><><><><><><><><
Spatial Autocorrelation
<><><><><><><><><><><><
analyses/addin_allpreds/mir/fixautocorrelation.r
Sends off to Odyssey to get space parameter information based on Baumen et al. 2017
<><><> output file is fs_space_orig.csv

<><><><><><><><><><><><
Run the Model!
<><><><><><><><><><><><
analyses/models/bern_original.R
Sends off to Odyssey for final model using fs_newspace_orig.csv


fs_newspace_orig.csv

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


