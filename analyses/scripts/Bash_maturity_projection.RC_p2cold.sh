#!/bin/bash

#SBATCH -n 20 # Number of cores requested

#SBATCH -N 1 # Ensure that all cores are on one machine

#SBATCH -t 3-2:15:00 # Runtime in minutes

#SBATCH -p wolkovich # Partition to submit to (mine!)

#SBATCH --mem=40000 # Memory per cpu in MB (see also --mem-per-cpu)

#SBATCH -o hostnames_p2pr.out # Standard out goes to this file

#SBATCH -e hostnames_p2pr.err # Standard err goes to this filehostname

#SBATCH --mail-type=ALL # email notification

#SBATCH --mail-user=moralescastilla@fas.harvard.edu # email for notifi

source new-modules.sh
module load R/3.3.0-fasrc01
module load gdal/2.1.2-fasrc01
module load proj/4.9.3-fasrc01
module load R_packages

R CMD BATCH --quiet --no-restore --no-save /n/wolkovich_lab/Lab/Nacho/Cold_thresh_def.R ggcmp2cold
