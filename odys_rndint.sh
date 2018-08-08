#!/bin/bash

#SBATCH -p wolkovich

#SBATCH -n 4

#SBATCH -N 1

#SBATCH -t 0-100:00:00

#SBATCH --mem 100000

#SBATCH -o hostname.out

#SBATCH -e hostname.err

#SBATCH --mail-type=ALL

#SBATCH --mail-user=cchamberlain@g.harvard.edu

source new-modules.sh
module load R/3.4.2-fasrc01
module load R_packages


R CMD BATCH --quiet --no-restore --save /n/wolkovich_lab/Lab/Cat/Odys_winter_intercept.R rndint
