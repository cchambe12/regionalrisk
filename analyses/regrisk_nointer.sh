#!/bin/bash

#SBATCH -p wolkovich

#SBATCH -n 24

#SBATCH -N 1

#SBATCH -t 0-99:00:00

#SBATCH --mem 90000

#SBATCH -o hostname.out

#SBATCH -e hostname.err

#SBATCH --mail-type=ALL

#SBATCH --mail-user=cchamberlain@g.harvard.edu

source new-modules.sh
module load R/3.4.2-fasrc01
module load R_packages


R CMD BATCH --quiet --no-restore --save /n/wolkovich_lab/Lab/Cat/Odys_brm_nointer.R brmsfit