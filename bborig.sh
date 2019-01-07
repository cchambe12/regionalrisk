#!/bin/bash

#SBATCH -p wolkovich

#SBATCH -n 4

#SBATCH -N 1

#SBATCH -t 0-100:00:00

#SBATCH --mem 30000

#SBATCH -o hostname.out

#SBATCH -e hostname.err

#SBATCH --mail-type=ALL

#SBATCH --mail-user=cchamberlain@g.harvard.edu

source new-modules.sh
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER
module load gcc/7.1.0-fasrc01 R_core/3.5.1-fasrc02
module load R_packages


R CMD BATCH --quiet --no-restore --save /n/wolkovich_lab/Lab/Cat/BB_orig.R rndslopes
