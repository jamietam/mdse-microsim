#!/bin/bash
#SBATCH --job-name=mds-microsim-males-1day-jt936
#SBATCH --partition=day
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=2G
#SBATCH --time=1-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jamie.tam@yale.edu
#SBATCH --output=batch-%j.out

module load R/4.2.0-foss-2020b

Rscript mds_calib.R
