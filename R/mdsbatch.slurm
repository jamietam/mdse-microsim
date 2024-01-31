#!/bin/bash
#SBATCH --job-name=mds-microsim-testparallel-1week-jzl9
#SBATCH --partition=week
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=2G
#SBATCH --time=7-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=john.liu@yale.edu
#SBATCH --output=batch-%j.out

module load R/4.2.0-foss-2020b

Rscript mds_calib.R females
Rscript mds_calib.R males
