#!/bin/bash
#SBATCH --job-name=mds-microsim-5min-jzl9
#SBATCH --partition=day
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=2G
#SBATCH --time=0-00:05:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=john.liu@yale.edu
#SBATCH --output=batch-%j.out

module load R/4.2.0-foss-2020b

Rscript mds_calib.R females
