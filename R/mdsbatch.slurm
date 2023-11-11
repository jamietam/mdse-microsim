#!/bin/bash
#SBATCH --job-name=mds-microsim-females-3days
#SBATCH --partition=week
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=2G
#SBATCH --time=3-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jamie.tam@yale.edu
#SBATCH --output=batch-%j.out

module load R/4.2.0-foss-2020b

Rscript mds_main.R
