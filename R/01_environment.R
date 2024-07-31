setwd(file.path(mainDir))

## R environment and install package dependencies
packages <- c('stringr','lbfgsb3c','splines','foreach',
              'ggplot2','gridBase','gridExtra','grid','lhs','matrixStats','backports',
              'devtools','ellipse','ggrepel','doParallel','tidyr','dplyr')
# install.packages(packages)
# devtools::install_github("DARTH-git/darthtools")
lapply(c(packages, 'darthtools'), library, character.only=TRUE)
# Determine number of cores and cluster setup based on HPC or personal computer
n.cores <- Sys.getenv("SLURM_CPUS_PER_TASK")
if (hpc == 1) { ## For HPC runs - Run this section of code, and NOT the one below
  cl <- makeCluster(as.numeric(n.cores), type = "FORK")
} else { ## For Personal Computer and Open On Demand Interface, Run this section of code and NOT the one above
  cl <- makeCluster(detectCores())
  args <- c("females", 1000, 2100, 40)  # Parameters for non-HPC setup
}
registerDoParallel(cl)
args <- `if`(hpc == 1, commandArgs(TRUE), args)

