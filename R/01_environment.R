## R environment and install package dependencies
packages <- c('stringr','lbfgsb3c','splines','foreach',
              'ggplot2','gridBase','gridExtra','grid','lhs','matrixStats','backports',
              'devtools','ellipse','ggrepel')
# install.packages(packages)
# devtools::install_github("DARTH-git/darthtools")
lapply(c(packages, 'darthtools'), library, character.only=TRUE)

## For HPC runs - Run this section of code, and NOT the one below
library(doParallel) ## Run on a single node
n.cores = Sys.getenv("SLURM_CPUS_PER_TASK")
cl <- makeCluster(as.numeric(n.cores),type="FORK")
registerDoParallel(cl)
args <- commandArgs(TRUE)

## For Personal Computer and Open On Demand Interface, Run this section of code and NOT the one above
# library(doParallel) # set up model to run in parallel
# n.cores = Sys.getenv("SLURM_CPUS_PER_TASK")
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# args <- c("females",1000,2016, 40) # whichgender, n.i, max(cohorts), number of calib starting points
