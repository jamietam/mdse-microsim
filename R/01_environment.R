setwd(file.path(mainDir))

## R environment and install package dependencies
# install CRAN packages
packages <- c('stringr','splines','foreach','doParallel','ggplot2','gridBase','gridExtra','grid','ggrepel',
              'lhs','matrixStats','backports','devtools','ellipse','tidyr','dplyr','reshape2')
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# install GitHub packages
if ('darthtools' %in% rownames(installed.packages())==FALSE){
  devtools::install_github("DARTH-git/darthtools")
}
if ('dampack' %in% rownames(installed.packages())==FALSE){
  devtools::install_github("DARTH-git/dampack")
}
# load all packages
lapply(c(packages, 'darthtools','dampack'), library, character.only=TRUE)

# Determine number of cores and cluster setup based on HPC or personal computer
n.cores <- Sys.getenv("SLURM_CPUS_PER_TASK")
if (hpc == 1) { ## For HPC runs - Run this section of code, and NOT the one below
  cl <- makeCluster(as.numeric(n.cores), type = "FORK")
} else { ## For Personal Computer and Open On Demand Interface, Run this section of code and NOT the one above
  cl <- makeCluster(detectCores())
}
registerDoParallel(cl)