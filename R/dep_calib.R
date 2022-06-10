rm(list = ls())  # remove any variables in R's memory
library(here)
library(stringr)
library(doParallel) # set up model to run in parallel
library(splines)
library(openxlsx)
library(foreach) # parallelization is in the foreach loop
cl <- makeCluster(detectCores()-2) # Leave 2 cores unused
registerDoParallel(cl)
setwd(file.path("C:/Users/JT936/Dropbox/GitHub/mds-microsim"))
here::i_am("R/dep_calib.R")
whichgender ="females"
# depression inputs
p.HD = read.xlsx("data/incidence_eaton.xlsx",sheet=paste0(whichgender),rowNames=TRUE, colNames=FALSE, check.names=FALSE)$X2 
p.HD[0:12]<-0
# p.HD.2016 = p.HD # NEED TO FIGURE OUT HOW TO scale up MDE incidence for those ages <25 starting in 2016
# p.HD.2016[0:26]<-p.HD.2016[0:26]*2.3823137 # inc_SF = 2.3823137

p.DR = c(rep(0.173,99),0) # probability to recover
p.RD = p.UD = c(rep(0.058,99),0) # probability of recurrent MD if Former MD or Recall Error (R, E) # ESTIMATE DURING CALIBRATION - leading to negative probabilities for specific birth cohorts/ages
p.RU = c(rep(0,25),rep(0.152,9),rep(0.101,15),rep(0.120,15),rep(0.923,35)) # probability to Recall Error (E) when Former MD (R)


