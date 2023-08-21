setwd(file.path("/gpfs/gibbs/project/tam_jamie/shared/mds-microsim/"))
library(openxlsx)

whichgender <- "males"

# Precompute annual mortality probabilities by birth cohort
hmd <- as.matrix(read.xlsx("data/hmd_mortality_age_year.xlsx",sheet=paste0(whichgender),rowNames=TRUE, colNames=TRUE, check.names=FALSE)) # Load all mortality inputs as matrices

cohorts <- 1900:2100

precompute_diag <- function(hmd, cohorts, finval) { 
  precomp = matrix(nrow=100, ncol=201)
  for (bc in cohorts) {
    ## Transition probabilities (per cycle) by birth cohort
    bc_i = bc - 1899
    precomp[1:min(100,202-bc_i),bc_i] <- diag(hmd[,bc_i:201])
    precomp[100,bc_i] <- finval
  }
  return(precomp)
}

p.HX <- precompute_diag(hmd,cohorts[-length(cohorts)],1) 

# format depression inputs
p.HD = matrix(nrow=100, ncol=201)
p.HD[,1:201] = as.matrix(read.xlsx("data/incidence_eaton.xlsx",sheet=whichgender,rowNames=TRUE, colNames=FALSE)[,1])
if (whichgender=="females"){
  MP=ns(0:21,knots=c(13,18)) ### Matrix of X's
  Rps=predict(MP,21)[1,] ## Predicts y value given a set of X's for age 22  # 0.0051285304 anchor at age 22
  y=c()
  for (j in 0:21){
    y=c(y,0.0051285304*exp(sum((MP[j,]-Rps)*c(4.832766251,-6.513158237,3.289480518)))) ## multiply by coefficients and sum , exp makes it positive for incidence
  }
  p.HD[0:22,]<-y
  p.HD[0:12,]<-rep(0,12) # assumes no 1st MDE before age 12
  # scale up incidence by year (p.HD is in age-cohort format) 
  # v_params = c("incSF_12.17"=1.62054949998856, "incSF_18.25"=2.75912116002291) # best fit females, fit_value=0.0189685713222878
  for (bc in cohorts){   
    bc1 = bc-1899
    for (age in 0:25){ # increase applies to youth and young adults ages 0-25
      if ((bc+age)>=2016 & age<18){
        p.HD[(age+1),bc1] = 1.62054949998856*p.HD[(age+1),bc1]
      }
      if ((bc+age)>=2016 & age>=18){ # starting in 2016
        p.HD[(age+1),bc1] = 2.75912116002291*p.HD[(age+1),bc1]
      }
    }
  }
}

if (whichgender=="males"){
  MP=ns(0:28,knots=c(13,18))
  Rps=predict(MP,28)[1,] ## Predicts y value given a set of X's for age 22  # 0.0019072600 anchor at age 29
  y=c()
  for (j in 0:28){
    y=c(y,0.0019072600*exp(sum((MP[j,]-Rps)*c(3.10610469809184,8.34323146575445,0.160023210647931)))) ## multiply by coefficients and sum , exp makes it positive for incidence
  }
  p.HD[0:29,]<-y
  p.HD[0:12,]<-rep(0,12) # assumes no 1st MDE before age 12
  # scale up incidence by year (p.HD is in age-cohort format)
  # v_params = c("incSF_12.17"=1.94638023432344, "incSF_18.25"=3.36043720226735) # best fit males, fit_value=0.00824625027868211
  for (bc in cohorts){   
    bc1 = bc-1899
    for (age in 0:25){ # increase applies to youth and young adults ages 0-25
      if ((bc+age)>=2016 & age<18){
        p.HD[(age+1),bc1] = 1.94638023432344*p.HD[(age+1),bc1]
      }
      if ((bc+age)>=2016 & age>=18){ # starting in 2016
        p.HD[(age+1),bc1] = 3.36043720226735*p.HD[(age+1),bc1]
      }
    }
  }
}

## Recovery
p.DR=NULL
p.DR[1:12]= p.DR[100] = 0 # probability to recover, final value = 0 because mortality prob = 1
p.DR[13:99] = 0.173 # assumes constant recovery by age

## Recurrence
p.RD=NULL
p.RD[1:12]= p.RD[100] = 0 # probability of recurrent MD if Former MD or Recall Error (R, E) 
p.RD[13:99] = 0.058 # assumes constant recovery by age

save(p.HX, p.HD, p.DR, p.RD, file=paste0("data/dep_precomputed_inputs_",whichgender,".RData"))
