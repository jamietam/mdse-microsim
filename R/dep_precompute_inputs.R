setwd(file.path("/gpfs/gibbs/project/tam_jamie/shared/mds-microsim/"))
library(openxlsx)

whichgender <- "males"

# Format depression inputs by age-calendaryear
p.HD = matrix(nrow=100, ncol=201)
p.HD[,1:201] = as.matrix(read.xlsx("data-raw/incidence_eaton.xlsx",sheet=whichgender,rowNames=TRUE, colNames=FALSE)[,1])
if (whichgender=="females"){
  MP=ns(0:21,knots=c(13,18)) ### Matrix of X's
  Rps=predict(MP,21)[1,] ## Predicts y value given a set of X's for age 22  # 0.0051285304 anchor at age 22
  y=c()
  for (j in 0:21){
    y=c(y,0.0051285304*exp(sum((MP[j,]-Rps)*c(4.832766251,-6.513158237,3.289480518)))) ## multiply by coefficients and sum , exp makes it positive for incidence
  }
  p.HD[0:22,]<-y
  p.HD[0:12,]<-rep(0,12) # assumes no 1st MDE before age 12
} else {
  MP=ns(0:28,knots=c(13,18))
  Rps=predict(MP,28)[1,] ## Predicts y value given a set of X's for age 22  # 0.0019072600 anchor at age 29
  y=c()
  for (j in 0:28){
    y=c(y,0.0019072600*exp(sum((MP[j,]-Rps)*c(3.10610469809184,8.34323146575445,0.160023210647931)))) ## multiply by coefficients and sum , exp makes it positive for incidence
  }
  p.HD[0:29,]<-y
  p.HD[0:12,]<-rep(0,12) # assumes no 1st MDE before age 12
}

colnames(p.HD) <- 1900:2100

## Recovery
p.DR=NULL
p.DR[1:12]= p.DR[100] = 0 # probability to recover, final value = 0 because mortality prob = 1
p.DR[13:99] = 0.173 # assumes constant recovery by age

## Recurrence
p.RD=NULL
p.RD[1:12]= p.RD[100] = 0 # probability of recurrent MD if Former MD or Recall Error (R, E) 
p.RD[13:99] = 0.058 # assumes constant recovery by age

save(p.HD, p.DR, p.RD, file=paste0("data/dep_precomputed_inputs_",whichgender,".RData"))
