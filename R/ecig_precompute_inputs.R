# E-cigarette transitions -------------------------------------------------
library(darthtools)
mainDir = "/Users/jt936/Dropbox/GitHub/mdse-microsim/"
setwd(mainDir)
# Data sources:
# 2013-2014: Brouwer AF, et al. Transitions between cigarette, ENDS and dual use in adults in the PATH study (waves 1-4): multistate transition modelling accounting for complex survey design. Tob Control. 2020 Nov 16:tobaccocontrol-2020-055967. doi: 10.1136/tobaccocontrol-2020-055967. 
# 2015-2016: Brouwer AF, et al. Changing patterns of cigarette and ENDS transitions in the USA: a multistate transition analysis of youth and adults in the PATH Study in 2015-2017 vs 2017-2019 Tobacco Control Published Online First: 28 March 2023. doi: 10.1136/tc-2022-057905
# 2017-2021: Brouwer AF, et al. Changing patterns of cigarette and ENDS transitions in the USA: a multistate transition analysis of adults in the PATH Study in 2017–2019 vs 2019–2021  doi: 10.1136/tc-2023-058453 

whichgender <- "males"

## ecig initiation 
# never established use --> ENDS only
p.NO.NE <- rate_to_prob(matrix(c(0.006, 0.006, 0.013, 0.013,
                                 0.026, 0.026, 0.054, 0.054,
                    0.01, 0.01, 0.018, 0.018, 
                    0.003, 0.002, 0.002, 0.002, 
                    0.001, 0.001, 0, 0, 
                    0.001, 0, 0, 0), 
                  nrow = 6, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("12.14", "15.17", "18.24", "25.34", "35.54", "55.90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021"))))

# cig-only-->dual
p.CQ.CE <- p.CO.CE <- rate_to_prob(matrix(c(0.16, 0.16, 0.419, 0.419,
                                            0.152, 0.152, 0.223, 0.223,
                                0.073, 0.069, 0.102, 0.153, 
                               0.055, 0.051, 0.052, 0.068, 
                               0.039, 0.034, 0.025, 0.023, 
                               0.02, 0.015, 0.025, 0.023), 
                             nrow = 6, ncol = 4, byrow = TRUE, 
                             dimnames = list(c("12.14", "15.17", "18.24", "25.34", "35.54", "55.90"),
                                             c("2013-2014", "2015-2016", "2017-2019", "2020-2021"))))

# cig-only--> ENDS only (complete switching)
p.CO.FE <- rate_to_prob(matrix(c(0.089, 0.089, 0.105, 0.105,
                                 0.04, 0.04, 0.058, 0.058,
                                 0.018, 0.019, 0.032, 0.045,
                                 0.015, 0.013, 0.017, 0.012,
                                 0.008, 0.006, 0.007, 0.003,
                                 0.007, 0.006, 0.004, 0.003),
                               nrow = 6, ncol = 4, byrow = TRUE, 
                               dimnames = list(c("12.14", "15.17", "18.24", "25.34", "35.54", "55.90"),
                                               c("2013-2014", "2015-2016", "2017-2019", "2020-2021"))))
##non-current --> ENDS only
p.FQ.FE <- p.NQ.NE <- p.FO.FE <- rate_to_prob(matrix(c(0.324, 0.324, 0.162, 0.162,
                                                       0.182, 0.182, 0.333, 0.333,
                                          0.046, 0.057, 0.118, 0.09, 
                                          0.018, 0.017, 0.019, 0.029, 
                                          0.009, 0.008, 0.004, 0.003, 
                                          0.002, 0.002, 0.004, 0.003), 
                                        nrow = 6, ncol = 4, byrow = TRUE, 
                                        dimnames = list(c("12.14", "15.17", "18.24", "25.34", "35.54", "55.90"),
                                                        c("2013-2014", "2015-2016", "2017-2019", "2020-2021"))))# columns

## ecig cessation
# e-cig only --> non-current
p.NE.NQ <- p.FE.FQ <- rate_to_prob(matrix(c(0.46, 0.46, 0.393, 0.393,
                                            0.402, 0.402, 0.235, 0.235,
                               0.313, 0.329, 0.193, 0.204, 
                               0.199, 0.194, 0.137, 0.244, 
                               0.187, 0.206, 0.113, 0.129, 
                               0.111, 0.101, 0.113, 0.129), 
                             nrow = 6, ncol = 4, byrow = TRUE, 
                             dimnames = list(c("12.14", "15.17", "18.24", "25.34", "35.54", "55.90"),
                                             c("2013-2014", "2015-2016", "2017-2019", "2020-2021"))))
# dual-->cig-only
p.CE.CQ <- rate_to_prob(matrix(c(0.352, 0.352, 0.008, 0.008,
                                 0.377, 0.377, 0.184, 0.184,
                    0.413, 0.435, 0.152, 0.18, 
                    0.457, 0.433, 0.23, 0.225, 
                    0.451, 0.462, 0.291, 0.323, 
                    0.44, 0.435, 0.291, 0.323), 
                  nrow = 6, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("12.14", "15.17", "18.24", "25.34", "35.54", "55.90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021"))))


##expand probability matrix to match age 1-100 and years 2017-2021
expandmatrix <- function(pmatrix){
  ##create new expanded matrix 
  expmatrix <- matrix(0, nrow = 100, ncol = 201, byrow = TRUE, dimnames = list(1:100, 1900:2100))
  ##fill with pmatrix
  for(r in 1:nrow(expmatrix)){
    for(c in 1:(ncol(expmatrix))){

      ## Ages 12-14
      if(r>=12 & r<=14 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r+1,c] <- pmatrix[1,1]
      }
      ##if individual is age18-24 and adult2015-16 (https://tobaccocontrol.bmj.com/content/tobaccocontrol/early/2023/03/28/tc-2022-057905/F3.large.jpg?width=800&height=600&carousel=1)
      else if(r>=12 & r<=14 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r+1,c] <- pmatrix[1,2]
      }
      ##if individual is age18-24 and adult2017-19 (from 1900-2019 for model)
      else if(r>=12 & r<=14 & c >= (2017-1899) & c <= (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[1,3]
      }
      ##if individual is age18-24 and adult2019-21 (from 2019-2100 for model)
      else if(r>=12 & r<=14 & c > (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[1,4]
      }
      
      ## Ages 15-17
      if(r>=15 & r<=17 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r+1,c] <- pmatrix[2,1]
      }
      ##if individual is age18-24 and adult2015-16 (https://tobaccocontrol.bmj.com/content/tobaccocontrol/early/2023/03/28/tc-2022-057905/F3.large.jpg?width=800&height=600&carousel=1)
      else if(r>=15 & r<=17 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r+1,c] <- pmatrix[2,2]
      }
      ##if individual is age18-24 and adult2017-19 (from 1900-2019 for model)
      else if(r>=15 & r<=17 & c >= (2017-1899) & c <= (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[2,3]
      }
      ##if individual is age18-24 and adult2019-21 (from 2019-2100 for model)
      else if(r>=15 & r<=17 & c > (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[2,4]
      }
      
      ## Ages 18-24
      else if(r>=18 & r<=24 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r+1,c] <- pmatrix[3,1]
      }
      ##if individual is age18-24 and adult2015-16 (https://tobaccocontrol.bmj.com/content/tobaccocontrol/early/2023/03/28/tc-2022-057905/F3.large.jpg?width=800&height=600&carousel=1)
      else if(r>=18 & r<=24 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r+1,c] <- pmatrix[3,2]
      }
      ##if individual is age18-24 and adult2017-19 (from 1900-2019 for model)
      else if(r>=18 & r<=24 & c >= (2017-1899) & c <= (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[3,3]
      }
      ##if individual is age18-24 and adult2019-21 (from 2019-2100 for model)
      else if(r>=18 & r<=24 & c > (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[3,4]
      }
      
      ## Ages 25-34
      ##if individual is age25-34 and adult2013-14
      else if(r>24 & r<=34 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r+1,c] <- pmatrix[4,1]
      }
      ##if individual is age25-34 and adult2015-16
      else if(r>24 & r<=34 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r+1,c] <- pmatrix[4,2]
      }
      ##if individual is age25-34 and adult2017-19
      else if(r>24 & r<=34 & c >= (2017-1899) & c <= (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[4,3]
      }
      ##if individual is age25-34 and adult2019-21
      else if(r>24 & r<=34 & c > (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[4,4]
      }
      ## Ages 35-54
      ##if individual is age35-54 and adult2013-14
      else if(r>34 & r<=54 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r+1,c] <- pmatrix[5,1]
      }
      ##if individual is age35-54 and adult2015-16
      else if(r>34 & r<=54 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r+1,c] <- pmatrix[5,2]
      }
      ## Ages 35-90
      ##if individual is age35-90 and adult2017-19
      else if(r>34 & r<=90 & c >= (2017-1899) & c <= (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[5,3]
      }
      ##if individual is age35-90 and adult2019-21
      else if(r>34 & r<=90 & c > (2019-1899)){
        expmatrix[r+1,c] <- pmatrix[5,4]
      }
      ## Ages 55-90
      ##if individual is 55.90 and adult2013-14
      else if(r>55 & r<=90 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r+1,c] <- pmatrix[6,1]
      }
      ##if individual is 55.90 and adult2015-16
      else if(r>55 & r<=90 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r+1,c] <- pmatrix[6,2]
      }
    }
  }
  return(expmatrix)
}

##expand all the probability matrices
vars <- c("p.NO.NE", "p.CO.CE", "p.FO.FE",
          "p.NE.NQ", "p.CE.CQ", "p.FE.FQ",
          "p.NQ.NE", "p.CQ.CE", "p.FQ.FE",
          "p.CO.FE")
          # ecig effects on smoking initiation

# Loop through each transition and expand its matrix
for (v in vars) {
  matrix_var <- get(v)   # Use get to retrieve the matrix
  expanded_matrix <- expandmatrix(matrix_var)   # Expand the matrix using expandmatrix function
  assign(v, expanded_matrix)  # Assign the expanded matrix back to the variable
}


# save e-cig transition matrices ------------------------------------------------------

save(p.NO.NE, p.CO.CE, p.FO.FE, 
     p.NE.NQ, p.CE.CQ, p.FE.FQ,
     p.NQ.NE, p.CQ.CE, p.FQ.FE, p.CO.FE,
     file=paste0("data/ecig_precomputed_inputs_", whichgender,".RData"))


# transitions that we did not use from Brouwer
# "p.NO.CO", "p.NE.CE", "p.NQ.CQ", 
# "p.CO.FO", "p.CQ.FQ", "p.CE.FE", 

# # ecig effects on smoking initiation
# p.NO.CO <- matrix(c(0.017, 0.014, 0.006, 0.004, #never established use --> cig only
#                     0.012, 0.008, 0.005, 0.002,
#                     0.01, 0.005, 0.002, 0.001,
#                     0.009, 0.003, 0.002, 0.001),
#                   nrow = 4, ncol = 4, byrow = TRUE,
#                   dimnames = list(c("18.24", "25.34", "35.54", "55.90"),
#                                   c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))
# p.NE.CE <- matrix(c(0.122, 0.112, 0.06, 0.045,
#                     0.131, 0.135, 0.087, 0.062,
#                     0.155, 0.159, 0.049, 0.038,
#                     0.105, 0.084, 0.049, 0.038),
#                   nrow = 4, ncol = 4, byrow = TRUE,
#                   dimnames = list(c("18.24", "25.34", "35.54", "55.90"),
#                                   c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))
# rr.NE.CE <- p.NE.CE / p.NO.CO

# # non-current --> cig-only
# p.NQ.CQ <- matrix(c(0.224, 0.194, 0.083, 0.06,
#                     0.132, 0.136, 0.074, 0.04,
#                     0.053, 0.049, 0.016, 0.017,
#                     0.021, 0.02, 0.016, 0.017),
#                   nrow = 4, ncol = 4, byrow = TRUE,
#                   dimnames = list(c("18.24", "25.34", "35.54", "55.90"),
#                                   c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))



## ecig effects on smoking cessation
# cig-only-->non-current
# p.CO.FO <- p.CQ.FQ <- matrix(c(0.013, 0.137, 0.1, 0.104,
#                                0.108, 0.116, 0.078, 0.09,
#                                0.073, 0.073, 0.063, 0.063,
#                                0.088, 0.083, 0.063, 0.063),
#                              nrow = 4, ncol = 4, byrow = TRUE,
#                              dimnames = list(c("18.24", "25.34", "35.54", "55.90"),
#                                              c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))
# # dual-->ENDS-only
# p.CE.FE <- matrix(c(0.13, 0.133, 0.203, 0.28,
#                     0.091, 0.096, 0.118, 0.192,
#                     0.085, 0.098, 0.039, 0.168,
#                     0.077, 0.063, 0.039, 0.168),
#                   nrow = 4, ncol = 4, byrow = TRUE,
#                   dimnames = list(c("18.24", "25.34", "35.54", "55.90"),
#                                   c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

# rr.CE.FE <- p.CE.FE / p.CO.FO

# e-cig relapse is the same as e-cig initiation because non-use could be never or former