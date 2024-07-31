##never established use --> never established use (don't need this)
p.NO.NO <- matrix(c(0.95, 0.918, 0.964, 0.988, 
                    0.968, 0.957, 0.993, 0.96, 
                    0.975, 0.978, 0.988, 0.992, 
                    0.961, 0.977, 0.988, 0.992), 
                  nrow = 4, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##never established use --> ENDS only
p.NO.NE <- matrix(c(0.01, 0.01, 0.018, 0.018, 
                     0.003, 0.002, 0.002, 0.002, 
                     0.001, 0.001, 0, 0, 
                     0.001, 0, 0, 0), 
                   nrow = 4, ncol = 4, byrow = TRUE, 
                   dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                   c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))
##never established use --> cig only
p.NO.CO <- matrix(c(0.017, 0.014, 0.006, 0.004, 
                    0.012, 0.008, 0.005, 0.002, 
                    0.01, 0.005, 0.002, 0.001, 
                    0.009, 0.003, 0.002, 0.001), 
                  nrow = 4, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))
##never established use --> dual
p.NO.CE <- matrix(c(0.002, 0.002, 0.001, 0.001, 
                    0, 0.001, 0, 0, 
                    0, 0, 0, 0, 
                    0, 0, 0, 0), 
                  nrow = 4, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##ENDS only --> ENDS only
p.FE.FE <- p.NE.NE <- matrix(c(0.456, 0.453, 0.73, 0.737, 
                    0.591, 0.597, 0.756, 0.677, 
                    0.584, 0.552, 0.827, 0.821, 
                    0.74, 0.783, 0.827, 0.821), 
                  nrow = 4, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))
##ENDS only --> cig only
p.NE.CQ <- p.FE.CQ <- matrix(c(0.109, 0.106, 0.017, 0.015, 
                               0.078, 0.075, 0.02, 0.017, 
                               0.074, 0.082, 0.011, 0.011, 
                               0.043, 0.032, 0.011, 0.011), 
                             nrow = 4, ncol = 4, byrow = TRUE, 
                             dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                             c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##ENDS only --> dual
p.NE.CE <- p.FE.CE <- matrix(c(0.122, 0.112, 0.06, 0.045, 
                    0.131, 0.135, 0.087, 0.062, 
                    0.155, 0.159, 0.049, 0.038, 
                    0.105, 0.084, 0.049, 0.038), 
                  nrow = 4, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))
##ENDS only --> non-current
p.NE.NQ <- p.FE.FQ <- matrix(c(0.313, 0.329, 0.193, 0.204, 
                               0.199, 0.194, 0.137, 0.244, 
                               0.187, 0.206, 0.113, 0.129, 
                               0.111, 0.101, 0.113, 0.129), 
                             nrow = 4, ncol = 4, byrow = TRUE, 
                             dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                             c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##non-current --> non-current
p.NQ.NQ <- p.FO.FO <- p.FQ.FQ <- matrix(c(0.713, 0.733, 0.789, 0.841, 
                                          0.843, 0.84, 0.904, 0.928, 
                                          0.936, 0.941, 0.979, 0.979, 
                                          0.977, 0.977, 0.979, 0.979), 
                                        nrow = 4, ncol = 4, byrow = TRUE, 
                                        dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                                        c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##non-current --> cig-only
p.NQ.CQ <- p.FO.CO <- p.FQ.CQ <- matrix(c(0.224, 0.194, 0.083, 0.06, 
                                                     0.132, 0.136, 0.074, 0.04, 
                                                     0.053, 0.049, 0.016, 0.017, 
                                                     0.021, 0.02, 0.016, 0.017), 
                                                   nrow = 4, ncol = 4, byrow = TRUE, 
                                                   dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                                                   c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##non-current --> ENDS only
p.NQ.NE <- p.FO.FE <- p.FQ.FE <- matrix(c(0.046, 0.057, 0.118, 0.09, 
                                          0.018, 0.017, 0.019, 0.029, 
                                          0.009, 0.008, 0.004, 0.003, 
                                          0.002, 0.002, 0.004, 0.003), 
                                        nrow = 4, ncol = 4, byrow = TRUE, 
                                        dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                                        c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##non-current --> dual
p.NQ.CE <- p.FO.CE <- p.FQ.CE <- matrix(c(0.018, 0.017, 0.0111, 0.01, 
                                          0.007, 0.007, 0.003, 0.003, 
                                          0.002, 0.002, 0, 0, 
                                          0, 0, 0, 0), 
                                        nrow = 4, ncol = 4, byrow = TRUE, 
                                        dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                                        c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))


##cig-only--> cig-only
p.CO.CO <- p.CQ.CQ <- matrix(c(0.777, 0.776, 0.766, 0.698, 
                               0.821, 0.82, 0.852, 0.829, 
                               0.88, 0.887, 0.906, 0.911, 
                               0.885, 0.896, 0.906, 0.911), 
                             nrow = 4, ncol = 4, byrow = TRUE, 
                             dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                             c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##cig-only-->ENDS-only
p.CO.FE <- p.CQ.FE <- matrix(c(0.018, 0.019, 0.032, 0.045, 
                                          0.015, 0.013, 0.017, 0.012, 
                                          0.008, 0.006, 0.006, 0.003, 
                                          0.007, 0.006, 0.006, 0.003), 
                                        nrow = 4, ncol = 4, byrow = TRUE, 
                                        dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                                        c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##cig-only-->non-current
p.CO.FO <- p.CQ.FQ <- matrix(c(0.013, 0.137, 0.1, 0.104, 
                               0.108, 0.116, 0.078, 0.09, 
                               0.073, 0.073, 0.063, 0.063, 
                               0.088, 0.083, 0.063, 0.063), 
                             nrow = 4, ncol = 4, byrow = TRUE, 
                             dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                             c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##cig-only-->dual
p.CO.CE <- p.CQ.CE <- matrix(c(0.073, 0.069, 0.102, 0.153, 
                               0.055, 0.051, 0.052, 0.068, 
                               0.039, 0.034, 0.025, 0.023, 
                               0.02, 0.015, 0.025, 0.023), 
                             nrow = 4, ncol = 4, byrow = TRUE, 
                             dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                             c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))


##dual-->dual
p.CE.CE <- matrix(c(0.374, 0.342, 0.606, 0.486, 
                    0.404, 0.422, 0.63, 0.536, 
                    0.43, 0.401, 0.657, 0.482, 
                    0.454, 0.475, 0.657, 0.482), 
                  nrow = 4, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##dual-->non-current
p.CE.FQ <- matrix(c(0.083, 0.09, 0.038, 0.054, 
                    0.048, 0.049, 0.022, 0.047, 
                    0.034, 0.039, 0.013, 0.026, 
                    0.03, 0.026, 0.013, 0.026), 
                  nrow = 4, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##dual-->ENDS-only
p.CE.FE <- matrix(c(0.13, 0.133, 0.203, 0.28, 
                    0.091, 0.096, 0.118, 0.192, 
                    0.085, 0.098, 0.039, 0.168, 
                    0.077, 0.063, 0.039, 0.168), 
                  nrow = 4, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))

##dual-->cig-only
p.CE.CQ <- matrix(c(0.413, 0.435, 0.152, 0.18, 
                    0.457, 0.433, 0.23, 0.225, 
                    0.451, 0.462, 0.291, 0.323, 
                    0.44, 0.435, 0.291, 0.323), 
                  nrow = 4, ncol = 4, byrow = TRUE, 
                  dimnames = list(c("age 18-24", "age 25-34", "age 35-54", "age 55-90"),
                                  c("2013-2014", "2015-2016", "2017-2019", "2020-2021")))


##expand probability matrix to match age 1-100 and fills in for years 2013-2100
expandmatrix <- function(pmatrix){
  ##create new expanded matrix 
  expmatrix <- matrix(0, nrow = 100, ncol = 201, byrow = TRUE, dimnames = list(1:100, 1900:2100))
  ##fill with pmatrix
  for(r in 1:nrow(expmatrix)){
    for(c in 1:ncol(expmatrix)){
      ##if individual is age18-24 and adult2013-14 (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8124082/)
      if(r>=18 & r<=24 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r,c] <- pmatrix[1,1]
      }
      ##if individual is age18-24 and adult2015-16 (https://tobaccocontrol.bmj.com/content/tobaccocontrol/early/2023/03/28/tc-2022-057905/F3.large.jpg?width=800&height=600&carousel=1)
      else if(r>=18 & r<=24 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r,c] <- pmatrix[1,2]
      }
      ##if individual is age18-24 and adult2017-19 (from 1900-2019 for model)
      else if(r>=18 & r<=24 & c >= (2017-1899) & c <= (2019-1899)){
        expmatrix[r,c] <- pmatrix[1,3]
      }
      ##if individual is age18-24 and adult2019-21 (from 2019-2100 for model)
      else if(r>=18 & r<=24 & c > (2019-1899)){
        expmatrix[r,c] <- pmatrix[1,4]
      }
      ##if individual is age25-34 and adult2013-14
      else if(r>24 & r<=34 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r,c] <- pmatrix[2,1]
      }
      ##if individual is age25-34 and adult2015-16
      else if(r>24 & r<=34 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r,c] <- pmatrix[2,2]
      }
      ##if individual is age25-34 and adult2017-19
      else if(r>24 & r<=34 & c >= (2017-1899) & c <= (2019-1899)){
        expmatrix[r,c] <- pmatrix[2,3]
      }
      ##if individual is age25-34 and adult2019-21
      else if(r>24 & r<=34 & c > (2019-1899)){
        expmatrix[r,c] <- pmatrix[2,4]
      }
      ##if individual is age35-54 and adult2013-14
      else if(r>34 & r<=54 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r,c] <- pmatrix[3,1]
      }
      ##if individual is age35-54 and adult2015-16
      else if(r>34 & r<=54 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r,c] <- pmatrix[3,2]
      }
      ##if individual is age35-90 and adult2017-19
      else if(r>34 & r<=90 & c >= (2017-1899) & c <= (2019-1899)){
        expmatrix[r,c] <- pmatrix[3,3]
      }
      ##if individual is age35-90 and adult2019-21
      else if(r>34 & r<=90 & c > (2019-1899)){
        expmatrix[r,c] <- pmatrix[3,4]
      }
      ##if individual is age 55-90 and adult2013-14
      else if(r>55 & r<=90 & c >= (2013-1899) & c <= (2014-1899)){
        expmatrix[r,c] <- pmatrix[4,1]
      }
      ##if individual is age 55-90 and adult2015-16
      else if(r>55 & r<=90 & c >= (2015-1899) & c <= (2016-1899)){
        expmatrix[r,c] <- pmatrix[4,2]
      }
    }
  }
  return(expmatrix)
}

##expand all the probability matrices
vars <- c("p.NO.NO", "p.NO.CO", "p.NO.CE", "p.NO.NE", 
          "p.NE.NE", "p.NE.CQ", "p.NE.CE", "p.NE.NQ", 
          "p.FE.CQ", "p.FE.FE", "p.FE.CE", "p.FE.FQ",
          "p.NQ.CQ", "p.NQ.CE", "p.NQ.NQ", "p.NQ.NE", 
          "p.FO.CO", "p.FO.CE", "p.FO.FO", "p.FO.FE",
          "p.FQ.CQ", "p.FQ.CE", "p.FQ.FQ", "p.FQ.FE", 
          "p.CO.CO", "p.CO.CE", "p.CO.FO", "p.CO.FE",
          "p.CQ.CQ", "p.CQ.CE", "p.CQ.FQ", "p.CQ.FE",
          "p.CE.CQ", "p.CE.CE", "p.CE.FQ", "p.CE.FE")
# Loop through each variable and expand its matrix
for (v in vars) {
  # Use get to retrieve the matrix
  matrix_var <- get(v)
  
  # Expand the matrix using expandmatrix function
  expanded_matrix <- expandmatrix(matrix_var)
  
  # Assign the expanded matrix back to the variable
  assign(v, expanded_matrix)
}

#save transition probability matrices
save(p.NO.NO, p.NO.CO, p.NO.CE, p.NO.NE, 
     p.NE.NE, p.NE.CQ, p.NE.CE, p.NE.NQ, 
     p.FE.CQ, p.FE.FE, p.FE.CE, p.FE.FQ,
     p.NQ.CQ, p.NQ.CE, p.NQ.NQ, p.NQ.NE, 
     p.FO.CO, p.FO.CE, p.FO.FO, p.FO.FE,
     p.FQ.CQ, p.FQ.CE, p.FQ.FQ, p.FQ.FE, 
     p.CO.CO, p.CO.CE, p.CO.FO, p.CO.FE,
     p.CQ.CQ, p.CQ.CE, p.CQ.FQ, p.CQ.FE,
     p.CE.CQ, p.CE.CE, p.CE.FQ, p.CE.FE, file=paste0("data/ecig_precomputed_inputs",whichgender,".RData"))

