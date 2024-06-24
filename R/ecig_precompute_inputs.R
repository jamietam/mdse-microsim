##note: I am assuming that for the following transition states, smoking and ecig can be reused even after quit
#but for happy/depressed, when one recovers they don't become depressed again

#incorporate mortality data here as well

##never established use --> never established use
list2env(setNames(replicate(5, matrix(c(.964,.988,.993,.96,.988,.992), nrow = 3, ncol = 2, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21"))), simplify = FALSE), c("p.NOH.NOH", "p.NOD.NOD", "p.NOR.NOR", "p.NOH.NOD", "p.NOD.NOR")), envir = .GlobalEnv)
##never established use --> ENDS only
p.NOH.NVD <- p.NOD.NVD <- p.NOD.NVR <- p.NOR.NVR <- p.NOH.NVH <- matrix(c(.018,.018, .002, .002, 0, 0), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
##never established use --> cig only
p.NOR.COR <- p.NOD.COR <- p.NOD.COD <- p.NOH.COD <- p.NOH.COH <- matrix(c(.006, .004, .005, .002, .002, .001), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
##never established use --> dual
p.NOR.CVR <- p.NOD.CVR <- p.NOD.CVD <- p.NOH.CVD <- p.NOH.CVH <- matrix(c(.001, .001, 0, 0, 0, 0), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))

##ENDS only --> ENDS only
p.NVD.NVR <- p.NVH.NVD <- p.NVR.NVR <- p.NVD.NVD <- p.NVH.NVH <- matrix(c(.73,.737,.756,.677,.827,.821), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.FVD.FVR <- p.FVH.FVD <- p.FVR.FVR <- p.FVD.FVD <- p.FVH.FVH <- p.NVH.NVH
##ENDS only --> cig only
p.NVD.CQR <- p.NVH.CQD <- p.NVR.CQR <- p.NVD.CQD <- p.NVH.CQH <- matrix(c(.017,.015,.02,.017,.011,.011), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.FVD.CQR <- p.FVH.CQD <- p.FVR.CQR <- p.FVD.CQD <- p.FVH.CQH <- p.NVH.CQH
##ENDS only --> dual
p.NVD.CVR <- p.NVH.CVD <- p.NVR.CVR <- p.NVD.CVD <- p.NVH.CVH <- matrix(c(.06,.045,.087,.062,.049,.038), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.FVD.CVR <- p.FVH.CVD <- p.FVR.CVR <- p.FVD.CVD <- p.FVH.CVH <- p.NVH.CVH
##ENDS only --> non-current
p.NVD.NQR <- p.NVH.NQD <- p.NVR.NQR <- p.NVD.NQD <- p.NVH.NQH <- matrix(c(.193,.204,.137,.244,.113,.129), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.FVD.FQR <- p.FVH.FQD <- p.FVR.FQR <- p.FVD.FQD <- p.FVH.FQH <- p.NVH.NQH

##non-current --> non-current
p.NQD.NQR <- p.NQH.NQD <- p.NQR.NQR <- p.NQD.NQD <- p.NQH.NQH <- matrix(c(.789,.841,.904,.928,.979,.979), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.FOD.FOR <- p.FOH.FOD <- p.FOR.FOR <- p.FOD.FOD <- p.FOH.FOH <- p.NQH.NQH
p.FQD.FQR <- p.FQH.FQD <- p.FQR.FQR <- p.FQD.FQD <- p.FQH.FQH <- p.NQH.NQH
##non-current --> cig-only
p.NQD.CQR <- p.NQH.CQD <- p.NQR.CQR <- p.NQD.CQD <- p.NQH.CQH <- matrix(c(.083,.06,.074,.04,.016,.017), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.FOD.COR <- p.FOH.COD <- p.FOR.COR <- p.FOD.COD <- p.FOH.COH <- p.NQH.CQH
p.FQD.CQR <- p.FQH.CQD <- p.FQR.CQR <- p.FQD.CQD <- p.FQH.CQH <- p.NQH.CQH
##non-current --> ENDS only
p.NQD.NVR <- p.NQH.NVD <- p.NQR.NVR <- p.NQD.NVD <- p.NQH.NVH <- matrix(c(.118,.09,.019,.029,.004,.003), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.FOD.FVR <- p.FOH.FVD <- p.FOR.FVR <- p.FOD.FVD <- p.FOH.FVH <- p.NQH.NVH
p.FQD.FVR <- p.FQH.FVD <- p.FQR.FVR <- p.FQD.FVD <- p.FQH.FVH <- p.NQH.NVH
##non-current --> dual
p.NQD.CVR <- p.NQH.CVD <- p.NQR.CVR <- p.NQD.CVD <- p.NQH.CVH <- matrix(c(.011,.01,.003,.003,0,0), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.FOD.CVR <- p.FOH.CVD <- p.FOR.CVR <- p.FOD.CVD <- p.FOH.CVH <- p.NQH.CVH
p.FQD.CVR <- p.FQH.CVD <- p.FQR.CVR <- p.FQD.CVD <- p.FQH.CVH <- p.NQH.CVH

##cig-only--> cig-only
p.COD.COR <- p.COH.COD <- p.COR.COR <- p.COD.COD <- p.COH.COH <- matrix(c(.766,.698,.852,.829,.906,.911), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.CQD.CQR <- p.CQH.CQD <- p.CQR.CQR <- p.CQD.CQD <- p.CQH.CQH <- p.COH.COH
##cig-only-->ENDS-only
p.COD.FVR <- p.COH.FVD <- p.COR.FVR <- p.COD.FVD <- p.COH.FVH <- matrix(c(.032,.045,.017,.012,.006,.003), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.CQD.FVR <- p.CQH.FVD <- p.CQR.FVR <- p.CQD.FVD <- p.CQH.FVH <- p.COH.FVH
##cig-only-->non-current
p.COD.FOR <- p.COH.FOD <- p.COR.FOR <- p.COD.FOD <- p.COH.FOH <- matrix(c(.1,.104,.078,.09,.063,.063), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.CQD.FQR <- p.CQH.FQD <- p.CQR.FQR <- p.CQD.FQD <- p.CQH.FQH <- p.COH.FOH
##cig-only-->dual
p.COD.CVR <- p.COH.CVD <- p.COR.CVR <- p.COD.CVD <- p.COH.CVH <- matrix(c(.102,.153,.052,.068,.025,.023), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
p.CQD.CVR <- p.CQH.CVD <- p.CQR.CVR <- p.CQD.CVD <- p.CQH.CVH <- p.COH.CVH

##dual-->dual
p.CVD.CVR <- p.CVH.CVD <- p.CVR.CVR <- p.CVD.CVD <- p.CVH.CVH <- matrix(c(.606,.486,.63,.536,.657,.482), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
##dual-->non-current
p.CVD.FQR <- p.CVH.FQD <- p.CVR.FQR <- p.CVD.FQD <- p.CVH.FQH <- matrix(c(.038,.054,.022,.047,.013,.026), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
##dual-->ENDS-only
p.CVD.FVR <- p.CVH.FVD <- p.CVR.FVR <- p.CVD.FVD <- p.CVH.FVH <- matrix(c(.203,.28,.118,.192,.039,.168), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))
##dual-->cig-only
p.CVD.CQR <- p.CVH.CQD <- p.CVR.CQR <- p.CVD.CQD <- p.CVH.CQH <- matrix(c(.152,.18,.23,.225,.291,.323), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("age 18-24", "age 25-34", "age 35-90"),c("2017-19","2019-21")))


##expand probability matrix to match age 1-100 and years 2017-2021
expandmatrix <- function(pmatrix){
  ##create new expanded matrix 
  expmatrix <- matrix(0, nrow = 100, ncol = 201, byrow = TRUE, dimnames = list(1:100, 1900:2100))
  ##fill with pmatrix
  for(r in 1:nrow(expmatrix)){
    for(c in 1:ncol(expmatrix)){
      ##if individual is age18-24 and adult2017-19
      if(r>=18 & r<=24 & c >= (2017-1899) & c <= (2021-1899)){
        expmatrix[r,c] <- pmatrix[1,1]
      }
      ##if individual is age18-24 and adult2019-21
      else if(r>=18 & r<=24 & c > 120){
        expmatrix[r,c] <- pmatrix[1,2]
      }
      ##if individual is age25-34 and adult2017-19
      else if(r>24 & r<=34 & c >= 118 & c <= 120){
        expmatrix[r,c] <- pmatrix[2,1]
      }
      ##if individual is age25-34 and adult2019-21
      else if(r>24 & r<=34 & c > 120){
        expmatrix[r,c] <- pmatrix[2,2]
      }
      ##if individual is age35-90 and adult2017-19
      else if(r>34 & r<=90 & c >= 118 & c <= 120){
        expmatrix[r,c] <- pmatrix[3,1]
      }
      ##if individual is age35-90 and adult2019-21
      else if(r>34 & r<=90 & c > 120){
        expmatrix[r,c] <- pmatrix[3,2]
      }
    }
  }
  return(expmatrix)
}

##expand all the probability matrices
vars<- c(
  "p.NOH.NOH", "p.NOD.NOD", "p.NOR.NOR", "p.NOH.NOD", "p.NOD.NOR",
  "p.NOH.NVD", "p.NOD.NVD", "p.NOD.NVR", "p.NOR.NVR", "p.NOH.NVH",
  "p.NOR.COR", "p.NOD.COR", "p.NOD.COD", "p.NOH.COD", "p.NOH.COH",
  "p.NOR.CVR", "p.NOD.CVR", "p.NOD.CVD", "p.NOH.CVD", "p.NOH.CVH",
  "p.NVD.NVR", "p.NVH.NVD", "p.NVR.NVR", "p.NVD.NVD", "p.NVH.NVH",
  "p.FVD.FVR", "p.FVH.FVD", "p.FVR.FVR", "p.FVD.FVD", "p.FVH.FVH",
  "p.NVD.CQR", "p.NVH.CQD", "p.NVR.CQR", "p.NVD.CQD", "p.NVH.CQH",
  "p.FVD.CQR", "p.FVH.CQD", "p.FVR.CQR", "p.FVD.CQD", "p.FVH.CQH",
  "p.NVD.CVR", "p.NVH.CVD", "p.NVR.CVR", "p.NVD.CVD", "p.NVH.CVH",
  "p.FVD.CVR", "p.FVH.CVD", "p.FVR.CVR", "p.FVD.CVD", "p.FVH.CVH",
  "p.NVD.NQR", "p.NVH.NQD", "p.NVR.NQR", "p.NVD.NQD", "p.NVH.NQH",
  "p.FVD.FQR", "p.FVH.FQD", "p.FVR.FQR", "p.FVD.FQD", "p.FVH.FQH",
  "p.NQD.NQR", "p.NQH.NQD", "p.NQR.NQR", "p.NQD.NQD", "p.NQH.NQH",
  "p.FOD.FOR", "p.FOH.FOD", "p.FOR.FOR", "p.FOD.FOD", "p.FOH.FOH",
  "p.FQD.FQR", "p.FQH.FQD", "p.FQR.FQR", "p.FQD.FQD", "p.FQH.FQH",
  "p.NQD.CQR", "p.NQH.CQD", "p.NQR.CQR", "p.NQD.CQD", "p.NQH.CQH",
  "p.FOD.COR", "p.FOH.COD", "p.FOR.COR", "p.FOD.COD", "p.FOH.COH",
  "p.FQD.CQR", "p.FQH.CQD", "p.FQR.CQR", "p.FQD.CQD", "p.FQH.CQH",
  "p.NQD.NVR", "p.NQH.NVD", "p.NQR.NVR", "p.NQD.NVD", "p.NQH.NVH",
  "p.FOD.FVR", "p.FOH.FVD", "p.FOR.FVR", "p.FOD.FVD", "p.FOH.FVH",
  "p.FQD.FVR", "p.FQH.FVD", "p.FQR.FVR", "p.FQD.FVD", "p.FQH.FVH",
  "p.NQD.CVR", "p.NQH.CVD", "p.NQR.CVR", "p.NQD.CVD", "p.NQH.CVH",
  "p.FOD.CVR", "p.FOH.CVD", "p.FOR.CVR", "p.FOD.CVD", "p.FOH.CVH",
  "p.FQD.CVR", "p.FQH.CVD", "p.FQR.CVR", "p.FQD.CVD", "p.FQH.CVH",
  "p.COD.COR", "p.COH.COD", "p.COR.COR", "p.COD.COD", "p.COH.COH",
  "p.CQD.CQR", "p.CQH.CQD", "p.CQR.CQR", "p.CQD.CQD", "p.CQH.CQH",
  "p.COD.FVR", "p.COH.FVD", "p.COR.FVR", "p.COD.FVD", "p.COH.FVH",
  "p.CQD.FVR", "p.CQH.FVD", "p.CQR.FVR", "p.CQD.FVD", "p.CQH.FVH",
  "p.COD.FOR", "p.COH.FOD", "p.COR.FOR", "p.COD.FOD", "p.COH.FOH",
  "p.CQD.FQR", "p.CQH.FQD", "p.CQR.FQR", "p.CQD.FQD", "p.CQH.FQH",
  "p.COD.CVR", "p.COH.CVD", "p.COR.CVR", "p.COD.CVD", "p.COH.CVH",
  "p.CQD.CVR", "p.CQH.CVD", "p.CQR.CVR", "p.CQD.CVD", "p.CQH.CVH",
  "p.CVD.CVR", "p.CVH.CVD", "p.CVR.CVR", "p.CVD.CVD", "p.CVH.CVH",
  "p.CVD.FQR", "p.CVH.FQD", "p.CVR.FQR", "p.CVD.FQD", "p.CVH.FQH",
  "p.CVD.FVR", "p.CVH.FVD", "p.CVR.FVR", "p.CVD.FVD", "p.CVH.FVH",
  "p.CVD.CQR", "p.CVH.CQD", "p.CVR.CQR", "p.CVD.CQD", "p.CVH.CQH"
)
# Loop through each variable and expand its matrix
for (var in vars) {
  # Use get to retrieve the matrix
  matrix_var <- get(var)
  
  # Expand the matrix using expandmatrix function
  expanded_matrix <- expandmatrix(matrix_var)
  
  # Assign the expanded matrix back to the variable
  assign(var, expanded_matrix)
}




# ##generate matrix
# ##vector of health name states
# v.en <- c("NHO","NDO","NRO","NHQ","NDQ","NRQ","FHQ","FDQ","FRQ","FHO","FDO","FRO","NHV","NDV","NRV","FHV","FDV","FRV","CHO","CDO","CRO","CHQ","CDQ","CRQ","CHV","CDV","CRV")
# 
# ##initiate matrix
# e.p_t <- matrix(data = 0, nrow = length(v.en), ncol = length(v.en)) 
# rownames(e.p_t) <-  v.en
# colnames(e.p_t) <- v.en
# 
# ##create list of probability matrices
# prob_matrices <- list(p.NO.NO, p.NO.CO, p.NO.CV, p.NO.NV, p.NV.NV, p.NV.CQ, p.NV.CV, p.NV.NQ, p.FV.CQ, p.FV.FV, p.FV.CV,p.FV.FQ,p.NQ.CQ,p.NQ.CV,p.NQ.NQ,p.NQ.NV,p.FO.CO,p.FO.CV,p.FO.FO,p.FO.FV,p.FQ.CQ,p.FQ.CV,p.FQ.FQ,p.FQ.FV,p.CO.CO,p.CO.CV,p.CO.FO,p.CO.FV,p.CQ.CQ,p.CQ.CV,p.CQ.FQ,p.CQ.FV,p.CV.CQ,p.CV.CV,p.CV.FQ,p.CV.FV)
# 
# ##assign name to each matrice in list
# names(prob_matrices) <- c("p.NO.NO", "p.NO.CO", "p.NO.CV", "p.NO.NV", "p.NV.NV", "p.NV.CQ", "p.NV.CV", "p.NV.NQ", "p.FV.CQ", "p.FV.FV", "p.FV.CV","p.FV.FQ","p.NQ.CQ","p.NQ.CV","p.NQ.NQ","p.NQ.NV","p.FO.CO","p.FO.CV","p.FO.FO","p.FO.FV","p.FQ.CQ","p.FQ.CV","p.FQ.FQ","p.FQ.FV","p.CO.CO","p.CO.CV","p.CO.FO","p.CO.FV","p.CQ.CQ","p.CQ.CV","p.CQ.FQ","p.CQ.FV","p.CV.CQ","p.CV.CV","p.CV.FQ","p.CV.FV")
# 
# 
# 
# ##fill empty matrix with transition probabilities
# fill_pmatrix <- function(age, birth_year){
#   
#   ##take in values as numbers
#   bc <- as.numeric(birth_year)
#   t <- as.numeric(age)
#   
#   int <- 1
#   ##fill in values into new matrix
#   for (matrix_name in names(prob_matrices)){
#     row_name <- substr(matrix_name,3,4)
#     col_name <- substr(matrix_name,6,7)
#     row_index <- match(row_name, v.en)
#     col_index <- match(col_name, v.en)
#     e.p_t[row_index,col_index] <- prob_matrices[[int]][t,bc]
#     int <- int + 1
#   }
#   return(e.p_t)
# }
# 
# ##first number is age (1=age18-24,2=age25-34,3=age35-90), second number is birth cohort(1=2017-19,2=2019-21)
# fill_pmatrix(1,2)
