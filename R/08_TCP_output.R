rm(list = ls()) 

mainDir = "/Users/srs475/Library/CloudStorage/Dropbox-UniversityofMichigan/Sarah Skolnick/GitHub/mdse-microsim/"
# mainDir = "/Users/jt936/Dropbox/GitHub/mdse-microsim/"
# mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" 
setwd(mainDir)

hpc = 0
n.i <- 10000 # number of people per birth cohort
policyyear <- 2027
v.affected_ages <- c(0:99) # affects all ages
d.c <- d.u <- d.w <- 0.03              # equal discounting of costs and QALYs by 3%
d.year <- 2025 # which year to start discounting from
args <- c("females",n.i, 2100) #no need to change this for now

source(paste0(mainDir,"R/01_environment.R"), echo=FALSE) #
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE)


malefile= "combined_male20000.RData"
femalefile="combined_female20000.RData"

# Load and format files for females
load(paste0(mainDir, "output/",femalefile))
dfF=reformat_model_outputs(l.results)
dfF_D=reformat_model_outputs(l.results_D)
dfF_ND=reformat_model_outputs(l.results_ND)

# Load and format files for males
load(paste0(mainDir, "output/",malefile))  
dfM=reformat_model_outputs(l.results)
dfM_D=reformat_model_outputs(l.results_D)
dfM_ND=reformat_model_outputs(l.results_ND)

library(dplyr)
df_F<-dfF[[1]] %>%
  select("age","year","scenario", "prev","status")%>%
  pivot_wider(
    names_from = status,
    values_from = prev
  )

df_M<-dfM[[1]] %>%
  select("age","year","scenario", "prev","status")%>%
  pivot_wider(
    names_from = status,
    values_from = prev
  )


df_F_D<-dfF_D[[1]] %>%
  select("age","year","scenario", "prev","status")%>%
  pivot_wider(
    names_from = status,
    values_from = prev
  )

df_M_D<-dfM_D[[1]] %>%
  select("age","year","scenario", "prev","status")%>%
  pivot_wider(
    names_from = status,
    values_from = prev
  )

df_F_ND<-dfF_ND[[1]] %>%
  select("age","year","scenario", "prev","status")%>%
  pivot_wider(
    names_from = status,
    values_from = prev
  )

df_M_ND<-dfM_ND[[1]] %>%
  select("age","year","scenario", "prev","status")%>%
  pivot_wider(
    names_from = status,
    values_from = prev
  )

df_combined_gender_depression_tobacco <- bind_rows(
  df_M %>% mutate(gender = "Male", depression = "Total"),
  df_M_D %>% mutate(gender = "Male", depression = "Depressed"),
  df_M_ND %>% mutate(gender = "Male", depression = "Not Depressed"),
  df_F %>% mutate(gender = "Female", depression = "Total"),
  df_F_D %>% mutate(gender = "Female", depression = "Depressed"),
  df_F_ND %>% mutate(gender = "Female", depression = "Not Depressed")
)

df_csv<-df_combined_gender_depression_tobacco %>% select("age","year","scenario","gender","depression", "C","E","CE")%>%
  rename(smoking = C, vaping = E, dual=CE)

write.csv(df_csv, "tobacco_prev.csv", row.names = FALSE)


#working on health outcomes

df_wide <- df_long %>%
  pivot_wider(
    names_from = status,
    values_from = prev
  )

df_merged <- merge(df_wide, dfF[[3]], by = c("year", "scenario"), all = TRUE)
