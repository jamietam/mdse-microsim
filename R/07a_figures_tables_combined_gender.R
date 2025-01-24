#rm(list = ls()) 

mainDir = "/Users/srs249/Documents/GitHub/mds-microsim/"
setwd(mainDir)
#choose scenarios of interest:
scenariosofinterest=c("baseline", "init_0.1_cess_0.69" ,"init_0.5_cess_2.10", "init_0.85_cess_4.96")



#choose the files you want to use for both genders here.

load("output/rnc_females_1000_12.23.24_09.47PM.Rda")
# organize data by population
l.results <- list() # total population
l.results_D <- list() # depressed population
l.results_notD <- list() # not depressed population
for (s in 1:length(scenarios)){
  l.results[[s]] <- allresults[s][[1]][[1]] # combine all scenario results for general US population into a list
  l.results_D[[s]] <- allresults[s][[1]][[2]] # combine all scenario results for depressed (D) population into a list
  l.results_notD[[s]] <- allresults[s][[1]][[3]] # combine all scenario results for NOT depressed (notD) population into a list
}
names(l.results) <- names(l.results_D) <- names(l.results_notD) <- scenarios

dfF=reformat_model_outputs(l.results[scenariosofinterest])
dfF_D=reformat_model_outputs(l.results_D[scenariosofinterest])
dfF_notD=reformat_model_outputs(l.results_notD[scenariosofinterest])



load("output/rnc_males_1000_12.23.24_09.56PM.Rda")
# organize data by population
l.results <- list() # total population
l.results_D <- list() # depressed population
l.results_notD <- list() # not depressed population
for (s in 1:length(scenarios)){
  l.results[[s]] <- allresults[s][[1]][[1]] # combine all scenario results for general US population into a list
  l.results_D[[s]] <- allresults[s][[1]][[2]] # combine all scenario results for depressed (D) population into a list
  l.results_notD[[s]] <- allresults[s][[1]][[3]] # combine all scenario results for NOT depressed (notD) population into a list
}
names(l.results) <- names(l.results_D) <- names(l.results_notD) <- scenarios

dfM=reformat_model_outputs(l.results[scenariosofinterest])
dfM_D=reformat_model_outputs(l.results_D[scenariosofinterest])
dfM_notD=reformat_model_outputs(l.results_notD[scenariosofinterest])




#Obtain table of outcomes for smoking, deaths, and disparities:
modelprevs= as.data.frame(dfF[[1]][,1:6])+as.data.frame(dfM[[1]][,1:6])
modelprevs=cbind(modelprevs,dfM[[1]][,7:8])
modelprevs$prev=modelprevs$counts/modelprevs$alive
modelprevs$year =modelprevs$year/2
modelprevs$age =modelprevs$age/2

modelprevs_D= as.data.frame(dfF_D[[1]][,1:6])+as.data.frame(dfM_D[[1]][,1:6])
modelprevs_D=cbind(modelprevs_D,dfM_D[[1]][,7:8])
modelprevs_D$prev=modelprevs_D$counts/modelprevs_D$alive
modelprevs_D$year =modelprevs_D$year/2
modelprevs_D$age =modelprevs_D$age/2

modelprevs_notD= as.data.frame(dfF_notD[[1]][,1:6])+as.data.frame(dfM_notD[[1]][,1:6])
modelprevs_notD=cbind(modelprevs_notD,dfM_notD[[1]][,7:8])
modelprevs_notD$prev=modelprevs_notD$counts/modelprevs_notD$alive
modelprevs_notD$year =modelprevs_notD$year/2
modelprevs_notD$age =modelprevs_notD$age/2

years_of_interest <- c(2025, 2040, 2060, 2080, 2100)
age_filter <- "18.99" 
statusfilter<-c("C")

filtered_df_D <- modelprevs_D %>%
  filter(year %in% years_of_interest, age == age_filter, status%in% statusfilter) %>%select(age,year,prev,scenario)
filtered_df_D$population="D"

filtered_df_ND <- modelprevs_notD %>%
  filter(year %in% years_of_interest, age == age_filter, status%in% statusfilter)%>%select(age,year,prev,scenario)
filtered_df_ND$population="ND"

filtered_df_T <- modelprevs %>%
  filter(year %in% years_of_interest, age == age_filter, status%in% statusfilter)%>%select(age,year,prev,scenario)
filtered_df_T$population="T"


prev<-rbind(filtered_df_D,filtered_df_ND,filtered_df_T )
df_cleaned <- prev[!duplicated(prev), ]
df_wide <-df_cleaned %>%
  pivot_wider(names_from = population, values_from = c(prev))


#Mort risk by depressed not depressed
PrevalenceDisp<-df_wide
PrevalenceDisp$absdiff=PrevalenceDisp$D -PrevalenceDisp$ND
PrevalenceDisp$reldiff=PrevalenceDisp$absdiff/PrevalenceDisp$ND
PrevalenceDisp$prevratio=PrevalenceDisp$D/PrevalenceDisp$ND
names(PrevalenceDisp)<-c("age","year","scenario","Depressed Smoking Prevalence","Not Depressed Smoking Prevalence","Total pop Smoking Prevalence","Absolute Difference (prev D-ND)", "Relative Difference (abs diff/prev ND)","Prevalence Ratio (prev D/ND)")


library(openxlsx)
write.xlsx(PrevalenceDisp, file = paste0("output/Prevlanec_Disparities_Table_allgender_Jan20.xlsx"))


#Health outcomes tables:
#Tables: I think i can just add these
Health_T=dfM[[3]][,c(1,3:33)]+dfF[[3]][,c(1,3:33)]
Health_T$scenario=dfM[[3]][,2]
Health_D=dfM_D[[3]][,c(1,3:33)]+dfF_D[[3]][,c(1,3:33)]
Health_D$scenario=dfM_D[[3]][,2]
Health_notD=dfM_notD[[3]][,c(1,3:33)]+dfF_notD[[3]][,c(1,3:33)]
Health_notD$scenario=dfM_notD[[3]][,2]

Health_notD$population="not derpessed"
Health_D$population="derpessed"
Health_T$population="total"

Health=rbind(Health_T,Health_D,Health_notD)

#select necessary columns:
Health<- Health[c("scenario","population","year","cSAD","cYLL","cSAD_averted","cYLL_averted_LYG")]
Health$year=Health$year/2

#limit number of years
years_of_interest <- c( 2100)
Health<- Health %>%
  filter(year %in% years_of_interest) 

#relative change in cSAD and cYLL. change from 2025 to 2100/2025
# Health_cl<- Health %>%
#   select(-c(cSAD_averted, cYLL_averted_LYG))
# results_df <- Health_cl %>%
#     filter(year %in% c(2025, 2100)) %>%
#     group_by(scenario, population)%>%
#     pivot_wider(names_from = year, 
#                 values_from = c(cYLL, cSAD), 
#                 names_glue = "{.value}_{year}") %>%
#     mutate(
#       percentage_change_yll = (cYLL_2100 - cYLL_2025) ,
#       percentage_change_sad = (cSAD_2100 - cSAD_2025)
#     ) %>%
#     select(scenario,population, percentage_change_yll, percentage_change_sad)
#here a larger percentage change means more deaths which is not very inuitive, going to look at disparities by deaths averted.
  

baseline_values <- Health %>%
  filter(scenario == "baseline", year == 2100) %>%
  select(population, cSAD_base = cSAD, cYLL_base = cYLL)

# Calculate averted deaths and percentage changes with join
results_df <- Health %>%
  filter(year == 2100) %>%
  left_join(baseline_values, by = "population") %>%  # Join baseline_values by population
  group_by(population, scenario) %>%
  mutate(
    SADdeathsaverted = cSAD_base - cSAD,
    YLLdeathsaverted = cYLL_base - cYLL,
    percentage_change_yll = (cYLL_base - cYLL) / cYLL_base,
    percentage_change_sad = (cSAD_base - cSAD) / cSAD_base
  ) %>%
  ungroup() %>%  # Always a good practice to ungroup if further operations are intended
  select(scenario,population, cYLL_averted_orig=cYLL_averted_LYG,YLLdeathsaverted,percentage_change_yll, cSAD_averted_orig=cSAD_averted,SADdeathsaverted, percentage_change_sad)



Health_new<- Health %>%left_join(results_df, by = c("scenario","population"))

Health_new<- Health_new[c("scenario","population","cSAD","cSAD_averted","cYLL","cYLL_averted_LYG","percentage_change_yll","percentage_change_sad")]

library(openxlsx)
write.xlsx(Health_new, file = paste0("output/Outcomes_SADYLL_Table_allgender_jan20.xlsx"))


#Cost Effectiveness


#Tables: I think i can just add these but i will have to recalculate ICER ratios. 
ICERST=dfM[[4]][,2:26]+dfF[[4]][,2:26]
ICERST$scenario=dfM[[4]][,1]
ICERSD=dfM_D[[4]][,2:26]+dfF_D[[4]][,2:26]
ICERSD$scenario=dfM[[4]][,1]
ICERSND=dfM_notD[[4]][,2:26]+dfF_notD[[4]][,2:26]
ICERSND$scenario=dfM[[4]][,1]

ICERSND$population="not derpessed"
ICERSD$population="derpessed"
ICERST$population="total"
ICERALL=rbind(ICERST,ICERSD,ICERSND)

ICERALL <- ICERALL[, !grepl("icer", names(ICERALL))]

#need to recalculate icers with combined numberators over combined demominators
ICERALL$icer_medQALY <- round(ICERALL$inc_med_cost / ICERALL$inc_effectQALY,0)
ICERALL$icer_socQALY <- round(ICERALL$inc_soc_cost / ICERALL$inc_effectQALY,0)
ICERALL$icer_medLY <- round(ICERALL$inc_med_cost / ICERALL$inc_effectLY,0)
ICERALL$icer_socLY <- round(ICERALL$inc_soc_cost / ICERALL$inc_effectLY,0)
ICERALL$icer_prodLY <- round(ICERALL$inc_prod / ICERALL$inc_effectLY,0)
ICERALL$icer_prodQALY <- round(ICERALL$inc_prod / ICERALL$inc_effectQALY,0)


ICERALL$SOC_cost_US=ICERALL$icer_socLY * ICERALL$US_LYG_cum #LYG from total/cummulative YLL across the years 
ICERALL$MED_cost_US=ICERALL$icer_medLY * ICERALL$US_LYG_cum


# ICERALL$icer_medQALY=ICERALL$inc_med_cost / ICERALL$inc_effectQALY
# ICERALL$icer_socQALY =ICERALL$inc_soc_cost / ICERALL$inc_effectQALY
# ICERALL$icer_prodQALY =ICERALL$inc_prod / ICERALL$inc_effectQALY


ICERALL1<- ICERALL[c("scenario","population","icer_medQALY","icer_socQALY","icer_prodQALY","icer_medLY","icer_socLY","icer_prodLY","MED_cost_US","SOC_cost_US")]
# names(ICERALL1) <-c("scenario","population","Cummulative Years of Life Lost (2023-2100)","Average Years of Life Lost (2023-2100)","Cummulative Smoking Attributable Deaths (2023-2100)","Average Smoking Attributable Deaths","ICER Medical Costs per QALY",
#                     "ICER Societal Cost per QALY","ICER Medical Cost per LY","ICER Societal Cost per LY","US Life Years Gained on Average","US Cummulative LYG (2023-2100)","Cummulative US Medical Costs (2023-2100)","Cummulative US Societal Costs (2023-2100)" ,
#                     "ICER productivity/QALY","SADs averted")

# ICERALL1<- ICERALL[c("scenario","population","cum_YLL","avg_YLL","cum_SAD","avg_SAD","icer_medQALY","icer_socQALY","icer_medLY","icer_socLY","US_LYG_avg","US_LYG_cum","MED_cost_US","SOC_cost_US","icer_prodQALY","US_SAD_avert")]
# names(ICERALL1) <-c("scenario","population","Cummulative Years of Life Lost (2023-2100)","Average Years of Life Lost (2023-2100)","Cummulative Smoking Attributable Deaths (2023-2100)","Average Smoking Attributable Deaths","ICER Medical Costs per QALY",
#                     "ICER Societal Cost per QALY","ICER Medical Cost per LY","ICER Societal Cost per LY","US Life Years Gained on Average","US Cummulative LYG (2023-2100)","Cummulative US Medical Costs (2023-2100)","Cummulative US Societal Costs (2023-2100)" , 
#                     "ICER productivity/QALY","SADs averted")
library(openxlsx)
write.xlsx(ICERALL1, file = paste0("output/COSTS_Table_allgender_jan20.xlsx"))




