#rm(list = ls()) 

mainDir = "/Users/srs249/Documents/GitHub/mds-microsim/"

#choose scenarios of interest:
scenariosofinterest=c("baseline", "init_0.1_cess_0.69" ,"init_0.5_cess_2.10", "init_0.85_cess_4.96")



#choose the files you want to use for both genders here.

load("output/rnc_females_1000_12.14.24_11.48AM.Rda")
rF=allresults
dfF=reformat_model_outputs(l.results[scenariosofinterest])
dfF_D=reformat_model_outputs(l.results_D[scenariosofinterest])
dfF_notD=reformat_model_outputs(l.results_notD[scenariosofinterest])
load("output/rnc_males_1000_12.14.24_03.06PM.Rda")
rM=allresults
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

years_of_interest <- c(2023, 2040, 2060, 2080, 2100)
age_filter <- "18.99" 
statusfilter<-c("C")

filtered_df_D <- modelprevs_D %>%
  filter(year %in% years_of_interest, age == age_filter, status%in% statusfilter) %>%select(age,year,prev,scenario)
filtered_df_D$population="D"

filtered_df_ND <- modelprevs_notD %>%
  filter(year %in% years_of_interest, age == age_filter, status%in% statusfilter)%>%select(age,year,prev,scenario)
filtered_df_ND$population="ND"


prev<-rbind(filtered_df_D,filtered_df_ND )
df_cleaned <- prev[!duplicated(prev), ]
df_wide <-df_cleaned %>%
  pivot_wider(names_from = population, values_from = c(prev))


#Mort risk by depressed not depressed
PrevalenceDisp<-df_wide
PrevalenceDisp$absdiff=PrevalenceDisp$D -PrevalenceDisp$ND
PrevalenceDisp$reldiff=PrevalenceDisp$absdiff/PrevalenceDisp$ND
PrevalenceDisp$prevratio=PrevalenceDisp$D/PrevalenceDisp$ND
names(PrevalenceDisp)<-c("age","year","scenario","Depressed Smoking Prevalence","Not Depressed Smoking Prevalence","Absolute Difference", "Relative Difference (abs diff/prev ND)","Prevalence Ratio")


library(openxlsx)
write.xlsx(PrevalenceDisp, file = paste0("output/Prevlanec_Disparities_Table_allgender.xlsx"))


#Tables: I think i can just add these
ICERST=dfM[[4]][,2:25]+dfF[[4]][,2:25]
ICERST$scenario=dfM[[4]][,1]
ICERSD=dfM_D[[4]][,2:25]+dfF_D[[4]][,2:25]
ICERSD$scenario=dfM[[4]][,1]
ICERSND=dfM_notD[[4]][,2:25]+dfF_notD[[4]][,2:25]
ICERSND$scenario=dfM[[4]][,1]

ICERSND$population="not derpessed"
ICERSD$population="derpessed"
ICERST$population="total"
ICERALL=rbind(ICERST,ICERSD,ICERSND)
ICERALL$SOC_cost_US=ICERALL$icer_socLY * ICERALL$US_LYG_cum #LYG from total/cummulative YLL across the years 
ICERALL$MED_cost_US=ICERALL$icer_medLY * ICERALL$US_LYG_cum


ICERALL$icer_medQALY=ICERALL$inc_med_cost / ICERALL$inc_effectQALY
ICERALL$icer_socQALY =ICERALL$inc_soc_cost / ICERALL$inc_effectQALY
ICERALL$icer_prodQALY =ICERALL$inc_prod / ICERALL$inc_effectQALY


ICERALL1<- ICERALL[c("scenario","population","cum_YLL","avg_YLL","cum_SAD","avg_SAD","icer_medQALY","icer_socQALY","icer_medLY","icer_socLY","US_LYG_avg","US_LYG_cum","MED_cost_US","SOC_cost_US","icer_prodQALY")]
names(ICERALL1) <-c("scenario","population","Cummulative Years of Life Lost (2023-2100)","Average Years of Life Lost (2023-2100)","Cummulative Smoking Attributable Deaths (2023-2100)","Average Smoking Attributable Deaths","ICER Medical Costs per QALY",
                    "ICER Societal Cost per QALY","ICER Medical Cost per LY","ICER Societal Cost per LY","US Life Years Gained on Average","US Cummulative LYG (2023-2100)","Cummulative US Medical Costs (2023-2100)","Cummulative US Societal Costs (2023-2100)" , "ICER productivity/QALY")
library(openxlsx)
write.xlsx(ICERALL1, file = paste0("output/COSTS_Table_allgender.xlsx"))




