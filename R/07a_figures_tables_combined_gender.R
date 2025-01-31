#rm(list = ls()) 

mainDir = "/Users/bradleydirks/Documents/GitHub/mdse-microsim/"
setwd(mainDir)

#choose the files you want to use for both genders here.
femalefile="rnc_females_1000_01.30.25_05.33PM.Rda"
malefile= "rnc_males_1000_01.30.25_05.33PM.Rda"

#females
load(paste0("output/",femalefile))
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
#males
load(paste0("output/",malefile))
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
#combine by gender
#Combine model prevs counts, alive and dead for gender.
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

#combine by depression status
modelprevs_notD$population="ND"
modelprevs_D$population="D"
modelprevs$population="T"

modelprevs_comb<-rbind(modelprevs,modelprevs_D,modelprevs_notD)

years_of_interest <- c(2023, 2040, 2060, 2080, 2100)
age_filter <- "18.99" 
statusfilter<-c("C")

filtered_df<- modelprevs_comb %>%
  filter(year %in% years_of_interest, age == age_filter, status%in% statusfilter) %>%select(age,year,prev,scenario,population)

df_wide <-filtered_df %>%
  pivot_wider(names_from = population, values_from = c(prev))

#Mort risk by depressed not depressed
PrevalenceDisp<-df_wide
PrevalenceDisp$absdiff=PrevalenceDisp$D -PrevalenceDisp$ND
PrevalenceDisp$reldiff=PrevalenceDisp$absdiff/PrevalenceDisp$ND
PrevalenceDisp$prevratio=PrevalenceDisp$D/PrevalenceDisp$ND

# List of prevalence columns to process
prevalence_columns <- c("T" ,"D", "ND", "absdiff",  "reldiff", "prevratio" )
sw<- c("init_0.5_cess_2.10", "init_0.1_cess_0.69"," init_0.85_cess_4.96")
# Function to process each prevalence column
process_prevalence <- function(column_name,est,w_est,b_est) {
  PrevalenceDisp %>%
    select(year, scenario, one_of(column_name)) %>%
    rename(prevalence = column_name) %>%
    mutate(prevalence = round(prevalence, 3)) %>%
    group_by(year) %>%
    summarize(
      scenario = list(c(sw[1], paste0(sw[2],",",sw[3]))),
      prevalence = list(
        c(prevalence[scenario ==sw[1]],paste0( "(",prevalence[scenario == sw[2]],", ", prevalence[scenario == sw[3]],  ")" ) ) ),
      .groups = 'drop' ) %>%
    unnest(cols = c(scenario, prevalence)) %>%
    mutate(prevalence_type = column_name) # Add a column to identify the prevalence type
}

# Apply the function to each prevalence column and combine the results
results <- map_dfr(prevalence_columns, process_prevalence)



PrevalenceDisp$prevalence=PrevalenceDisp$T



PrevalenceDisp$prevalence <- round(PrevalenceDisp$prevalence, 3)





# Group by year and format the data
formatted_df <- PrevalenceDisp %>%
  select(year, scenario, prevalence)%>%
  group_by(year) %>%
  summarize(
    scenario1 = list(c("init_0.5_cess_2.10", "init_0.1_cess_0.69, init_0.85_cess_4.96")),
    prevalence1 = list(
      c(
        prevalence[scenario == "init_0.5_cess_2.10"],
        paste0("(", prevalence[scenario == "init_0.1_cess_0.69"], ", ", prevalence[scenario == "init_0.85_cess_4.96"], ")")
      )
    )
  ) %>%
  unnest(cols = c(scenario1, prevalence1))
names(PrevalenceDisp)<-c("age","year","scenario","Depressed Smoking Prevalence","Not Depressed Smoking Prevalence","Absolute Difference", "Relative Difference (abs diff/prev ND)","Prevalence Ratio")


library(openxlsx)
write.xlsx(PrevalenceDisp, file = paste0("output/Prevlanec_Disparities_Table_allgender.xlsx"))


#Tables: I think i can just add these
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
ICERALL$SOC_cost_US=ICERALL$icer_socLY * ICERALL$US_LYG_cum #LYG from total/cummulative YLL across the years 
ICERALL$MED_cost_US=ICERALL$icer_medLY * ICERALL$US_LYG_cum


ICERALL$icer_medQALY=ICERALL$inc_med_cost / ICERALL$inc_effectQALY
ICERALL$icer_socQALY =ICERALL$inc_soc_cost / ICERALL$inc_effectQALY
ICERALL$icer_prodQALY =ICERALL$inc_prod / ICERALL$inc_effectQALY


ICERALL1<- ICERALL[c("scenario","population","cum_YLL","avg_YLL","cum_SAD","avg_SAD","icer_medQALY","icer_socQALY","icer_medLY","icer_socLY","US_LYG_avg","US_LYG_cum","MED_cost_US","SOC_cost_US","icer_prodQALY","US_SAD_avert")]
names(ICERALL1) <-c("scenario","population","Cummulative Years of Life Lost (2023-2100)","Average Years of Life Lost (2023-2100)","Cummulative Smoking Attributable Deaths (2023-2100)","Average Smoking Attributable Deaths","ICER Medical Costs per QALY",
                    "ICER Societal Cost per QALY","ICER Medical Cost per LY","ICER Societal Cost per LY","US Life Years Gained on Average","US Cummulative LYG (2023-2100)","Cummulative US Medical Costs (2023-2100)","Cummulative US Societal Costs (2023-2100)" , 
                    "ICER productivity/QALY","SADs averted")
library(openxlsx)
write.xlsx(ICERALL1, file = paste0("output/COSTS_Table_allgender.xlsx"))




