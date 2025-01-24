#table formating
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

df<-PrevalenceDisp
round_fixed_decimal <- function(x, decimals = 2) {
  sprintf(paste0("%.", decimals, "f"), x)
}

percent_format <- function(x, digits = 2) {
  paste0(formatC(x * 100, format = "f", digits = digits), "%")
}

# Apply rounding and formatting
df_formatted <- df %>%
  mutate(across(c(`Depressed Smoking Prevalence`,
                  `Not Depressed Smoking Prevalence`,
                  `Total pop Smoking Prevalence`,
                  `Absolute Difference (prev D-ND)`
                  ), percent_format)) %>%
  mutate(across(c(`Relative Difference (abs diff/prev ND)`,
                  `Prevalence Ratio (prev D/ND)`), round_fixed_decimal))

# Pivoting the table to have intervention, optimistic, and pessimistic next to each other

df_final <- df_formatted %>%
  filter(scenario %in% c("init_0.5_cess_2.10", "init_0.85_cess_4.96", "init_0.1_cess_0.69")) %>%
  pivot_wider(names_from = scenario, values_from = c(`Depressed Smoking Prevalence`,
                                                     `Not Depressed Smoking Prevalence`,
                                                     `Total pop Smoking Prevalence`,
                                                     `Absolute Difference (prev D-ND)`, 
                                                     `Relative Difference (abs diff/prev ND)`,
                                                     `Prevalence Ratio (prev D/ND)`)) %>%
  mutate(across(starts_with("Depressed Smoking Prevalence"):"Prevalence Ratio (prev D/ND)_init_0.5_cess_2.10",
                ~ paste(.x, " (", get(str_replace(cur_column(), "init_0.5_cess_2.10", "init_0.85_cess_4.96")),
                        ", ", get(str_replace(cur_column(), "init_0.5_cess_2.10", "init_0.1_cess_0.69")), ")", sep = ""),
                .names = "{str_replace(.col, '_init_0.5_cess_2.10', '')}"))
df_final$scenario="intervention"
# Select only needed columns
df_final2 <- df_final %>%
  select(year, scenario, "Depressed Smoking Prevalence", 
         "Not Depressed Smoking Prevalence",
         "Total pop Smoking Prevalence",
         "Absolute Difference (prev D-ND)",
         "Relative Difference (abs diff/prev ND)",
         "Prevalence Ratio (prev D/ND)")



df_base <- df_formatted %>%
  filter(scenario %in% "baseline") %>%
  select(year,scenario, "Depressed Smoking Prevalence", 
          "Not Depressed Smoking Prevalence",
          "Total pop Smoking Prevalence",
          "Absolute Difference (prev D-ND)",
          "Relative Difference (abs diff/prev ND)",
          "Prevalence Ratio (prev D/ND)")


df_final3=rbind(df_base,df_final2)

measure_order <- c("Depressed Smoking Prevalence", 
                   "Not Depressed Smoking Prevalence",
                   "Total pop Smoking Prevalence",
                   "Absolute Difference (prev D-ND)","Prevalence Ratio (prev D/ND)",
                   "Relative Difference (abs diff/prev ND)")

df_final4 <- df_final3 %>%
  pivot_longer(cols = c("Depressed Smoking Prevalence", 
                        "Not Depressed Smoking Prevalence",
                        "Total pop Smoking Prevalence",
                        "Absolute Difference (prev D-ND)",
                        "Relative Difference (abs diff/prev ND)","Prevalence Ratio (prev D/ND)"),
               names_to = "measure",
               values_to = "value") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate( measure = factor(measure, levels = measure_order)) %>%
  arrange(measure, year)

#different format to try out:

reshaped_df <- df_final4 %>%
  pivot_longer(cols = c(baseline, intervention), names_to = "scenario", values_to = "value") %>%
   pivot_wider(names_from = c(measure, scenario), values_from = value)

df_long <- df_final4 %>%
  pivot_longer(cols = c(baseline, intervention), names_to = "scenario", values_to = "value") %>%
  mutate(
    value_only = str_extract(value, "^[^\\(]+"), # Extract the value
    ci = str_extract(value, "\\(.*\\)") # Extract the confidence interval
  ) %>%
  select(year, measure, scenario, value_only, ci) 

df_clean <- df_long %>%
  pivot_longer(cols = c(value_only, ci), names_to = "type", values_to = "result") %>%
  mutate(result = replace_na(result, "0"))%>%
  pivot_wider(names_from = c(measure, scenario), values_from = result)

library(openxlsx)
write.xlsx(df_clean, file = paste0("output/Prevlanec_Disparities_Table_allgender_Jan20.xlsx"))


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

Health_new$cSAD<-Health_new$cSAD/1000000
Health_new$cSAD_averted<-Health_new$cSAD_averted/1000000
Health_new$cYLL<-Health_new$cYLL/1000000
Health_new$cYLL_averted_LYG<-Health_new$cYLL_averted_LYG/1000000

Health_new_a<- Health_new %>%
  mutate(across(c("percentage_change_yll","percentage_change_sad"
  ), percent_format)) %>%
  mutate(across(c(`cSAD`,
                  `cSAD_averted`,`cYLL`,
                  `cYLL_averted_LYG`), round_fixed_decimal))


Health_new1 <- Health_new_a %>%
  filter(scenario %in% c("init_0.5_cess_2.10", "init_0.85_cess_4.96", "init_0.1_cess_0.69")) %>%
  group_by(population) %>%
  pivot_wider(names_from = scenario, values_from = c("cSAD","cSAD_averted","cYLL","cYLL_averted_LYG","percentage_change_yll","percentage_change_sad")) %>%
  mutate(across(starts_with("cSAD"):"percentage_change_sad_init_0.5_cess_2.10",
                ~ paste(.x, " (", get(str_replace(cur_column(), "init_0.5_cess_2.10", "init_0.85_cess_4.96")),
                        ", ", get(str_replace(cur_column(), "init_0.5_cess_2.10", "init_0.1_cess_0.69")), ")", sep = ""),
                .names = "{str_replace(.col, '_init_0.5_cess_2.10', '')}"))%>%
  select(c(population,"cSAD","cSAD_averted","cYLL","cYLL_averted_LYG","percentage_change_yll","percentage_change_sad"))


Health_new1$scenario="intervention"
# Select only needed columns
df_H_1 <- Health_new1 %>%
  select(population, scenario, "cSAD","cSAD_averted","cYLL","cYLL_averted_LYG","percentage_change_yll","percentage_change_sad")



df_H_base <- Health_new_a %>%
  filter(scenario %in% "baseline") %>%
  select(population, scenario, "cSAD","cSAD_averted","cYLL","cYLL_averted_LYG","percentage_change_yll","percentage_change_sad")

df_finalH4<- rbind(df_H_base,df_H_1)


df <- df_finalH4 %>%
  # Create a unique identifier for each combination of population and scenario
  mutate(pop_scenario = paste(scenario,population, sep = "_")) %>%
  arrange(population, scenario) %>%
  # Select the relevant columns
  select(pop_scenario, cSAD, cSAD_averted, cYLL, cYLL_averted_LYG, percentage_change_yll, percentage_change_sad) %>%
  # Pivot the data longer
  pivot_longer(cols = -pop_scenario, names_to = "variable", values_to = "value") %>%
  # Pivot it wider to get the desired shape
  pivot_wider(names_from = pop_scenario, values_from = value)


df_wide <- df %>%
  # Pivot the data longer to allow for manipulation of each unique value
  pivot_longer(-variable, names_to = "pop_scenario", values_to = "value_ci") %>%
  
  # Use separate to split the value and confidence interval
  separate(value_ci, into = c("value", "ci"), sep = " \\(", extra = "merge", fill = "right") %>%
  
  # Restore parentheses around the confidence interval
  mutate(ci = if_else(!is.na(ci), paste0("(", ci), ci)) %>%
  
  # Create a new type to differentiate value and ci
  pivot_longer(cols = c("value", "ci"), names_to = "type", values_drop_na = TRUE) %>%
  
  # Spread values to wide format again
  pivot_wider(names_from = pop_scenario, values_from = value) %>%
  
  # Reorder rows to place CIs below their values
  mutate(type = factor(type, levels = c("value", "ci"))) %>%
  arrange(variable, type)

df_wide <- df_wide %>%
  mutate(across(everything(), ~replace_na(., "0")))


library(openxlsx)
write.xlsx(df_wide, file = paste0("output/Outcomes_SADYLL_Table_allgender_jan20.xlsx"))


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
ICERALL$Prod_US=ICERALL$icer_prodLY * ICERALL$US_LYG_cum

# ICERALL$icer_medQALY=ICERALL$inc_med_cost / ICERALL$inc_effectQALY
# ICERALL$icer_socQALY =ICERALL$inc_soc_cost / ICERALL$inc_effectQALY
# ICERALL$icer_prodQALY =ICERALL$inc_prod / ICERALL$inc_effectQALY


ICERALL1<- ICERALL[c("scenario","population","icer_medQALY","icer_socQALY","icer_prodQALY","icer_medLY","icer_socLY","icer_prodLY","MED_cost_US","SOC_cost_US","Prod_US")]
# names(ICERALL1) <-c("scenario","population","Cummulative Years of Life Lost (2023-2100)","Average Years of Life Lost (2023-2100)","Cummulative Smoking Attributable Deaths (2023-2100)","Average Smoking Attributable Deaths","ICER Medical Costs per QALY",
#                     "ICER Societal Cost per QALY","ICER Medical Cost per LY","ICER Societal Cost per LY","US Life Years Gained on Average","US Cummulative LYG (2023-2100)","Cummulative US Medical Costs (2023-2100)","Cummulative US Societal Costs (2023-2100)" ,
#                     "ICER productivity/QALY","SADs averted")

# ICERALL1<- ICERALL[c("scenario","population","cum_YLL","avg_YLL","cum_SAD","avg_SAD","icer_medQALY","icer_socQALY","icer_medLY","icer_socLY","US_LYG_avg","US_LYG_cum","MED_cost_US","SOC_cost_US","icer_prodQALY","US_SAD_avert")]
# names(ICERALL1) <-c("scenario","population","Cummulative Years of Life Lost (2023-2100)","Average Years of Life Lost (2023-2100)","Cummulative Smoking Attributable Deaths (2023-2100)","Average Smoking Attributable Deaths","ICER Medical Costs per QALY",
#                     "ICER Societal Cost per QALY","ICER Medical Cost per LY","ICER Societal Cost per LY","US Life Years Gained on Average","US Cummulative LYG (2023-2100)","Cummulative US Medical Costs (2023-2100)","Cummulative US Societal Costs (2023-2100)" , 
#                     "ICER productivity/QALY","SADs averted")

ICERALL1$MED_cost_US=ICERALL1$MED_cost_US/1000000000
ICERALL1$SOC_cost_US=ICERALL1$SOC_cost_US/1000000000
ICERALL1$Prod_US=ICERALL1$Prod_US/1000000000


ICERALL1 <- ICERALL1 %>%
  mutate(across(c("icer_medQALY","icer_socQALY","icer_prodQALY","icer_medLY","icer_socLY","icer_prodLY"
  ), ~ round(.x, -2))) %>%
  mutate(across(c("MED_cost_US","SOC_cost_US","Prod_US"), round_fixed_decimal))

ICERALL1a <- ICERALL1 %>%
  filter(scenario %in% c("init_0.5_cess_2.10", "init_0.85_cess_4.96", "init_0.1_cess_0.69")) %>%
  group_by(population) %>%
  pivot_wider(names_from = scenario, values_from = c("icer_medQALY","icer_socQALY","icer_prodQALY","icer_medLY","icer_socLY","icer_prodLY","MED_cost_US","SOC_cost_US","Prod_US")) %>%
  mutate(across(starts_with("icer_medQALY"):"Prod_US_init_0.5_cess_2.10",
                ~ paste(.x, " (", get(str_replace(cur_column(), "init_0.5_cess_2.10", "init_0.85_cess_4.96")),
                        ", ", get(str_replace(cur_column(), "init_0.5_cess_2.10", "init_0.1_cess_0.69")), ")", sep = ""),
                .names = "{str_replace(.col, '_init_0.5_cess_2.10', '')}"))%>%
  select(c(population,"icer_medQALY","icer_socQALY","icer_prodQALY","icer_medLY","icer_socLY","icer_prodLY","MED_cost_US","SOC_cost_US","Prod_US"))


# Reshape data to long format
df_long <- ICERALL1a  %>%
  pivot_longer(cols = -population, 
               names_to = "measure", 
               values_to = "value_with_ci")

# Separate values and confidence intervals

df_separated <- df_long %>%
  mutate(value = str_extract(value_with_ci, "-?\\d+\\.?\\d*"),
         ci = str_extract(value_with_ci, "\\(.*?\\)"))

df_longer <- df_separated %>%
  pivot_longer(cols = c(value, ci), 
               names_to = "type", 
               values_to = "number") %>%
  mutate(type = ifelse(type == "value", "Value", "CI"),
         measure = ifelse(type == "Value", measure, paste0(measure, " (CI)")))

# Pivot the table to have populations as columns
df_final <- df_longer %>%
  select(measure, population, number) %>%
  pivot_wider(names_from = population, values_from = number) %>%
  arrange(match(measure, unique(df_longer$measure)))%>%
  select(measure, derpessed, `not derpessed`, total)




library(openxlsx)
write.xlsx(df_final, file = paste0("output/COSTS_Table_allgender_jan20.xlsx"))


