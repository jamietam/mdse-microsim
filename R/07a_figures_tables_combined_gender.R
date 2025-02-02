#rm(list = ls()) 

mainDir = "/Users/bradleydirks/University of Michigan Dropbox/Sarah Skolnick/GitHub/mdse-microsim/"
setwd(mainDir)

#choose the files you want to use for both genders here.
femalefile="rnc_females_1000_01.31.25_09.59PM.Rda"
malefile= "rnc_males_1000_01.31.25_08.38PM.Rda"

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
dfF=reformat_model_outputs(l.results)
dfF_D=reformat_model_outputs(l.results_D)
dfF_notD=reformat_model_outputs(l.results_notD)
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
dfM=reformat_model_outputs(l.results)
dfM_D=reformat_model_outputs(l.results_D)
dfM_notD=reformat_model_outputs(l.results_notD)


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
outcome_columns<-c("T", "D", "ND", "absdiff","prevratio", "reldiff")
sw<- c("init_0.5_cess_2.10", "init_0.1_cess_0.69","init_0.85_cess_4.96")
dataframe<-PrevalenceDisp

dataframe$T <- paste0(sprintf("%.1f", dataframe$T * 100), "%")
dataframe$D <- paste0(sprintf("%.1f", dataframe$D * 100), "%")
dataframe$ND <- paste0(sprintf("%.1f", dataframe$ND * 100), "%")
dataframe$absdiff <- paste0(sprintf("%.1f", dataframe$absdiff * 100), "%")
dataframe$reldiff <- sprintf("%.1f", dataframe$reldiff)  
dataframe$prevratio <- sprintf("%.1f", dataframe$prevratio)  

# Function to process each prevalence column
process_prevalence <- function(column_name) {
  dataframe %>%
    select(year, scenario, one_of(column_name)) %>%
    rename(prevalence = column_name) %>%
    group_by(year) %>%
    summarize(
      scenario1 = list(c(sw[1], paste0(sw[2],",",sw[3]))),
      prevalence1 = list(
        c(paste0(prevalence[scenario ==sw[1]]),paste0( "(",prevalence[scenario == sw[2]],", ", prevalence[scenario == sw[3]],  ")" ) ) ),
      .groups = 'drop' ) %>%
    unnest(cols = c(scenario1, prevalence1)) %>%
    mutate(prevalence_type = column_name) # Add a column to identify the prevalence type
}

# Apply the function to each prevalence column and combine the results
resultsMDSE <- map_dfr(outcome_columns, process_prevalence)
resultsMDSE_f <-resultsMDSE %>%
  pivot_wider(names_from = prevalence_type, values_from = prevalence1)


sw<- c("FDA_est", "FDA_best","FDA_worst")
resultsFDA <- map_dfr(outcome_columns, process_prevalence)
resultsFDA_f <-resultsMDSE %>%
  pivot_wider(names_from = prevalence_type, values_from = prevalence1)

library(openxlsx)
write.xlsx(resultsMDSE_f, file = paste0("output/Prevlance_allgender_MDSE.xlsx"))
write.xlsx(resultsFDA_f, file = paste0("output/Prevlance_allgender_FDA.xlsx"))


#Health outcomes
#Combine model prevs counts, alive and dead for gender.
Health=dfM[[3]][,3:33]+dfF[[3]][,3:33]
Health=cbind(Health,dfM[[3]][,1:2])
Health_D=dfM_D[[3]][,3:33]+dfF_D[[3]][,3:33]
Health_D=cbind(Health_D,dfM_D[[3]][,1:2])
Health_notD=dfM_notD[[3]][,3:33]+dfF_notD[[3]][,3:33]
Health_notD=cbind(Health_notD,dfM_notD[[3]][,1:2])

#combine by depression status
Health_notD$population="ND"
Health_D$population="D"
Health$population="T"

Health_comb<-rbind(Health,Health_D,Health_notD)
Health_comb1<-Health_comb
Health_comb<-Health_comb %>%
  filter(year %in% 2100)%>%
  select(scenario,year,population,cSAD,cSAD_averted,cYLL,cYLL_averted_LYG)


baseline_values <- Health_comb %>%
  filter(scenario == "baseline", year == 2100) %>%
  select(population, cSAD_base = cSAD, cYLL_base = cYLL)

# Calculate averted deaths and percentage changes with join
results_df <- Health_comb %>%
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

Health_new<- Health_comb %>%left_join(results_df, by = c("scenario","population"))
# Calculate averted deaths and percentage changes with join
outcome_columns<-c( "cSAD", "cSAD_averted","cYLL","cYLL_averted_LYG" ,"percentage_change_yll", "percentage_change_sad")
sw<- c("init_0.5_cess_2.10", "init_0.1_cess_0.69","init_0.85_cess_4.96")
dataframe<-Health_new

dataframe$cSAD <- paste0(sprintf("%.1f", dataframe$cSAD/1000000))
dataframe$cSAD_averted <- paste0(sprintf("%.1f", dataframe$cSAD_averted/1000000))
dataframe$cYLL <- paste0(sprintf("%.1f", dataframe$cYLL/1000000))
dataframe$cYLL_averted_LYG <- paste0(sprintf("%.1f", dataframe$cYLL_averted_LYG/1000000))
dataframe$percentage_change_yll <- paste0(sprintf("%.1f", dataframe$percentage_change_yll * 100), "%")
dataframe$percentage_change_sad <- paste0(sprintf("%.1f", dataframe$percentage_change_sad * 100), "%")


# Function to process each prevalence column
process_prevalence_H <- function(column_name) {
  dataframe %>%
    select(population, scenario, one_of(column_name)) %>%
    rename(prevalence = column_name) %>%
    group_by(population) %>%
    summarize(
      scenario1 = list(c(sw[1], paste0(sw[2],",",sw[3]))),
      prevalence1 = list(
        c(paste0(prevalence[scenario ==sw[1]]),paste0( "(",prevalence[scenario == sw[2]],", ", prevalence[scenario == sw[3]],  ")" ) ) ),
      .groups = 'drop' ) %>%
    unnest(cols = c(scenario1, prevalence1)) %>%
    mutate(prevalence_type = column_name) # Add a column to identify the prevalence type
}


resultsMDSE_H <- map_dfr(outcome_columns, process_prevalence_H)
resultsMDSE_fH <-resultsMDSE_H %>%
  pivot_wider(names_from = population, values_from = prevalence1)


sw<- c("FDA_est", "FDA_best","FDA_worst")
resultsFDA_H <- map_dfr(outcome_columns, process_prevalence_H)
resultsFDA_fH <-resultsFDA_H %>%
  pivot_wider(names_from = population, values_from = prevalence1)


library(openxlsx)
write.xlsx(resultsMDSE_fH , file = paste0("output/HealthOutcomes_allgender_MDSE.xlsx"))
write.xlsx(resultsFDA_fH , file = paste0("output/HealthOutcomes_allgender_FDA.xlsx"))


#Cost Effectiveness


#Tables: I think i can just add these but i will have to recalculate ICER ratios and societal costs. 
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


#need to recalculate icers with combined numberators over combined demominators
ICERALL$icer_medQALY <- round(ICERALL$inc_med_cost / ICERALL$inc_effectQALY,0)
ICERALL$icer_socQALY <- round(ICERALL$inc_soc_cost / ICERALL$inc_effectQALY,0)
ICERALL$icer_medLY <- round(ICERALL$inc_med_cost / ICERALL$inc_effectLY,0)
ICERALL$icer_socLY <- round(ICERALL$inc_soc_cost / ICERALL$inc_effectLY,0)
ICERALL$icer_prodLY <- round(ICERALL$inc_prod / ICERALL$inc_effectLY,0)
ICERALL$icer_prodQALY <- round(ICERALL$inc_prod / ICERALL$inc_effectQALY,0)

#need to recalculated costs for society
ICERALL$SOC_cost_US=round(ICERALL$icer_socLY * ICERALL$US_LYG_cum/1000000000000,1) #LYG from total/cummulative YLL across the years 
ICERALL$MED_cost_US=round(ICERALL$icer_medLY * ICERALL$US_LYG_cum/1000000000000,1)
ICERALL$Prod_US=round(ICERALL$icer_prodLY * ICERALL$US_LYG_cum/1000000000000,1) #LYG from total/cummulative YLL across the years 



ICERALL<- ICERALL[c("scenario","population","icer_medQALY","icer_socQALY","icer_prodQALY","icer_medLY","icer_socLY","icer_prodLY","MED_cost_US","SOC_cost_US","Prod_US")]

outcome_columns<-c("icer_medQALY","icer_socQALY","icer_prodQALY","icer_medLY","icer_socLY","icer_prodLY","MED_cost_US","SOC_cost_US","Prod_US")
dataframe<-ICERALL

sw<- c("init_0.5_cess_2.10", "init_0.1_cess_0.69","init_0.85_cess_4.96")
resultsMDSE_C <- map_dfr(outcome_columns, process_prevalence_H)
resultsMDSE_fC <-resultsMDSE_C %>%
  pivot_wider(names_from = population, values_from = prevalence1)


sw<- c("FDA_est", "FDA_best","FDA_worst")
resultsFDA_C <- map_dfr(outcome_columns, process_prevalence_H)
resultsFDA_fC <-resultsFDA_C %>%
  pivot_wider(names_from = population, values_from = prevalence1)


library(openxlsx)
write.xlsx(resultsMDSE_fC, file = paste0("output/COSTS_Table_allgender_MDSE.xlsx"))
write.xlsx(resultsFDA_fC, file = paste0("output/COSTS_Table_allgender_FDA.xlsx"))



#Figures


table(modelprevs_comb$age)
data=modelprevs_comb
#prevalence figures: columns of smoking prev, ecig prev, dual use prev and rows by mental health

plot_by_status <- function(data, status_value, population_value) {
df_filtered <- subset(data, status == status_value &  population == population_value & age=="18.99")

df_filtered <- df_filtered %>%
  select(year,status,population, scenario, prev) %>%
  pivot_wider(names_from = scenario, values_from = prev)

plot<-ggplot() +
  # Scenario 1 line
  geom_line(data = df_filtered, aes(x = year, y = init_0.5_cess_2.10), color = "blue", size = 1) +
  # Confidence interval ribbon
  geom_ribbon(data = df_filtered,
              aes(x = year, ymin = init_0.1_cess_0.69, ymax = init_0.85_cess_4.96),
              fill = "lightblue", alpha = 0.6)+
  # Additional customization
  labs(title = paste0(status_value,population_value),
       x = "Year",
       y = "Prevalence") +
  theme_minimal()
return(plot)
}

pdf("prevalence_grid.pdf", width = 9, height = 9) # You can adjust the dimensions as needed

# Set up a 3x3 grid of plots
par(mfrow = c(3, 3))
plot_by_status(modelprevs_comb, "C", "T") 
plot_by_status(modelprevs_comb, "E", "T") 
plot_by_status(modelprevs_comb, "CE", "T") 
plot_by_status(modelprevs_comb, "C", "D") 
plot_by_status(modelprevs_comb, "E", "D") 
plot_by_status(modelprevs_comb, "CE", "D") 
plot_by_status(modelprevs_comb, "C", "ND") 
plot_by_status(modelprevs_comb, "E", "ND") 
plot_by_status(modelprevs_comb, "CE", "ND") 

dev.off()


#prevalence figures: columns of cSAD, cYLL and rows by mental health
plotCSAD_by_status_H <- function(data, population_value) {
  df_filtered <- subset(data,  population == population_value )
  
  df_filtered <- df_filtered %>%
    select(year,cYLL,cSAD,population, scenario) %>%
    pivot_wider(names_from = scenario, values_from = c(cSAD,cYLL))
  
  plot<-ggplot() +
    # Scenario 1 line
    geom_line(data = df_filtered, aes(x = year, y = cSAD_init_0.5_cess_2.10), color = "blue", size = 1) +
    # Confidence interval ribbon
    geom_ribbon(data = df_filtered,
                aes(x = year, ymin = cSAD_init_0.1_cess_0.69, ymax = cSAD_init_0.85_cess_4.96),
                fill = "lightblue", alpha = 0.6)+
    # Additional customization
    labs(title = paste0(population_value),
         x = "Year",
         y = "CSAD") +
    theme_minimal()
  return(plot)
}
plotCYLL_by_status_H <- function(data, population_value) {
  df_filtered <- subset(data,  population == population_value )
  
  df_filtered <- df_filtered %>%
    select(year,cYLL,cSAD,population, scenario) %>%
    pivot_wider(names_from = scenario, values_from = c(cSAD,cYLL))
  
  plot<-ggplot() +
    # Scenario 1 line
    geom_line(data = df_filtered, aes(x = year, y = cYLL_init_0.5_cess_2.10), color = "blue", size = 1) +
    # Confidence interval ribbon
    geom_ribbon(data = df_filtered,
                aes(x = year, ymin = cYLL_init_0.1_cess_0.69, ymax = cYLL_init_0.85_cess_4.96),
                fill = "lightblue", alpha = 0.6)+
    # Additional customization
    labs(title = paste0(population_value),
         x = "Year",
         y = "CYLL") +
    theme_minimal()
  return(plot)
}



pdf("HealthOutcomes_grid.pdf", width = 9, height = 9) # You can adjust the dimensions as needed

# Set up a 3x3 grid of plots
par(mfrow = c(2, 3))
plotCSAD_by_status_H(Health_comb1, "T") 
plotCYLL_by_status_H(Health_comb1, "T") 
plotCSAD_by_status_H(Health_comb1, "D") 
plotCYLL_by_status_H(Health_comb1, "D") 
plotCSAD_by_status_H(Health_comb1, "ND") 
plotCYLL_by_status_H(Health_comb1, "ND") 

dev.off()



generate_plot1 <- function(data2, status_filter, age_filter, title_suffix, y_label, shape_text, population_filter ) {
  ggplot() +
    geom_line(data = subset(data2, status == status_filter & age == age_filter & population ==population_filter),  
              aes(x = year, y = prev, color = scenario)) +
    # scale_y_continuous(name = y_label, limits = y_limits, breaks = y_breaks) +
    # scale_x_continuous(name = "Year", limits = c(2005, calib_endyear), breaks = seq(2005, calib_endyear, 1)) +
    labs(title = paste0(title_suffix)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
}
generate_plot2 <- function(data2, age_filter, title_suffix, y_label, shape_text, population_filter,scenario_filter ) {
  ggplot() +
    geom_line(data = subset(data2, status == (status == "N" | status == "C" | status == "F"| status =="E") & age == age_filter & population ==population_filter & scenario ==scenario_filter),  
              aes(x = year, y = prev, color = status)) +
    # scale_y_continuous(name = y_label, limits = y_limits, breaks = y_breaks) +
    # scale_x_continuous(name = "Year", limits = c(2005, calib_endyear), breaks = seq(2005, calib_endyear, 1)) +
    labs(title = paste0(title_suffix)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
}
# Distribution in total population
N_age <- generate_plot1(modelprevs_comb, "N", 18.99, "Never smoked", "Prevalence (%)", "National Survey on Drug Use and Health", "T")

# Distribution in total population
N_age <- generate_plot1(modelprevs_comb, "N", 18.99, "Never smoked", "Prevalence (%)", "National Survey on Drug Use and Health", "T")
C_age <- generate_plot1(modelprevs_comb, "C", 18.99, "Current smoking", "Prevalence (%)", "National Survey on Drug Use and Health",  "T")
F_age <- generate_plot1(modelprevs_comb, "F", 18.99, "Former smoking", "Prevalence (%)", "National Survey on Drug Use and Health",  "T")
D_age <- generate_plot1(modelprevs_comb, "D", 18.99, "Current MDE", "Prevalence (%)",  "National Survey on Drug Use and Health",  "T")

E_age <- generate_plot1(modelprevs_comb, "E", 18.99, "Current vaping", "Prevalence (%)", "National Survey on Drug Use and Health",  "T")
NE_age <- generate_plot1(modelprevs_comb, "NE", 18.99, "NS, Current vaping", "Prevalence (%)", "National Survey on Drug Use and Health",  "T")
CE_age <- generate_plot1(modelprevs_comb, "CE", 18.99, "CS, Current vaping", "Prevalence (%)", "National Survey on Drug Use and Health",  "T")
FE_age <- generate_plot1(modelprevs_comb, "FE", 18.99, "FS, Current vaping", "Prevalence (%)", "National Survey on Drug Use and Health",  "T")

# Distribution in MDE population
N_D_age <- generate_plot1(modelprevs_comb, "N", 18.99, "Never smoked, MDE Pop", "Prevalence (%)",  "National Survey on Drug Use and Health", "D")
C_D_age <- generate_plot1(modelprevs_comb, "C", 18.99, "Current smoking, MDE Pop", "Prevalence (%)",  "National Survey on Drug Use and Health", "D")
F_D_age <- generate_plot1(modelprevs_comb, "F", 18.99, "Former smoking, MDE Pop", "Prevalence (%)",  "National Survey on Drug Use and Health", "D")
E_D_age <- generate_plot1(modelprevs_comb, "E", 18.99, "Current vaping, MDE Pop", "Prevalence (%)", "National Survey on Drug Use and Health", "D")

NE_D_age <- generate_plot1(modelprevs_comb, "NE", 18.99, "NS, Current vaping - MDE Pop", "Prevalence (%)", "National Survey on Drug Use and Health", "D")
CE_D_age <- generate_plot1(modelprevs_comb, "CE", 18.99, "CS, Current vaping - MDE Pop", "Prevalence (%)", "National Survey on Drug Use and Health", "D")
FE_D_age <- generate_plot1(modelprevs_comb, "FE", 18.99, "FS, Current vaping - MDE Pop", "Prevalence (%)", "National Survey on Drug Use and Health", "D")

# Figure for e-cig transitions
eciginit <- as.data.frame(c(p.NO.NE[18,],p.CO.CE[18,],p.FO.FE[18,]))
eciginit <- cbind(rep(c(1900:2100),3),
                  eciginit,c(rep("p.NO.NE",201),rep("p.CO.CE",201),rep("p.FO.FE",201)))
names(eciginit) <- c('year', 'value','prob')
p.OE_age <- ggplot(data=eciginit) +  geom_line(aes(x=year,y=value,color=prob))+
  scale_y_continuous(name="Probability of e-cig initiation", limits=c(0,0.2)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="E-cig initiation, 2021 vs 2023")
grid_arrange_shared_legend <- function(plots,columns,titletext) {
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(arrangeGrob(grobs= lapply(plots, function(x)
    x + theme(legend.position="none", plot.title = element_text(size = rel(0.8)))),ncol=columns),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight),
    top=textGrob(titletext,just="top", vjust=1,check.overlap=TRUE,gp=gpar(fontsize=9, fontface="bold"))
  )
}


pdf(file = paste0(mainDir,"output/","_diagnostic_policy_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
grid_arrange_shared_legend(list(N_age, C_age, F_age),3,"Smoking distribution among total population")
grid_arrange_shared_legend(list(D_age),1,"MDE")
grid_arrange_shared_legend(list(N_D_age, C_D_age, F_D_age),3,"Smoking distribution among people with depression")
# p.OE_age
grid_arrange_shared_legend(list(E_age,E_D_age),2,"Vaping by age and Depression Status")
grid_arrange_shared_legend(list(NE_age, CE_age, FE_age),3,"Smoking and Vaping Status Age Distribution in the Total Population")
grid_arrange_shared_legend(list(NE_D_age, CE_D_age, FE_D_age),3,"Smoking and Vaping Status Age Distribution in the Depressed Population")
# inputs
# grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking inputs")
# grid_arrange_shared_legend(list(p.HD_age,p.HD_ageC),2,"Incidence by smoking status")
# grid.arrange(p.DR_age,p.RD_age,ncol=2) # depression recover and recurrence
# Xprobs_age # mortality probabilities
dev.off()