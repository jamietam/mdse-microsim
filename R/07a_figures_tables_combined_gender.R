# create subdirectory for figures based on today's date
figDir <- format(as.POSIXct(Sys.time()), "%m.%d.%y")
dir.create(file.path(mainDir, "output", figDir), showWarnings = FALSE)

#Obtain table of outcomes for smoking, deaths, and disparities:
#Combine model prevs counts, alive and dead 
#Combine by gender
#Total
df.prevs= as.data.frame(dfF[[1]][,4:6])+as.data.frame(dfM[[1]][,4:6])
df.prevs=cbind(dfF[[1]][,1:2],df.prevs,dfM[[1]][,7:8])
#recalculate prevalence
df.prevs$prev=df.prevs$counts/df.prevs$alive

#Depressed
df.prevs_D= as.data.frame(dfF_D[[1]][,4:6])+as.data.frame(dfM_D[[1]][,4:6])
df.prevs_D=cbind(dfF_D[[1]][,1:2],df.prevs_D,dfM_D[[1]][,7:8])
df.prevs_D$prev=df.prevs_D$counts/df.prevs_D$alive

#Healthy,Not Depressed
df.prevs_ND= as.data.frame(dfF_ND[[1]][,4:6])+as.data.frame(dfM_ND[[1]][,4:6])
df.prevs_ND=cbind(dfF_ND[[1]][,1:2],df.prevs_ND,dfM_ND[[1]][,7:8])
df.prevs_ND$prev=df.prevs_ND$counts/df.prevs_ND$alive

#Combine by depression status
df.prevs_ND$population="ND"
df.prevs_D$population="D"
df.prevs$population="T"

df.prevs_comb<-rbind(df.prevs,df.prevs_D,df.prevs_ND)

years_of_interest <- c(2026, 2040, 2060, 2080, 2100)
age_filter <- "18.99" 
statusfilter<-c("C") #Current Smoker Prevalence

df.prevs<- df.prevs_comb %>%
  filter(year %in% years_of_interest, age == age_filter, status%in% statusfilter) %>%select(age,year,prev,scenario,population)%>%
  pivot_wider(names_from = population, values_from = c(prev))


#Calculate Prevalence Disparities and Difference Measures
df.prevs$absdiff=df.prevs$D -df.prevs$ND
df.prevs$reldiff=df.prevs$absdiff/df.prevs$ND
df.prevs$prevratio=df.prevs$D/df.prevs$ND

#format the different outcomes
df.prevs$T <- paste0(sprintf("%.1f", df.prevs$T * 100), "%")
df.prevs$D <- paste0(sprintf("%.1f", df.prevs$D * 100), "%")
df.prevs$ND <- paste0(sprintf("%.1f", df.prevs$ND * 100), "%")
df.prevs$absdiff <- paste0(sprintf("%.1f", df.prevs$absdiff * 100), "%")
df.prevs$reldiff <- sprintf("%.1f", df.prevs$reldiff)  
df.prevs$prevratio <- sprintf("%.1f", df.prevs$prevratio)  

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
# List of prevalence columns to process
outcome_columns<-c("T", "D", "ND", "absdiff","prevratio", "reldiff")
# Apply the function to each prevalence column and combine the results
dataframe<-df.prevs
sw<- c("baseline","baseline","baseline") #scenarios
df.baseline <- map_dfr(outcome_columns, process_prevalence)
df.baseline <-df.baseline %>%
  pivot_wider(names_from = prevalence_type, values_from = prevalence1,names_prefix = "B_")
#MDSE Scenarios
sw<- c("init_0.5_cess_2.10", "init_0.1_cess_0.69","init_0.85_cess_4.96")
df.MDSE <- map_dfr(outcome_columns, process_prevalence)
df.MDSE <-df.MDSE %>%
  pivot_wider(names_from = prevalence_type, values_from = prevalence1)
df.MDSE<-cbind(df.MDSE,df.baseline)
df.MDSE<-df.MDSE[c("year", "scenario1","B_D", "D","B_ND", "ND", "B_T","T","B_absdiff" , "absdiff" ,"B_prevratio", "prevratio", "B_reldiff", "reldiff")]

#FDA Scenarios
sw<- c("FDA_est", "FDA_best","FDA_worst")
df.FDA <- map_dfr(outcome_columns, process_prevalence)
df.FDA <-df.FDA %>%
  pivot_wider(names_from = prevalence_type, values_from = prevalence1)
df.FDA<-cbind(df.FDA,df.baseline)
df.FDA<-df.FDA[c("year", "scenario1","B_D", "D","B_ND", "ND", "B_T","T","B_absdiff" , "absdiff" ,"B_prevratio", "prevratio", "B_reldiff", "reldiff")]

write.xlsx(df.MDSE, file = paste0(mainDir, "output/",figDir,"/Prevalance_allgender_MDSE.xlsx"))
write.xlsx(df.FDA, file = paste0(mainDir, "output/",figDir,"/Prevalance_allgender_FDA.xlsx"))


#Health outcomes
#Combine model prevs counts, alive and dead for gender.
Health=dfM[[3]][,3:39]+dfF[[3]][,3:39]
Health=cbind(Health,dfM[[3]][,1:2])
Health_D=dfM_D[[3]][,3:39]+dfF_D[[3]][,3:39]
Health_D=cbind(Health_D,dfM_D[[3]][,1:2])
Health_ND=dfM_ND[[3]][,3:39]+dfF_ND[[3]][,3:39]
Health_ND=cbind(Health_ND,dfM_ND[[3]][,1:2])

#combine by depression status
Health_ND$population="ND"
Health_D$population="D"
Health$population="T"

Health_comb<-rbind(Health,Health_D,Health_ND)
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


sw<- c("baseline","baseline","baseline")
resultsbaseline_H <- map_dfr(outcome_columns, process_prevalence_H)
resultsbaseline_fH <-resultsbaseline_H %>%
  pivot_wider(names_from = population, values_from = prevalence1,names_prefix = "B_")

#MDSE Scenarios
sw<- c("init_0.5_cess_2.10", "init_0.1_cess_0.69","init_0.85_cess_4.96")
resultsMDSE_H <- map_dfr(outcome_columns, process_prevalence_H)
resultsMDSE_fH <-resultsMDSE_H %>%
  pivot_wider(names_from = population, values_from = prevalence1)
resultsMDSE_fH1<-cbind(resultsMDSE_fH,resultsbaseline_fH)
resultsMDSE_fH<-resultsMDSE_fH1[c("scenario1","prevalence_type","B_D","D","B_ND","ND","B_T","T")]

#FDA scenarios
sw<- c("FDA_est", "FDA_best","FDA_worst")
resultsFDA_H <- map_dfr(outcome_columns, process_prevalence_H)
resultsFDA_fH <-resultsFDA_H %>%
  pivot_wider(names_from = population, values_from = prevalence1)
resultsFDA_fH1<-cbind(resultsFDA_fH,resultsbaseline_fH)
resultsFDA_fH<-resultsFDA_fH1[c("scenario1","prevalence_type","B_D","D","B_ND","ND","B_T","T")]


write.xlsx(resultsMDSE_fH , file = paste0(mainDir, "output/",figDir,"/HealthOutcomes_allgender_MDSE.xlsx"))
write.xlsx(resultsFDA_fH , file = paste0(mainDir, "output/",figDir,"/HealthOutcomes_allgender_FDA.xlsx"))


#Cost Effectiveness


#Tables: I think i can just add these but i will have to recalculate ICER ratios and societal costs. 
ICERST=dfM[[4]][,2:27]+dfF[[4]][,2:27]
ICERST$scenario=dfM[[4]][,1]
ICERSD=dfM_D[[4]][,2:27]+dfF_D[[4]][,2:27]
ICERSD$scenario=dfM[[4]][,1]
ICERSND=dfM_ND[[4]][,2:27]+dfF_ND[[4]][,2:27]
ICERSND$scenario=dfM[[4]][,1]

ICERSND$population="not depressed"
ICERSD$population="depressed"
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
ICERALL$SOC_cost_US=round(ICERALL$icer_socLY * ICERALL$US_LYG_cum/1000000000,1) #LYG from total/cummulative YLL across the years 
ICERALL$MED_cost_US=round(ICERALL$icer_medLY * ICERALL$US_LYG_cum/1000000000,1)
ICERALL$Prod_US=round(ICERALL$icer_prodLY * ICERALL$US_LYG_cum/1000000000,1) #LYG from total/cummulative YLL across the years 



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


write.xlsx(resultsMDSE_fC, file = paste0(mainDir, "output/",figDir,"/COSTS_Table_allgender_MDSE.xlsx"))
write.xlsx(resultsFDA_fC, file = paste0(mainDir, "output/",figDir,"/COSTS_Table_allgender_FDA.xlsx"))



#Figures Functions#
# grid_arrange_shared_legend <- function(plots,columns,titletext) {
#   g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
#   legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
#   lheight <- sum(legend$height)
#   grid.arrange(arrangeGrob(grobs= lapply(plots, function(x)
#     x + theme(legend.position="none", plot.title = element_text(size = rel(0.8)))),ncol=columns),
#     legend,
#     ncol = 1,
#     heights = unit.c(unit(1, "npc") - lheight, lheight),
#     top=textGrob(titletext,just="top", vjust=1,check.overlap=TRUE,gp=gpar(fontsize=9, fontface="bold"))
#   )
# }


grid_arrange_shared_legend <- function(plots, nrow=NULL, ncol=NULL, titletext) {
  # Calculate total number of plots
  
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  
  grid.arrange(
    arrangeGrob(grobs= lapply(plots, function(x)
      x + theme(legend.position="none", plot.title = element_text(size = rel(0.8)))),
      ncol=ncol, nrow=nrow),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight),
    top=textGrob(titletext, just="top", vjust=1, check.overlap=TRUE, gp=gpar(fontsize=9, fontface="bold"))
  )
}

# prev_by_status(df.prevs_comb, "C", "T",FALSE,"18.99") 
# data=df.prevs_comb
# status_value="C"
# population_value="T"
#   FDA_include_TorF=FALSE
#   age_filter="18.99"
#Prevalence Figures functions: columns of smoking prev, ecig prev, dual use prev and rows by mental health
prev_by_status <- function(data, status_value, population_value,FDA_include_TorF,age_filter) {
  df_filtered <- subset(data, status == status_value &  population == population_value & age==age_filter)
  
  df_filtered <- df_filtered %>%
    select(year,status,population, scenario, prev) %>%
    pivot_wider(names_from = scenario, values_from = prev)
  
  if (population_value=="T"){outcomelabel="Total adult population:"
  dep.calib=status_value
  }else if (population_value=="D"){outcomelabel="Adults with MDE:"
  if (status_value=="C"){dep.calib="C_D"
  }else if (status_value=="E"){dep.calib="E_D"
  }else if (status_value=="F"){dep.calib="F_D"
  }else if (status_value=="N"){dep.calib="N_D"
  }else if (status_value=="CE"){dep.calib="CE_D"
  }else if (status_value=="NE"){dep.calib="NE_D"
  }else if (status_value=="FE"){dep.calib="FE_Dr"}
  }else{outcomelabel="Adults without MDE:"
  dep.calib=status_value}
  
  if (status_value=="C"){outcomelabel2=" Current smoking"
  }else if (status_value=="E"){outcomelabel2=" E-cigarette use"
  }else if (status_value=="F"){outcomelabel2=" Former smoking"
  }else if (status_value=="N"){outcomelabel2=" Never smoking"
  }else if (status_value=="CE"){outcomelabel2=" Dual use"
  }else if (status_value=="NE"){outcomelabel2=" Never smoking, e-cig use"
  }else if (status_value=="FE"){outcomelabel2=" Former smoking, e-cig use"
  }else{outcomelabel2=" help"}
  
  if (FDA_include_TorF==TRUE){
    plot <- ggplot() +
      geom_pointrange(data = subset(df.calib_targets, status == dep.calib& age==as.numeric(age_filter)), 
                 aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI)) +
      # MDSE line with color mapped to aesthetics
      geom_line(data = df_filtered, aes(x = year, y = init_0.5_cess_2.10, color = "MDSE"), size = 1) +
      # Confidence interval ribbon for MDSE
      geom_ribbon(data = df_filtered,
                  aes(x = year, ymin = init_0.1_cess_0.69, ymax = init_0.85_cess_4.96),
                  fill = "lightblue", alpha = 0.6) +
      # FDA line with color mapped to aesthetics
      geom_line(data = df_filtered, aes(x = year, y = FDA_est, color = "FDA"), size = 1) +
      # Confidence interval ribbon for FDA
      geom_ribbon(data = df_filtered,
                  aes(x = year, ymin = FDA_best, ymax = FDA_worst),
                  fill = "lightpink", alpha = 0.6) +
      # Add custom colors to legend
      scale_color_manual(values = c("FDA" = "red", "MDSE" = "blue")) +
      # Additional customization
      labs(title = paste0(outcomelabel, outcomelabel2),
           x = "Year",
           y = "Prevalence",
           color = "Policy Effects") +  # Legend title
      theme_minimal()
  }else{plot<-ggplot() +
    geom_pointrange(data = subset(df.calib_targets, status == dep.calib& age==as.numeric(age_filter)), 
                    aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI)) +
    # Scenario 1 line
    geom_line(data = df_filtered, aes(x = year, y = init_0.5_cess_2.10), color = "blue", size = 1) +
    geom_line(data = df_filtered, aes(x = year, y = baseline), color = "black", size = 1) +
    # Confidence interval ribbon
    geom_ribbon(data = df_filtered,
                aes(x = year, ymin = init_0.1_cess_0.69, ymax = init_0.85_cess_4.96),
                fill = "lightblue", alpha = 0.6)+
    # Additional customization
    labs(title = paste0(outcomelabel,outcomelabel2),
         x = "Year",
         y = "Prevalence") +
    scale_x_continuous(limits = c(2005,2100),breaks = c(2005,seq(2025,2100,25)))+
    scale_y_continuous(limits = c(0,0.5))+
    theme_minimal()
  }
  
  return(plot)
}


# Mort_by_status(Health_comb1, "T", "cSAD",TRUE)
data=Health_comb1
population_value="T"
outcome="cYLL_averted_LYG"
FDA_include_TorF=FALSE
#Health Outcome figures: columns of cSAD, cYLL and rows by mental health

Health_comb1$cSAD_averted<-Health_comb1$cSAD_averted/1000000
Health_comb1$cYLL_averted_LYG<-Health_comb1$cYLL_averted_LYG/1000000
Mort_by_status <- function(data, population_value, outcome,FDA_include_TorF) { #outcome = either cSAD_averted or cYLL_averted_LYG, sw is scenarios list 
  df_filtered <- subset(data,  population == population_value )
  
  df_filtered <- df_filtered %>%
    select(year,cYLL_averted_LYG,cSAD_averted,population, scenario) %>%
    pivot_wider(names_from = scenario, values_from = c(cYLL_averted_LYG,cSAD_averted))
  if (population_value=="T"){outcomelabel="Total adult population"
  }else if (population_value=="D"){outcomelabel="Adults with MDE"
  }else(outcomelabel="Adults without MDE")
  if (outcome=="cSAD_averted"){outcomelabel2="Cumulative SADs averted"
  ylim=7
  }else{outcomelabel2="Cumulative LYG"
  ylim=110}

  if (FDA_include_TorF==TRUE){
    plot<-ggplot() +
      # Scenario 1 line
      geom_line(data = df_filtered, aes_string(x = "year", y = paste0(outcome,"_init_0.5_cess_2.10"), color = "'MDSE'"), size = 1) +
      # Confidence interval ribbon
      geom_ribbon(data = df_filtered,
                  aes_string(x = "year", ymin = paste0(outcome,"_init_0.1_cess_0.69"), ymax = paste0(outcome,"_init_0.85_cess_4.96")),
                  fill = "lightblue", alpha = 0.6)+
      # Additional customization
      geom_line(data = df_filtered, aes_string(x = "year", y = paste0(outcome,"_FDA_est"), color = "'FDA'"), size = 1) +
      # Confidence interval ribbon
      geom_ribbon(data = df_filtered,
                  aes_string(x = "year", ymin = paste0(outcome,"_FDA_best"), ymax = paste0(outcome,"_FDA_worst")),
                  fill = "lightpink", alpha = 0.6)+
      scale_color_manual(values = c("FDA" = "red", "MDSE" = "blue")) +
      labs(title = paste0(outcomelabel),
           x = "Year",
           y = paste0(outcomelabel2, " (millions)")) +
      theme_minimal()
  }else{
    plot<-ggplot() +
      # Scenario 1 line
      geom_line(data = df_filtered, aes_string(x = "year", y = paste0(outcome,"_init_0.5_cess_2.10")), color = "blue", size = 1) +
      # Confidence interval ribbon
      geom_ribbon(data = df_filtered,
                  aes_string(x = "year", ymin = paste0(outcome,"_init_0.1_cess_0.69"), ymax = paste0(outcome,"_init_0.85_cess_4.96")),
                  fill = "lightblue", alpha = 0.6)+
      # Additional customization
      labs(title = paste0(outcomelabel),
           x = "Year",
           y = paste0(outcomelabel2, " (millions)")) +
      scale_y_continuous(limits = c(0,ylim))+
      theme_minimal()}
  return(plot)
}

#Cost Outcome figures: columns of cSAD, cYLL and rows by mental health
#create new dataframe: 
ICERALL$icer_medLY 
ICERALL$icer_socLY 
ICERALL$icer_prodLY 

df_filtered1 <- Health_comb1%>%
  filter(year %in% c(2050:2100), population == "T")%>%
  select(year,scenario,cYLL_averted_LYG)
df_filtered2 <- ICERALL%>%
  filter( population == "total")%>%
  select(scenario,icer_medLY ,icer_socLY,icer_prodLY )

costsfigdata=merge(df_filtered1,df_filtered2,by = "scenario", all.x = TRUE)
costsfigdata$US_SOC<-(costsfigdata$icer_socLY*costsfigdata$cYLL_averted_LYG)/1000000000
costsfigdata$US_MED<-(costsfigdata$icer_medLY*costsfigdata$cYLL_averted_LYG)/1000000000
costsfigdata$US_PROD<-(costsfigdata$icer_prodLY*costsfigdata$cYLL_averted_LYG)/1000000000

#plotCosts_(costsfigdata,"US_SOC")
# data=costsfigdata
# outcome="US_SOC"

plotCosts_ <- function(data,outcome) {
  
  
  df_filtered <- data %>%
    select(year,US_SOC,US_MED,US_PROD, scenario) %>%
    pivot_wider(names_from = scenario, values_from = c(US_SOC,US_MED,US_PROD))
  
  if (outcome=="US_SOC"){outcomelabel="US Cumulative Societal Costs"
  }else if (outcome=="US_MED"){outcomelabel="US Cumulative Medical costs"
  }else(outcomelabel="US Productivity Cumulative Gains")
  plot<-ggplot() +
    # Scenario 1 line
    geom_line(data = df_filtered, aes_string(x = "year", y = paste0(outcome,"_init_0.5_cess_2.10"),color = "'MDSE'"), size = 1) +
    # Confidence interval ribbon
    geom_ribbon(data = df_filtered,
                aes_string(x = "year", ymin = paste0(outcome,"_init_0.1_cess_0.69"), ymax = paste0(outcome,"_init_0.85_cess_4.96")),
                fill = "lightblue", alpha = 0.6)+
    scale_color_manual(values = c("FDA" = "red", "MDSE" = "blue")) +
    # Additional customization
    labs(title = paste0(outcomelabel),
         x = "Year",
         y = "$ in billions") +
    scale_y_continuous(limits = c(0,100))+
    theme_minimal()
  return(plot)
}
#ages: 18.25 18.99 26.34 35.49 50.64 65.99 
#prevalence
CT<-prev_by_status(df.prevs_comb, "C", "T",FALSE,"18.99") 
ET<-prev_by_status(df.prevs_comb, "E", "T",FALSE,"18.99") 
dualT<-prev_by_status(df.prevs_comb, "CE", "T",FALSE,"18.99") 
CD<-prev_by_status(df.prevs_comb, "C", "D",FALSE,"18.99") 
ED<-prev_by_status(df.prevs_comb, "E", "D",FALSE,"18.99") 
dualD<-prev_by_status(df.prevs_comb, "CE", "D",FALSE,"18.99") 
CND<-prev_by_status(df.prevs_comb, "C", "ND",FALSE,"18.99") 
END<-prev_by_status(df.prevs_comb, "E", "ND",FALSE,"18.99") 
dualND<-prev_by_status(df.prevs_comb, "CE", "ND",FALSE,"18.99") 

CT_FDA<-prev_by_status(df.prevs_comb, "C", "T",TRUE,"18.99") 
ET_FDA<-prev_by_status(df.prevs_comb, "E", "T",TRUE,"18.99") 
dualT_FDA<-prev_by_status(df.prevs_comb, "CE", "T",TRUE,"18.99") 
CD_FDA<-prev_by_status(df.prevs_comb, "C", "D",TRUE,"18.99") 
ED_FDA<-prev_by_status(df.prevs_comb, "E", "D",TRUE,"18.99") 
dualD_FDA<-prev_by_status(df.prevs_comb, "CE", "D",TRUE,"18.99") 
CND_FDA<-prev_by_status(df.prevs_comb, "C", "ND",TRUE,"18.99") 
END_FDA<-prev_by_status(df.prevs_comb, "E", "ND",TRUE,"18.99") 
dualND_FDA<-prev_by_status(df.prevs_comb, "CE", "ND",TRUE,"18.99") 


# Health Outcomes
cSADT_FDA <- Mort_by_status(Health_comb1, "T", "cSAD_averted",TRUE)
cYLLT_FDA <- Mort_by_status(Health_comb1, "T", "cYLL_averted_LYG",TRUE)
cSADD_FDA <- Mort_by_status(Health_comb1, "D", "cSAD_averted",TRUE)
cYLLD_FDA <- Mort_by_status(Health_comb1, "D", "cYLL_averted_LYG",TRUE)
cSADND_FDA <- Mort_by_status(Health_comb1, "ND", "cSAD_averted",TRUE)
cYLLND_FDA <- Mort_by_status(Health_comb1, "ND", "cYLL_averted_LYG",TRUE)

cSADT <- Mort_by_status(Health_comb1, "T", "cSAD_averted",FALSE)
cYLLT <- Mort_by_status(Health_comb1, "T", "cYLL_averted_LYG",FALSE)
cSADD <- Mort_by_status(Health_comb1, "D", "cSAD_averted",FALSE)
cYLLD <- Mort_by_status(Health_comb1, "D", "cYLL_averted_LYG",FALSE)
cSADND <- Mort_by_status(Health_comb1, "ND", "cSAD_averted",FALSE)
cYLLND <- Mort_by_status(Health_comb1, "ND", "cYLL_averted_LYG",FALSE)

#Costs
Cost_SOC <- plotCosts_(costsfigdata,"US_SOC")
Cost_MED <- plotCosts_(costsfigdata,"US_MED")
Cost_PROD <- plotCosts_(costsfigdata,"US_PROD")



# Distribution in total population
N_age <- prev_by_status(df.prevs_comb, "N", "T",TRUE,"18.99") 
C_age <- prev_by_status(df.prevs_comb, "C", "T",TRUE,"18.99") 
F_age <- prev_by_status(df.prevs_comb, "F", "T",TRUE,"18.99") 
D_age <- prev_by_status(df.prevs_comb, "C", "T",TRUE,"18.99") 

E_age <- prev_by_status(df.prevs_comb, "E", "T",TRUE,"18.99") 
NE_age <- prev_by_status(df.prevs_comb, "NE", "T",TRUE,"18.99") 
CE_age <- prev_by_status(df.prevs_comb, "CE", "T",TRUE,"18.99") 
FE_age <- prev_by_status(df.prevs_comb, "CE", "T",TRUE,"18.99") 

# Distribution in MDE population
N_D_age <- prev_by_status(df.prevs_comb, "N", "D",TRUE,"18.99") 
C_D_age <-  prev_by_status(df.prevs_comb, "C", "D",TRUE,"18.99") 
F_D_age <-  prev_by_status(df.prevs_comb, "F", "D",TRUE,"18.99") 
E_D_age <-  prev_by_status(df.prevs_comb, "E", "D",TRUE,"18.99") 

NE_D_age <-  prev_by_status(df.prevs_comb, "NE", "D",TRUE,"18.99") 
CE_D_age <-  prev_by_status(df.prevs_comb, "CE", "D",TRUE,"18.99") 
FE_D_age <-  prev_by_status(df.prevs_comb, "FE", "D",TRUE,"18.99") 


NCFE_total_B <- ggplot() +
  geom_line(data = subset(df.prevs_comb, age == 18.99 & (status == "N" | status == "C" | status == "F"| status =="E") & population=="T"& scenario=="baseline"),  
            aes(x = year, y = prev, color = status)) +
  labs(title = paste0("Tobacco use - ", " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

NCFE_D_B <- ggplot() +
  geom_line(data = subset(df.prevs_comb, age == 18.99 & (status == "N" | status == "C" | status == "F"| status =="E") & population=="D"& scenario=="baseline"),  
            aes(x = year, y = prev, color = status)) +
  labs(title = paste0("Tobacco use - ", " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
NCFE_total_MDSE <- ggplot() +
  geom_line(data = subset(df.prevs_comb, age == 18.99 & (status == "N" | status == "C" | status == "F"| status =="E") & population=="T"& scenario=="init_0.5_cess_2.10"),  
            aes(x = year, y = prev, color = status)) +
  labs(title = paste0("Tobacco use - ", " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

NCFE_D_MDSE <- ggplot() +
  geom_line(data = subset(df.prevs_comb, age == 18.99 & (status == "N" | status == "C" | status == "F"| status =="E") & population=="D"& scenario=="init_0.5_cess_2.10"),  
            aes(x = year, y = prev, color = status)) +
  labs(title = paste0("Tobacco use - ", " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())


# Figure for e-cig transitions
eciginit <- as.data.frame(c(p.NO.NE[,"2025"],p.NO.NE[,"2026"],p.CO.CE[,"2025"],p.CO.CE[,"2026"],p.FO.FE[,"2025"],p.FO.FE[,"2026"]))
eciginit <- cbind(rep(seq(0:99),6),
                  rep(c(2025, 2026), each = 100, times = 3),
                  eciginit,c(rep("p.NO.NE",200),rep("p.CO.CE",200),rep("p.FO.FE",200)))
names(eciginit) <- c('age', 'year', 'value','prob')
p.OE_age <- ggplot(data=eciginit) +  geom_line(aes(x=age,y=value,color=prob,linetype = factor(year)))+
  scale_y_continuous(name="Probability of e-cig initiation", limits=c(0,1)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="E-cig initiation, 2025 vs 2026")

# Create the first table
# Define row names based on the calculations you're interested in
row_names <- c(
  "First Initiation Reduction", "Subsequent Initiation Reduction", "1st year Cessation", "Subsequent Cessation",
  "1st year Dual Use Risk", "Subsequent Dual Use Risk", "1st year Switching", "Subsequent Switching",
  "1st year Vape Initiation among deterred Smokers", "Subsequent Vape Initiation among deterred Smokers"
)

# Extract values into a table
mdse_values <- c(
  paste(params$init_0.5_cess_2.10[1], "(", params$init_0.1_cess_0.69[1], ", ", params$init_0.85_cess_4.96[1], ")", sep=""),
  paste(params$init_0.5_cess_2.10[2], "(", params$init_0.1_cess_0.69[2], ", ", params$init_0.85_cess_4.96[2], ")", sep=""),
  paste(params$init_0.5_cess_2.10[4], "(", params$init_0.1_cess_0.69[4], ", ", params$init_0.85_cess_4.96[4], ")", sep=""),
  paste(params$init_0.5_cess_2.10[5], "(", params$init_0.1_cess_0.69[5], ", ", params$init_0.85_cess_4.96[5], ")", sep=""),
  paste(params$init_0.5_cess_2.10[6], "(", params$init_0.1_cess_0.69[6], ", ", params$init_0.85_cess_4.96[6], ")", sep=""),
  paste(params$init_0.5_cess_2.10[7], "(", params$init_0.1_cess_0.69[7], ", ", params$init_0.85_cess_4.96[7], ")", sep=""),
  paste(params$init_0.5_cess_2.10[8], "(", params$init_0.1_cess_0.69[8], ", ", params$init_0.85_cess_4.96[8], ")", sep=""),
  paste(params$init_0.5_cess_2.10[9], "(", params$init_0.1_cess_0.69[9], ", ", params$init_0.85_cess_4.96[9], ")", sep=""),
  paste(params$init_0.5_cess_2.10[10], "(", params$init_0.1_cess_0.69[10], ", ", params$init_0.85_cess_4.96[10], ")", sep=""),
  paste(params$init_0.5_cess_2.10[11], "(", params$init_0.1_cess_0.69[11], ", ", params$init_0.85_cess_4.96[11], ")", sep="")
)

fda_values <- c(
  paste(params$FDA_est[1], "(", params$FDA_worst[1], ", ", params$FDA_best[1], ")", sep=""),
  paste(params$FDA_est[2], "(", params$FDA_worst[2], ", ", params$FDA_best[2], ")", sep=""),
  paste(params$FDA_est[4], "(", params$FDA_worst[4], ", ", params$FDA_best[4], ")", sep=""),
  paste(params$FDA_est[5], "(", params$FDA_worst[5], ", ", params$FDA_best[5], ")", sep=""),
  paste(params$FDA_est[6], "(", params$FDA_worst[6], ", ", params$FDA_best[6], ")", sep=""),
  paste(params$FDA_est[7], "(", params$FDA_worst[7], ", ", params$FDA_best[7], ")", sep=""),
  paste(params$FDA_est[8], "(", params$FDA_worst[8], ", ", params$FDA_best[8], ")", sep=""),
  paste(params$FDA_est[9], "(", params$FDA_worst[9], ", ", params$FDA_best[9], ")", sep=""),
  paste(params$FDA_est[10], "(", params$FDA_worst[10], ", ", params$FDA_best[10], ")", sep=""),
  paste(params$FDA_est[11], "(", params$FDA_worst[11], ", ", params$FDA_best[11], ")", sep="")
)

# Combine into a data frame
df <- data.frame(MDSE = mdse_values, FDA = fda_values, row.names = row_names)

# Print the data frame
table1 <- tableGrob(df, theme = ttheme_minimal(base_size = 10))
# Title for the first table
# title1 <- textGrob(paste0("mds_microsim \n",whichgender), gp = gpar(fontsize = 15))
# subtitle1 <- textGrob("Calibration fit values", gp = gpar(fontsize = 15, fontface = "bold"))

# Combine title and table for the first plot
# table1_with_titles <- arrangeGrob(grobs = list(title1, subtitle1, table1), 
#                                   nrow = 3, heights = c(0.3, 0.3, 1)

#PPT figures
jpeg(paste0("output/",figDir,"/smokingprevalence_grid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(CD, CND, ncol = 2, nrow = 1)
dev.off()

jpeg(paste0("output/",figDir,"/ecigprevalence_grid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(ED, END, ncol = 2, nrow = 1)
dev.off()

jpeg(paste0("output/",figDir,"/dualprevalence_grid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(dualD, dualND, ncol = 2, nrow = 1)
dev.off()

jpeg(paste0("output/",figDir,"/cSADgrid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(cSADT,cSADD, ncol = 2, nrow = 1)
dev.off()

jpeg(paste0("output/",figDir,"/cLYGgrid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(cYLLT,cYLLD, ncol = 2, nrow = 1)
dev.off()

#Individual figures
pdf(paste0("output/",figDir,"/prevalence_grid.pdf"), width = 9, height = 9)
grid_arrange_shared_legend(list(CT_FDA, CD_FDA, CND_FDA, ET_FDA,ED_FDA,END_FDA,dualT_FDA,dualD_FDA,dualND_FDA), nrow = 3, ncol = 3,"Product Use Prevalence by Depression Status")
dev.off()

# Save to PDF
pdf(paste0("output/",figDir,"/HealthOutcomes_grid.pdf"), width = 9, height = 9)
grid_arrange_shared_legend(list(cSADT_FDA,cSADD_FDA,cSADND_FDA,cYLLT_FDA,cYLLD_FDA,cYLLND_FDA), nrow = 2, ncol = 3,"cYLL and cSAD by Depression Status")
dev.off()

pdf(paste0("output/",figDir,"/costs_grid.pdf"), width = 9, height = 3)
grid_arrange_shared_legend(list(Cost_SOC,Cost_MED,Cost_PROD), nrow = 1, ncol = 3,"Costs for Total Population")
dev.off()

#Diagnostic figures
pdf(file = paste0(mainDir,"output/",figDir,"/diagnostic_policy_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
#maybe put a timer around table 
table1
grid_arrange_shared_legend(list(N_age, C_age, F_age),1,3,"Smoking distribution among total population")
grid_arrange_shared_legend(list(NCFE_total_B,NCFE_D_B),1,2,"Tobacco USE Baseline")
grid_arrange_shared_legend(list(NCFE_total_MDSE,NCFE_D_MDSE),1,2,"Tobacco USE MDSE main scenario")
grid_arrange_shared_legend(list(D_age),1,1,"MDE")
grid_arrange_shared_legend(list(N_D_age, C_D_age, F_D_age),1,3,"Smoking distribution among people with depression")
p.OE_age
grid_arrange_shared_legend(list(E_age,E_D_age),1,2,"Vaping by age and Depression Status")
grid_arrange_shared_legend(list(NE_age, CE_age, FE_age),1,3,"Smoking and Vaping Status Age Distribution in the Total Population")
grid_arrange_shared_legend(list(NE_D_age, CE_D_age, FE_D_age),1,3,"Smoking and Vaping Status Age Distribution in the Depressed Population")
grid_arrange_shared_legend(list(CT_FDA, CD_FDA, CND_FDA, ET_FDA,ED_FDA,END_FDA,dualT_FDA,dualD_FDA,dualND_FDA), nrow = 3, ncol = 3,"Product Use Prevalence by Depression Status")
grid_arrange_shared_legend(list(cSADT_FDA,cSADD_FDA,cSADND_FDA,cYLLT_FDA,cYLLD_FDA,cYLLND_FDA), nrow = 2, ncol = 3,"cYLL and cSAD by Depression Status")
grid_arrange_shared_legend(list(Cost_SOC,Cost_MED,Cost_PROD), nrow = 1, ncol = 3,"Costs for Total Population")
# inputs
# grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking inputs")
# grid_arrange_shared_legend(list(p.HD_age,p.HD_ageC),2,"Incidence by smoking status")
# grid.arrange(p.DR_age,p.RD_age,ncol=2) # depression recover and recurrence
# Xprobs_age # mortality probabilities
dev.off()

