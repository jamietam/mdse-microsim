# create subdirectory for figures based on today's date
figDir <- format(as.POSIXct(Sys.time()), "%m.%d.%y")
dir.create(file.path(mainDir, "output", figDir), showWarnings = FALSE)
wb <- createWorkbook()


# PREVALENCE OUTCOMES -----------------------------------------------------
#Obtain table of outcomes for smoking, deaths, and disparities:
#Combine model prevs counts, alive and dead 
#Combine by gender

#Total
df.prevs= as.data.frame(dfF[[1]][,4:6])+as.data.frame(dfM[[1]][,4:6])
df.prevs=cbind(dfF[[1]][,1:2],df.prevs,dfM[[1]][,7:8])
df.prevs$prev=df.prevs$counts/df.prevs$alive#recalculate prevalence
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

df.prevs_depsens<-df.prevs_comb %>%
  filter(year == 2100, age == age_filter, status%in% statusfilter, scenario%in% c("main", "Dep_Sens") ) %>%select(age,year,prev,scenario,population)%>%
  pivot_wider(names_from = population, values_from = c(prev))

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


# Function to process each prevalence column
process_prevalence <- function(column_name) {
  dataframe %>%
    select(year, scenario, one_of(column_name)) %>%
    rename(prevalence = column_name) %>%
    group_by(year) %>%
    summarize(
      scenario1 = list(c(sw[1], paste0(sw[2],",",sw[3]))),
      prevalence1 = list(
        c(paste0(prevalence[scenario ==sw[1]]),paste0( "(",prevalence[scenario == sw[3]]," - ", prevalence[scenario == sw[2]],  ")" ) ) ),
      .groups = 'drop' ) %>%
    unnest(cols = c(scenario1, prevalence1)) %>%
    mutate(prevalence_type = column_name) # Add a column to identify the prevalence type
}

# List of prevalence columns to process
outcome_columns<-c("T", "D", "ND", "absdiff")
# Apply the function to each prevalence column and combine the results
dataframe<-df.prevs
sw<- c("baseline","baseline","baseline") #scenarios
df.baseline <- map_dfr(outcome_columns, process_prevalence)
df.baseline <-df.baseline %>%
  pivot_wider(names_from = prevalence_type, values_from = prevalence1,names_prefix = "B_")
#MDSE Scenarios
sw<- c("main", "worst","best")
df.MDSE <- map_dfr(outcome_columns, process_prevalence)
df.MDSE <-df.MDSE %>%
  pivot_wider(names_from = prevalence_type, values_from = prevalence1)
df.MDSE<-cbind(df.MDSE,df.baseline)
df.MDSE<-df.MDSE[c("year", "scenario1","B_T","T","B_D", "D","B_ND", "ND","B_absdiff" , "absdiff" )]

#calculate depressions averted: prevalence of depression each year time population for taht year
df.prevs_D<- df.prevs_comb %>%
  filter(population=="T", age == 18.99, status=="D")%>%select(year,prev,scenario)
pop_sums <- colSums(pop)
merged_df <- merge(data.frame(year = names(pop_sums), population1 = pop_sums, stringsAsFactors = FALSE), df.prevs_D, by = "year")
# Multiplying the population by the prevalence for each year
merged_df$result <- merged_df$population1 * merged_df$prev

diffDprev=merged_df$prev[merged_df$year==2100 & merged_df$scenario=="baseline"]-merged_df$prev[merged_df$year==2100 & merged_df$scenario=="main"]

compute_scenario_difference <- function(df, scenario_a, scenario_b) {
  # Subset dataframes for each scenario
  df_a <- subset(df, scenario == scenario_a)
  df_b <- subset(df, scenario == scenario_b)
  # Rename columns to avoid conflicts during merging
  colnames(df_a)[which(colnames(df_a) == "result")] <- "result_a"
  colnames(df_b)[which(colnames(df_b) == "result")] <- "result_b"
  # Merge the dataframes by year
  merged_df <- merge(df_a, df_b, by = "year")
  # Calculate the difference between scenarios
  merged_df$difference <- merged_df$result_b - merged_df$result_a
  # Select relevant columns
  result_df <- merged_df[, c("year", "result_a", "result_b", "difference")]
  return(result_df)
}

depressiondifference=compute_scenario_difference(merged_df,"main","baseline")
cum_dep<-data.frame(sum(depressiondifference$difference[depressiondifference$year%in% c(2028:2100)]))

# add first sheet and write the data
addWorksheet(wb, "1a.Prevalance_allgender_MDSE")
writeData(wb, "1a.Prevalance_allgender_MDSE", df.MDSE)

addWorksheet(wb, "Depression_difference_poptotal")
writeData(wb, "Depression_difference_poptotal", cum_dep)


# HEALTH OUTCOMES ---------------------------------------------------------
#Health outcomes
#Combine model prevs counts, alive and dead for gender.
#PLEASE NOTE DEATH RATE SHOULD NOT BE ADDED
#Health outcomes for sensitivity

nc=ncol(dfM[[3]])
Health=dfM[[3]][,6:nc]+dfF[[3]][,6:nc]
Health=cbind(dfM[[3]][,1:2],Health)
Health_D=dfM_D[[3]][,6:nc]+dfF_D[[3]][,6:nc]
Health_D=cbind(dfM_D[[3]][,1:2],Health_D)
Health_ND=dfM_ND[[3]][,6:nc]+dfF_ND[[3]][,6:nc]
Health_ND=cbind(dfM_ND[[3]][,1:2],Health_ND)
#combine by depression status
Health_ND$population="ND"
Health_D$population="D"
Health$population="T"


Health_comb<-rbind(Health,Health_D,Health_ND)

Health_combcheck=Health_comb%>% select(scenario, cSAD_averted_new)

Health_comb_orig<-Health_comb
Health_comb<-Health_comb %>%
  filter(year %in% 2100)%>%
  select(scenario,year,population,
         aLYG_new,cLYG_new,aSAD_averted_new,cSAD_averted_new,
         aLYG_new_disc,cLYG_new_disc,aSAD_averted_new_disc,cSAD_averted_new_disc,
         cSAD_new, cLYpop_new,
         cSAD_new_disc, cLYpop_new_disc)

baseline_values <- Health_comb %>%
  filter(scenario == "baseline", year == 2100) %>%
  select(population, cSAD_new_base=cSAD_new, cLYpop_new_base=cLYpop_new)

# Calculate averted deaths and percentage changes with join
results_df <- Health_comb %>%
  filter(year == 2100) %>%
  left_join(baseline_values, by = "population") %>%  # Join baseline_values by population
  group_by(population, scenario) %>%
  mutate(
    percentage_change_LY_new = (cLYG_new) / cLYpop_new_base,
    percentage_change_sad_new = (cSAD_averted_new) / cSAD_new_base
    
  ) %>%
  ungroup() %>%  # Always a good practice to ungroup if further operations are intended
  select(scenario,population, percentage_change_LY_new, percentage_change_sad_new 
         )


Health_new<- Health_comb %>%left_join(results_df, by = c("scenario","population"))
Health_new_depsens<-Health_new%>%
  filter(scenario %in% c("main","Dep_Sens"), population=="T")%>%
  select(scenario,cSAD_averted_new)
# Calculate averted deaths and percentage changes with join
dataframe<-Health_new
dataframe_forwriting<-Health_new
variables=c( "cSAD_new", "cLYpop_new",  "cSAD_new_disc", "cLYpop_new_disc", 
             "cSAD_averted_new", "cSAD_averted_new_disc",
           "cLYG_new", "cLYG_new_disc")
dataframe[variables] <- apply(dataframe[variables], 2, function(x) paste0(sprintf("%.1f", x / 1000000)))
dataframe_forwriting[variables] <- apply(dataframe_forwriting[variables], 2, function(x) paste0(sprintf("%.2f", x / 1000000)))
dataframe$percentage_change_LY_new <- paste0(sprintf("%.1f", dataframe$percentage_change_LY_new * 100), "%")
dataframe$percentage_change_sad_new  <- paste0(sprintf("%.1f", dataframe$percentage_change_sad_new * 100), "%")


dataframesens1<-Health_new
variables=c( "cSAD_new", "cLYpop_new", "cSAD_new_disc", "cLYpop_new_disc", 
             "cSAD_averted_new", "cSAD_averted_new_disc",
            "cLYG_new", "cLYG_new_disc")
dataframesens1[variables] <- apply(dataframesens1[variables], 2, function(x) paste0(sprintf("%.2f", x / 1000000)))

sensitivity1<-dataframesens1 %>%
  filter(population == "T" , scenario %in% c("MPRPM","main","Init_Sens","Cess_Sens","CO.CE_Sens","CO.FE_Sens","NO.NE_Sens"))%>%
  select(scenario,cSAD_averted_new)
a=sensitivity1[sensitivity1$scenario=="MPRPM",]
b=sensitivity1[sensitivity1$scenario=="main",]
sensitivity1[1,]=a
sensitivity1[2,]=b
sensitivity1$per_MPRPM_new <- as.numeric(sensitivity1$cSAD_averted_new)/as.numeric(sensitivity1$cSAD_averted_new[sensitivity1$scenario=="MPRPM"])
sensitivity1$per_MPRPM_new<-paste0(sprintf("%.1f", sensitivity1$per_MPRPM_new * 100), "%")

sensitivity2<-dataframesens1 %>%
  filter(population == "T" , scenario %in% c("main","p.EX_Sens0","p.EX_Sens.15"))%>%
  select(scenario,cSAD_averted_new, cLYG_new)

sensitivity3<-dataframe %>%
  filter(population == "T" , scenario %in% c("main","Dep_Sens"))%>%
  select(scenario,cSAD_averted_new,cLYG_new)

sensitivity4<-dataframesens1 %>%
  filter(population == "T" , scenario %in% c("main","Dep_Sens"))%>%
  select(scenario,cSAD_averted_new, cLYG_new)

# Function to process each outcome column
process_prevalence_H <- function(column_name) {
  dataframe %>%
    select(population, scenario, one_of(column_name)) %>%
    rename(prevalence = column_name) %>%
    group_by(population) %>%
    summarize(
      scenario1 = list(c(sw[1], paste0(sw[2],",",sw[3]))),
      prevalence1 = list(
        c(paste0(prevalence[scenario ==sw[1]]),paste0( "(",prevalence[scenario == sw[2]]," - ", prevalence[scenario == sw[3]],  ")" ) ) ),
      .groups = 'drop' ) %>%
    unnest(cols = c(scenario1, prevalence1)) %>%
    mutate(prevalence_type = column_name) # Add a column to identify the prevalence type
}


#new lyg
outcome_columns<-c("cSAD_new","cSAD_averted_new","cSAD_new_disc","cSAD_averted_new_disc",
                   "cLYpop_new", "cLYG_new", "cLYpop_new_disc", "cLYG_new_disc","percentage_change_sad_new","percentage_change_LY_new")
sw<- c("baseline","baseline","baseline")
resultsbaseline_H <- map_dfr(outcome_columns, process_prevalence_H)
resultsbaseline_fH <-resultsbaseline_H %>%
  pivot_wider(names_from = population, values_from = prevalence1,names_prefix = "B_")

#MDSE Scenarios
sw<- c("main", "worst","best")
resultsMDSE_H <- map_dfr(outcome_columns, process_prevalence_H)
resultsMDSE_fH <-resultsMDSE_H %>%
  pivot_wider(names_from = population, values_from = prevalence1)
resultsMDSE_fH1<-cbind(resultsMDSE_fH,resultsbaseline_fH)
resultsMDSE_fH_new<-resultsMDSE_fH1[c("scenario1","prevalence_type","B_T","T","B_D","D","B_ND","ND")]



addWorksheet(wb, "1a.Table2_HealthOutcomes_new")
writeData(wb, "1a.Table2_HealthOutcomes_new", resultsMDSE_fH_new)

addWorksheet(wb, "1a._HealthOutcomes_more_detail")
writeData(wb, "1a._HealthOutcomes_more_detail", dataframe_forwriting)

addWorksheet(wb, "1b.eTable5_SensitivyMPRPM")
writeData(wb, "1b.eTable5_SensitivyMPRPM", sensitivity1)

addWorksheet(wb, "1b.eTable6_SensitivyVap")
writeData(wb, "1b.eTable6_SensitivyVap", sensitivity2)



# COST OUTCOMES -----------------------------------------------------------
## Cost Effectiveness
#Tables: I think i can just add these but i will have to recalculate ICER ratios and societal costs.
nc=ncol(dfM[[4]])
ICERST=dfM[[4]][,2:nc]+dfF[[4]][,2:nc]
ICERST$scenario=dfM[[4]][,1]

ICERST$population="T"
ICERALL=ICERST

#need to recalculate icers with combined numberators over combined demominators
ICERALL$icer_medQALY <- round(ICERALL$inc_med_cost / ICERALL$inc_effectQALY,0)
ICERALL$icer_socQALY <- round(ICERALL$inc_soc_cost / ICERALL$inc_effectQALY,0)
ICERALL$icer_medLY <- round(ICERALL$inc_med_cost / ICERALL$inc_effectLY,0)
ICERALL$icer_socLY <- round(ICERALL$inc_soc_cost / ICERALL$inc_effectLY,0)
ICERALL$icer_prodLY <- round(ICERALL$inc_prod / ICERALL$inc_effectLY,0)
ICERALL$icer_prodQALY <- round(ICERALL$inc_prod / ICERALL$inc_effectQALY,0)
ICERALL$icer_consLY <- round(ICERALL$inc_cons / ICERALL$inc_effectLY,0)
ICERALL$icer_consQALY <- round(ICERALL$inc_cons / ICERALL$inc_effectQALY,0)

#GET discounted LYG and costs
COSTS=merge(Health_comb_orig,ICERALL, by=c("population","scenario"))

COSTS$SOC_cost_US_new=round(COSTS$icer_socLY * COSTS$cLYG_new_disc/1000000000,1) #LYG from total/cummulative YLL across the years 
COSTS$MED_cost_US_new=round(COSTS$icer_medLY * COSTS$cLYG_new_disc/1000000000,1)
COSTS$Prod_US_new=round(COSTS$icer_prodLY * COSTS$cLYG_new_disc/1000000000,1) #LYG from total/cummulative YLL across the years 
COSTS$cons_US_new=round(COSTS$icer_consLY * COSTS$cLYG_new_disc/1000000000,1) 

COSTS_depsens<-COSTS%>%
  filter(population == "T" , scenario %in% c("main","Dep_Sens"), year==2100)%>%
  select(scenario, cSAD_averted_new,MED_cost_US_new)

ICERALL<- COSTS[c("year","scenario","population","icer_medQALY","icer_socQALY","icer_prodQALY","icer_consQALY","icer_medLY","icer_socLY","icer_prodLY","icer_consLY",
                  "MED_cost_US_new","SOC_cost_US_new","Prod_US_new","cons_US_new")]

outcome_columns<-c("icer_medQALY","icer_socQALY","icer_medLY","icer_socLY",
                   "MED_cost_US_new","cons_US_new","Prod_US_new","SOC_cost_US_new")
ICERALL[outcome_columns] <- lapply(ICERALL[outcome_columns], function(x) trimws(format(round(x, 0), big.mark = ",")))

dataframe<-ICERALL %>%
  filter(year == 2100)

# Define a function to change numeric values
flip_signs <- function(df) {
  df <- df %>%
    #mutate(across(Prod_US, ~ ifelse(grepl("^-", .), ., paste0("+", .))))%>%
    mutate(across(where(is.character), ~ ifelse(startsWith(., "-"), sub("-", "+", .), paste0("-", .)))) %>%
    # Flip signs for numeric columns
    mutate(across(where(is.numeric), ~ ifelse(. > 0, paste0("-", .), paste0("+", abs(.)))))
  # Process character columns to flip signs
  return(df)
}
# Assuming your dataframe is named 'dataframe'
dataframe[c("Prod_US_new")] <- flip_signs(dataframe[c("Prod_US_new")])


process_prevalence_C <- function(column_name) {
  dataframe %>%
    select(population, scenario, one_of(column_name)) %>%
    rename(prevalence = column_name) %>%
    group_by(population) %>%
    summarize(
      scenario1 = list(c(sw[1], paste0(sw[2],",",sw[3]))),
      prevalence1 = list(
        c(paste0(prevalence[scenario ==sw[1]]),paste0( "(",prevalence[scenario == sw[2]]," - ", prevalence[scenario == sw[3]],  ")" ) ) ),
      .groups = 'drop' ) %>%
    unnest(cols = c(scenario1, prevalence1)) %>%
    mutate(prevalence_type = column_name) # Add a column to identify the prevalence type
}


outcome_columns<-c("icer_medQALY","icer_socQALY","icer_medLY","icer_socLY",
                   "MED_cost_US_new","cons_US_new","Prod_US_new","SOC_cost_US_new")
sw<- c("main", "worst","best")
resultsMDSE_C <- map_dfr(outcome_columns, process_prevalence_C)
resultsMDSE_fC_new <-resultsMDSE_C %>%
  pivot_wider(names_from = population, values_from = prevalence1)

addWorksheet(wb, "1a.Table3_COSTS_new")
writeData(wb, "1a.Table3_COSTS_new", resultsMDSE_fC_new)

#Depression Sensitivity Analysis eTable7:

merged_df_sens<- merge(df.prevs_depsens, COSTS_depsens, by = "scenario")
merged_df_sens1<- merge(merged_df_sens, sensitivity4, by = "scenario")
merged_df_sens1<- merged_df_sens1[c("scenario","D","MED_cost_US_new","cLYG_new")]
df.depsens <- merged_df_sens1[rev(rownames(merged_df_sens1)), ]
addWorksheet(wb, "1b.eTable7_depsens")
writeData(wb, "1b.eTable7_depsens", df.depsens)

saveWorkbook(wb, file = paste0(mainDir, "output/",figDir,"/Tables.xlsx"), overwrite=TRUE)



# FIGURE FUNCTIONS --------------------------------------------------------
#Figure organization function
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


#Prevalence Figures functions: columns of smoking prev, ecig prev, dual use prev and rows by mental health
prev_by_status <- function(data, status_value, population_value, age_filter, ylim, ybreaks=NULL) {
  df_filtered <- subset(data, status == status_value &  population == population_value & age==age_filter)
  
  df_filtered <- df_filtered %>%
    select(year,status,population, scenario, prev) %>%
    pivot_wider(names_from = scenario, values_from = prev)
  
  if (population_value=="T"){outcomelabel="Total Population"
  dep.calib=status_value
  }else if (population_value=="D"){outcomelabel="Current MD"
  dep.calib=paste0(status_value,"_D")
  }else{outcomelabel="No Current MD"
  dep.calib=paste0(status_value,"_ND")}
  
  if (status_value=="C"){outcomelabel2=" Current Smoking"
  }else if (status_value=="E"){outcomelabel2=" Current Vaping"
  }else if (status_value=="F"){outcomelabel2=" Former Smoking"
  }else if (status_value=="N"){outcomelabel2=" Never Smoking"
  }else if (status_value=="CE"){outcomelabel2=" Dual Use"
  }else if (status_value=="NE"){outcomelabel2=" Never smoking, Current Vaping"
  }else if (status_value=="FE"){outcomelabel2=" Former smoking, Current Vaping"
  }else if (status_value=="D"){outcomelabel2=" Current MD"
  } else {outcomelabel2=" help"}
  
  plot<- ggplot() +
   geom_pointrange(data = subset(df.calib_targets, status == dep.calib & age==as.numeric(age_filter)),
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = "NSDUH Data"),size = .5, alpha = 0.5) +
    # Scenario 1 line
    geom_line(data = df_filtered, aes(x = year, y = main, color = "Nicotine Product Standard"), size = 1, linetype = "dashed") +
    geom_line(data = df_filtered, aes(x = year, y = baseline, color = "Status Quo"), size = 1) +
    # Confidence interval ribbon
    geom_ribbon(data = df_filtered,
                aes(x = year, ymin = worst, ymax = best),
                fill = "lightblue", alpha = 0.6)+
    # Additional customization
    scale_color_manual(values = c("NSDUH Data" = "black", "Nicotine Product Standard" = "blue", "Status Quo" = "black"),
                       breaks = c("NSDUH Data", "Nicotine Product Standard", "Status Quo")) +
    
    labs(title = paste0(outcomelabel,", ",outcomelabel2),
         x = "Year",
         y = paste0(outcomelabel2," Prevalence"),
         color = "") +
    scale_x_continuous(limits = c(2005,2100),breaks = c(2005,seq(2025,2100,25)))+
    scale_y_continuous(limits = c(0,ylim), breaks=ybreaks)+
    theme_minimal()+
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    guides(
      color = guide_legend(override.aes = list(linetype = c("dashed", "solid", "solid"), size = .5), keywidth = 2.5, keyheight = 1),
      linetype = guide_legend(override.aes = list(size = .5), keywidth = 2.5, keyheight = 1)
    )
  
  return(plot)
}



#ECIGonly(df.prevs_comb,"T","18.99",.2)
ECIGonly <- function(data, population_value, age_filter, ylim) {
  df_filtered <- data %>%
    filter(population == population_value, age == age_filter, status %in% c("CE", "E")) %>%
    select(year, status, scenario, prev) %>%
    pivot_wider(names_from = c(scenario, status), values_from = prev)
  
  # Subtract the "E" values from the "CE" values and create a new column for each scenario
  scenarios <- unique(data$scenario)
  for (scenario in scenarios) {
    ce_col <- paste0(scenario, "_CE")
    e_col <- paste0(scenario, "_E")
    diff_col <- paste0(scenario, "_Diff")
    if (ce_col %in% names(df_filtered) & e_col %in% names(df_filtered)) {
      df_filtered[[diff_col]] <- df_filtered[[e_col]] - df_filtered[[ce_col]]
    }
  }
  plot<- ggplot() +
    geom_pointrange(data = subset(df.calib_targets, status == "NEFE" & age==as.numeric(age_filter)), 
                    aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = "NSDUH Data"),size = .5) +
    # Scenario 1 line
    geom_line(data = df_filtered, aes(x = year, y = main_Diff, color = "Nicotine Product Standard"), size = 1) +
    geom_line(data = df_filtered, aes(x = year, y = baseline_Diff, color = "Status Quo"), size = 1) +
    # Confidence interval ribbon
    geom_ribbon(data = df_filtered,
                aes(x = year, ymin = worst_Diff, ymax = best_Diff),
                fill = "lightblue", alpha = 0.6)+
    # Additional customization
    scale_color_manual(values = c("NSDUH Data" = "black", "Nicotine Product Standard" = "blue","Status Quo" = "black")) +
    labs(title = paste0(population_value),
         x = "Year",
         y = "Prevalence",
         color = "") +
    scale_x_continuous(limits = c(2005,2100),breaks = c(2005,seq(2025,2100,25)))+
    scale_y_continuous(limits = c(0,ylim))+
    theme_minimal()+
    theme(legend.position = "bottom", legend.direction = "horizontal") 
  
  return(plot)
}  


#Health Outcome figures: columns of cSAD, cYLL and rows by mental health
Mort_by_status <- function(data, population_value, outcome, ylim) { #outcome = either cSAD_averted or cYLL_averted_LYG, sw is scenarios list 
  df_filtered <- subset(data, population == population_value) %>%
    select(year, cLYG_new, cSAD_averted_new, population, scenario)
  
  # Use apply to divide all numeric columns by 1e6
  df_filtered[, sapply(df_filtered, is.numeric)] <- apply(df_filtered[, sapply(df_filtered, is.numeric)], 2, function(x) x / 1e6)
  
  df_filtered <- df_filtered %>%
    pivot_wider(names_from = scenario, values_from = c( cLYG_new, cSAD_averted_new))
  
  if (population_value=="T"){outcomelabel="Total adult population"
  }else if (population_value=="D"){outcomelabel="Adults with MDE"
  }else(outcomelabel="Adults without MDE")
  if (startsWith(outcome, "cSAD")) {
    outcomelabel2 = "Cumulative SADs averted"
  } else {
    outcomelabel2 = "Cumulative LYG"
  }
  

    plot<-ggplot() +
      # Scenario 1 line
      geom_line(data = df_filtered, aes_string(x = "year", y = paste0(outcome,"_main")), color = "blue", size = 1) +
      # Confidence interval ribbon
      geom_ribbon(data = df_filtered,
                  aes_string(x = "year", ymin = paste0(outcome,"_worst"), ymax = paste0(outcome,"_best")),
                  fill = "lightblue", alpha = 0.6)+
      # Additional customization
      labs(title = paste0(outcomelabel),
           x = "Year",
           y = paste0(outcomelabel2, " (millions)")) +
      scale_y_continuous(limits = c(0,ylim))+
      theme_minimal()
  return(plot)
}

#Cost Outcome figures: columns of cSAD, cYLL and rows by mental health
 # data=COSTS
 # outcome="SOC_cost_US" 
 # population_value="T"
 # ylim=10
#MED_cost_US,SOC_cost_US,Prod_US
#plotCosts_(COSTS,"T","SOC_cost_US",10)

plotCosts_ <- function(data,population_value, outcome,ylim) {
  df_filtered <- subset(data,  population == population_value )
  df_filtered <- df_filtered %>%
    select(year,MED_cost_US_new,SOC_cost_US_new,Prod_US_new, scenario) %>%
    pivot_wider(names_from = scenario, values_from = c(MED_cost_US_new,SOC_cost_US_new,Prod_US_new))
  startsWith(outcome, "cSAD")
  if (startsWith(outcome, "SOC")){outcomelabel="Cumulative Societal Cost"
  }else if (startsWith(outcome, "MED")){outcomelabel="Cumulative US Medical costs"
  }else(outcomelabel="Cumulative US Productivity Gains")
   plot<- ggplot() +
    # Scenario 1 line
    geom_line(data = df_filtered, aes_string(x = "year", y = paste0(outcome,"_main")),color="blue", size = 1) +
    # Confidence interval ribbon
    geom_ribbon(data = df_filtered,
                aes_string(x = "year", ymin = paste0(outcome,"_worst"), ymax = paste0(outcome,"_best")),
                fill = "lightblue", alpha = 0.6)+
    
    # Additional customization
    labs(title = paste0(outcomelabel),
         x = "Year",
         y = "$ in billions") +
    scale_y_continuous(limits = c(0,ylim))+
    theme_minimal()
  return(plot)
}

# Utility function to extract the legend from a ggplot object
get_legend <- function(myggplot) {
  tmp <- ggplotGrob(myggplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
# SPECIFY FIGURES ---------------------------------------------------------
#ages: 18.25 18.99 26.34 35.49 50.64 65.99 
#Smoking, ECIG and DUAL prevalence
CTlegend<-prev_by_status(df.prevs_comb, "C", "T","18.99",0.5) 
CT<-prev_by_status(df.prevs_comb, "C", "T","18.99",0.5,seq(0,0.5,0.1)) + theme(legend.position = "none")
ET<-prev_by_status(df.prevs_comb, "E", "T","18.99",0.3,seq(0,0.3,0.1)) + theme(legend.position = "none")
dualT<-prev_by_status(df.prevs_comb, "CE", "T","18.99",0.11,seq(0,0.11,0.05)) + theme(legend.position = "none")
CD<-prev_by_status(df.prevs_comb, "C", "D","18.99",0.5,seq(0,0.5,0.1)) + theme(legend.position = "none")
ED<-prev_by_status(df.prevs_comb, "E", "D","18.99",0.3,seq(0,0.3,0.1)) + theme(legend.position = "none")
dualD<-prev_by_status(df.prevs_comb, "CE", "D","18.99",0.11,seq(0,0.11,0.05)) + theme(legend.position = "none")
CND<-prev_by_status(df.prevs_comb, "C", "ND","18.99",0.5,seq(0,0.5,0.1)) + theme(legend.position = "none")
END<-prev_by_status(df.prevs_comb, "E", "ND","18.99",0.3,seq(0,0.3,0.1)) + theme(legend.position = "none")
dualND<-prev_by_status(df.prevs_comb, "CE", "ND","18.99",0.11,seq(0,0.11,0.05)) + theme(legend.position = "none")

MDE_18.99<-prev_by_status(df.prevs_comb,"D","T","18.99",0.14, seq(0,0.24,0.02))
MDE_18.25<-prev_by_status(df.prevs_comb,"D","T","18.25",0.24, seq(0,0.24,0.02))

pdf(paste0("output/",figDir,"/1dep.pdf"), width = 10, height = 10)
MDE_18.99
dev.off()

                      #ECIG only figures
#Subtract prevalence of dual users minus all vapers
E_onlyT<-ECIGonly(df.prevs_comb,"T","18.99",.2)
E_onlyD<-ECIGonly(df.prevs_comb,"D","18.99",.5)
E_onlyND<-ECIGonly(df.prevs_comb,"ND","18.99",.2)


# Health Outcomes
# cLYG_new, 
cSADT_new <- Mort_by_status(Health_comb_orig, "T", "cSAD_averted_new",10)
cYLLT_new <- Mort_by_status(Health_comb_orig, "T", "cLYG_new",10)
cSADD_new <- Mort_by_status(Health_comb_orig, "D", "cSAD_averted_new",10)
cYLLD_new <- Mort_by_status(Health_comb_orig, "D", "cLYG_new",10)
cSADND_new <- Mort_by_status(Health_comb_orig, "ND", "cSAD_averted_new",10)
cYLLND_new <- Mort_by_status(Health_comb_orig, "ND", "cLYG_new",10)


#Costs
#MED_cost_US_new,SOC_cost_US_new,Prod_US_new
Cost_SOC_new  <- plotCosts_(COSTS,"T","SOC_cost_US_new",1000)
Cost_MED_new  <-  plotCosts_(COSTS,"T","MED_cost_US_new",1000) 
Cost_PROD_new  <- plotCosts_(COSTS,"T","Prod_US_new",1000) 

# Distribution in total population #prev_by_status(df.prevs_comb, "C", "T",FALSE,"18.99",0.5) 
N_age <- prev_by_status(df.prevs_comb, "N", "T","18.99",1) 
C_age <- prev_by_status(df.prevs_comb, "C", "T","18.99",0.5) 
F_age <- prev_by_status(df.prevs_comb, "F", "T","18.99",0.5) 
D_age <- prev_by_status(df.prevs_comb, "D", "T","18.99",0.13) 

E_age <- prev_by_status(df.prevs_comb, "E", "T","18.99",0.5) 
NE_age <- prev_by_status(df.prevs_comb, "NE", "T","18.99",0.5) 
CE_age <- prev_by_status(df.prevs_comb, "CE", "T","18.99",0.5) 
FE_age <- prev_by_status(df.prevs_comb, "FE", "T","18.99",0.5) 

# Distribution in MDE population
N_D_age <- prev_by_status(df.prevs_comb, "N", "D","18.99",1) 
C_D_age <-  prev_by_status(df.prevs_comb, "C", "D","18.99",0.5) 
F_D_age <-  prev_by_status(df.prevs_comb, "F", "D","18.99",0.5) 
E_D_age <-  prev_by_status(df.prevs_comb, "E", "D","18.99",0.5) 

NE_D_age <-  prev_by_status(df.prevs_comb, "NE", "D","18.99",0.5) 
CE_D_age <-  prev_by_status(df.prevs_comb, "CE", "D","18.99",0.5) 
FE_D_age <-  prev_by_status(df.prevs_comb, "FE", "D","18.99",0.5) 


NCFE_total_B <- ggplot() +
  geom_line(data = subset(df.prevs_comb, age == 18.99 & (status == "N" | status == "C" | status == "F"| status =="E") & population=="T"& scenario=="baseline"),  
            aes(x = year, y = prev, color = status)) +
  labs(title = paste0("Tobacco use total - ", " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

NCFE_D_B <- ggplot() +
  geom_line(data = subset(df.prevs_comb, age == 18.99 & (status == "N" | status == "C" | status == "F"| status =="E") & population=="D"& scenario=="baseline"),  
            aes(x = year, y = prev, color = status)) +
  labs(title = paste0("Tobacco use MDE - ", " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
NCFE_total_MDSE <- ggplot() +
  geom_line(data = subset(df.prevs_comb, age == 18.99 & (status == "N" | status == "C" | status == "F"| status =="E") & population=="T"& scenario=="main"),  
            aes(x = year, y = prev, color = status)) +
  labs(title = paste0("Tobacco use total- ", " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

NCFE_D_MDSE <- ggplot() +
  geom_line(data = subset(df.prevs_comb, age == 18.99 & (status == "N" | status == "C" | status == "F"| status =="E") & population=="D"& scenario=="main"),  
            aes(x = year, y = prev, color = status)) +
  labs(title = paste0("Tobacco use MDE- ", " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())


df.prevs_comb2 <- df.prevs_comb %>%
  mutate(status = case_when(
    population == "D" & status == "N" ~ "N_D",
    population == "D" & status == "C" ~ "C_D",
    population == "D" & status == "F" ~ "F_D",
    population == "D" & status == "E" ~ "E_D",
    TRUE ~ status  # Keep the existing value if no condition above is met
  ))
# library(ggplot2)
# library(gridExtra)
# library(grid)

# Initial plot creation similar to your code
NCFE_total <- ggplot() +
  geom_pointrange(data = subset(df.calib_targets, age == 18.99 & 
                                  (status != "E" | survey_year >= 2020) & 
                                  (status == "N" | status == "C" | status == "F" | status == "E")), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = status, shape = "National Survey on Drug Use and Health")) +
  geom_line(data = subset(df.prevs_comb, age == 18.99 & (status != "E" | year >= 2020) & 
                            (status == "N" | status == "C" | status == "F" | status == "E") & scenario == "baseline" & population == "T"),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0, 1), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Year", limits = c(calib_startyear, 2023), breaks = seq(calib_startyear, 2023, 1)) +
  labs(title = paste0("Total Population")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank(), legend.position = "bottom")

NCFE_D_total <- ggplot() +
  geom_pointrange(data = subset(df.calib_targets, age == 18.99 & 
                                  (status != "E_D" | survey_year >= 2020) & 
                                  (status == "N_D" | status == "C_D" | status == "F_D" | status == "E_D")), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = status, shape = "National Survey on Drug Use and Health")) +
  geom_line(data = subset(df.prevs_comb2, age == 18.99 & 
                            (status != "E_D" | year >= 2020) & 
                            (status == "N_D" | status == "C_D" | status == "F_D" | status == "E_D") & scenario == "baseline" & population == "D"),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0, 1), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Year", limits = c(calib_startyear, 2023), breaks = seq(calib_startyear, 2023, 1)) +
  labs(title = paste0("Current MDE Population")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank(), legend.position = "bottom")


df.prevs_comb_1<-df.prevs_comb%>% filter(age==18.99,scenario %in% c("main", "Dep_Sens", "baseline", "Dep_base"), population=="T",status == "D")


df.prevs_comb_1 <- df.prevs_comb_1 %>%
  mutate(color_group = ifelse(scenario %in% c("Dep_Sens", "Dep_base"), "Depression 2016", "Depression 2022"),
         line_type = ifelse(scenario %in% c("Dep_base", "baseline"), "Status Quo", "Policy"))


Depression_2016_2023_baseline <-ggplot() +
  geom_pointrange(data = subset(df.calib_targets, status == "D" & age == 18.99), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = "NSDUH data")) +
  geom_line(data = subset(df.prevs_comb_1, status == "D" & age == 18.99 & scenario %in% c("main", "Dep_Sens", "baseline", "Dep_base") & population == "T"), 
            aes(x = year, y = prev, color = color_group, linetype = line_type)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0.06, 0.12)) +
  scale_x_continuous(name = "Year") +
  scale_color_manual(values = c("Depression 2016" = "blue", "Depression 2022" = "black", "NSDUH data" = "black"),
                     name = "",
                     labels = c("Depression 2016" = "Incidence returns to \npre-2016 levels from 2023-2100", "Depression 2022" = "Incidence levels \nconstant 2016-2100", "NSDUH data" = "NSDUH Data")) +
  scale_linetype_manual(values = c("Status Quo" = "solid", "Policy" = "dashed"),
                        name = "",
                        labels = c("Status Quo" = "Status Quo (Solid)", "Policy" = "Nicotine Product \n Standard (Dashed)"))+
  theme_minimal()
  
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

# Create table of parameters
# Define row names based on the calculations you're interested in
row_names <- c(
  "First Initiation Reduction", "Subsequent Initiation Reduction", "1st year Cessation", "Subsequent Cessation",
  "1st year Dual Use Risk", "Subsequent Dual Use Risk", "1st year Switching", "Subsequent Switching",
  "1st year Vape Initiation among deterred Smokers", "Subsequent Vape Initiation among deterred Smokers", "vaping mortality"
)
# Extract values into a table
mdse_values <- c(
  paste(params$main[1], "(", params$worst[1], ", ", params$best[1], ")", sep=""),
  paste(params$main[2], "(", params$worst[2], ", ", params$best[2], ")", sep=""),
  paste(params$main[3], "(", params$worst[3], ", ", params$best[3], ")", sep=""),
  paste(params$main[4], "(", params$worst[4], ", ", params$best[4], ")", sep=""),
  paste(params$main[5], "(", params$worst[5], ", ", params$best[5], ")", sep=""),
  paste(params$main[6], "(", params$worst[6], ", ", params$best[6], ")", sep=""),
  paste(params$main[7], "(", params$worst[7], ", ", params$best[7], ")", sep=""),
  paste(params$main[8], "(", params$worst[8], ", ", params$best[8], ")", sep=""),
  paste(params$main[9], "(", params$worst[9], ", ", params$best[9], ")", sep=""),
  paste(params$main[10], "(", params$worst[10], ", ", params$best[10], ")", sep=""),
  paste(params$main[11], "(", params$worst[11], ", ", params$best[11], ")", sep="")
)


# Combine into a data frame
df <- data.frame(MDSE = mdse_values,  row.names = row_names)
table1 <-grid.table(df)



# PAPER FIGURES -----------------------------------------------------------
shared_legend <-get_legend(CTlegend)

pdf(paste0("output/",figDir,"/1a.Fig2_prevalence.pdf"), width = 10, height = 10)
grid.arrange(arrangeGrob(CD, CND,CT,ED,END,ET,dualD,dualND,dualT, ncol = 3,nrow=3), shared_legend,  # Add shared legend
             ncol = 1, heights = c(4, 0.5) )
dev.off()

pdf(paste0("output/",figDir,"/1b.eFig2_Depression.pdf") , width = 6, height = 6)
Depression_2016_2023_baseline
dev.off()


# PPT FIGURES -------------------------------------------------------------
#PPT figures
jpeg(paste0("output/",figDir,"/3.smokingprevalence_grid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(CD, CND, ncol = 2, nrow = 1)
dev.off()

jpeg(paste0("output/",figDir,"/3.ecigprevalence_grid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(ED, END, ncol = 2, nrow = 1)
dev.off()

jpeg(paste0("output/",figDir,"/3.dualprevalence_grid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(dualD, dualND, ncol = 2, nrow = 1)
dev.off()

jpeg(paste0("output/",figDir,"/3.cSAD_newgrid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(cSADT_new,cSADD_new, ncol = 2, nrow = 1)
dev.off()

jpeg(paste0("output/",figDir,"/3.cLYGgrid.jpeg"), width = 9, height = 3.5, units = "in", res = 500)
grid.arrange(cYLLT_new,cYLLD_new, ncol = 2, nrow = 1)
dev.off()

jpeg(paste0("output/",figDir,"/MDE.jpeg"), width = 4.5, height = 5.5, units = "in", res = 500)
Depression_2016_2023_baseline
dev.off()

# DIAGNOSTIC FIGURES PDF --------------------------------------------------

#Diagnostic figures
pdf(file = paste0(mainDir,"output/",figDir,"/2.diagnostic_policy_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
#maybe put a timer around table 
grid.table(df)
grid_arrange_shared_legend(list(N_age, C_age, F_age),1,3,"Smoking distribution among total population")
grid_arrange_shared_legend(list(NCFE_total_B,NCFE_D_B),1,2,"Tobacco USE Baseline")
grid_arrange_shared_legend(list(NCFE_total_MDSE,NCFE_D_MDSE),1,2,"Tobacco USE MDSE main scenario")
grid_arrange_shared_legend(list(Depression_2016_2023_baseline),1,1,"MDE")
grid_arrange_shared_legend(list(N_D_age, C_D_age, F_D_age),1,3,"Smoking distribution among people with depression")
 p.OE_age
grid_arrange_shared_legend(list(E_age,E_D_age),1,2,"Vaping by Depression Status")
grid_arrange_shared_legend(list(E_onlyT,E_onlyD,E_onlyND),1,3,"Exclusive Vaping by Depression Status")
grid_arrange_shared_legend(list(NE_age, CE_age, FE_age),1,3,"Smoking and Vaping Status in the Total Population")
grid_arrange_shared_legend(list(NE_D_age, CE_D_age, FE_D_age),1,3,"Smoking and Vaping Status in the Depressed Population")
grid_arrange_shared_legend(list(MDE_18.99,MDE_18.25),1,2,"MD prevalence")
grid.arrange(Cost_MED_new, Cost_SOC_new, Cost_PROD_new, nrow = 1, ncol = 3)# inputs
grid_arrange_shared_legend(list(MDE_18.99,MDE_18.25),1,2,"MD prevalence")
dev.off()

