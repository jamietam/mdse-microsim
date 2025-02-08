#Table and Figures for analysis
#run this for men and women:
load("output/rnc_females_1000_12.14.24_11.48AM.Rda")
rF=allresults
load("output/rnc_males_1000_12.14.24_03.06PM.Rda")
rM=allresults

# load NSDUH combined men/women data
load(paste0(mainDir,"data/nsduh_calib_targets_both.RData")) 

#choose scenarios of interest:
scenariosofinterest=c("baseline", "init_0.1_cess_0.69" ,"init_0.5_cess_2.10", "init_0.85_cess_4.96")

df=reformat_model_outputs(l.results[scenariosofinterest])
df_D=reformat_model_outputs(l.results_D[scenariosofinterest])
df_notD=reformat_model_outputs(l.results_notD[scenariosofinterest])

#Table1 Prevalence disparities:



#Obtain table of outcomes for smoking, deaths, and disparities:
modelprevs= df[[1]]
modelprevs_D= df_D[[1]]
modelprevs_notD= df_notD[[1]]

years_of_interest <- c(2025, 2040, 2060, 2080, 2100)
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


write.xlsx(PrevalenceDisp, file = paste0("output/Prevlanec_Disparities_Table",whichgender,".xlsx"))


## Table 3 from Xi paper

dfI_D=df_D[[3]]
dfI_notD=df_notD[[3]]


years_of_interest <- c(2030, 2040, 2050, 2060, 2070,2080, 2090, 2100)
scenario_filter<- c("baseline" ,"init_0.5_cess_2.10")

filtered_dfI_D <- dfI_D %>%
  filter(year %in% years_of_interest, scenario%in% scenario_filter) %>%select(year,scenario,cSAD,cLYG)
filtered_dfI_D$population="D"

filtered_dfI_notD <- dfI_notD %>%
  filter(year %in% years_of_interest, scenario%in% scenario_filter) %>%select(year,scenario,cSAD,cLYG)
filtered_dfI_D$population="ND"


df_wide_D <-filtered_dfI_D %>%
  pivot_wider(names_from = scenario, values_from = c(cSAD,cLYG))


df_wide_notD <-filtered_dfI_notD  %>%
  pivot_wider(names_from = scenario, values_from = c(cSAD,cLYG))

df_wide_D$cSAD_baseline
df_wide_D$DifferencecSAD=df_wide_D$cSAD_init_0.5_cess_2.10-df_wide_D$cSAD_baseline
df_wide_D$no_initasperccSAD=df_wide_D$cSAD_baseline/df_wide_D$cSAD_init_0.5_cess_2.10
df_wide_D$complete_CessaspercSAD=df_wide_D$DifferencecSAD/df_wide_D$cSAD_init_0.5_cess_2.10
df_wide_D$DifferenceccLYG=df_wide_D$cLYG_init_0.5_cess_2.10-df_wide_D$cLYG_baseline
df_wide_D$no_initasperccLYG=df_wide_D$cLYG_baseline/df_wide_D$cLYG_init_0.5_cess_2.10
df_wide_D$complete_CessaspercLYG=df_wide_D$DifferenceccLYG/df_wide_D$cLYG_init_0.5_cess_2.10
df_wide_D$population="depressed"

df_wide_notD$DifferencecSAD=df_wide_notD$cSAD_init_0.5_cess_2.10-df_wide_notD$cSAD_baseline
df_wide_notD$no_initasperccSAD=df_wide_notD$cSAD_baseline/df_wide_notD$cSAD_init_0.5_cess_2.10
df_wide_notD$complete_CessaspercSAD=df_wide_notD$DifferencecSAD/df_wide_notD$cSAD_init_0.5_cess_2.10
df_wide_notD$DifferenceccLYG=df_wide_notD$cLYG_init_0.5_cess_2.10-df_wide_notD$cLYG_baseline
df_wide_notD$no_initasperccLYG=df_wide_notD$cLYG_baseline/df_wide_notD$cLYG_init_0.5_cess_2.10
df_wide_notD$complete_CessaspercLYG=df_wide_notD$DifferenceccLYG/df_wide_notD$cLYG_init_0.5_cess_2.10
df_wide_notD$population="not depressed"


df_wide_D=df_wide_D[,c("year","population","cSAD_baseline","cSAD_init_0.5_cess_2.10","DifferencecSAD","no_initasperccSAD","complete_CessaspercSAD",
                      "cLYG_baseline","cLYG_init_0.5_cess_2.10","DifferenceccLYG","no_initasperccLYG","complete_CessaspercLYG")]
names(df_wide_D)=c("year","population","No-initiation scenario cSAD", "init_0.5_cess_2.10 scenario cSAD","Difference (complete cessastion) cSAD","No-initiation as % of init_0.5_cess_2.10 cSAD", "Complete cessation as % of init_0.5_cess_2.10 cSAD",
                   "No-initiation scenario cLYG", "init_0.5_cess_2.10 scenario cLYG","Difference (complete cessastion) cLYG","No-initiation as % of init_0.5_cess_2.10 cLYG", "Complete cessation as % of init_0.5_cess_2.10 cLYG")


df_wide_notD=df_wide_notD[,c("year","population","cSAD_baseline","cSAD_init_0.5_cess_2.10","DifferencecSAD","no_initasperccSAD","complete_CessaspercSAD",
                       "cLYG_baseline","cLYG_init_0.5_cess_2.10","DifferenceccLYG","no_initasperccLYG","complete_CessaspercLYG")]
names(df_wide_notD)=c("year","population","No-initiation scenario cSAD", "init_0.5_cess_2.10 scenario cSAD","Difference (complete cessastion) cSAD","No-initiation as % of init_0.5_cess_2.10 cSAD", "Complete cessation as % of init_0.5_cess_2.10 cSAD",
                   "No-initiation scenario cLYG", "init_0.5_cess_2.10 scenario cLYG","Difference (complete cessastion) cLYG","No-initiation as % of init_0.5_cess_2.10 cLYG", "Complete cessation as % of init_0.5_cess_2.10 cLYG")



table3=rbind(df_wide_D,df_wide_notD)

write.xlsx(table3, file = paste0("output/Table3",whichgender,".xlsx"))






#Tables:
ICERST=df[[4]]
ICERSD=df_D[[4]]
ICERSND=df_notD[[4]]

ICERSND$population="not derpessed"
ICERSD$population="derpessed"
ICERST$population="total"
ICERALL=rbind(ICERST,ICERSD,ICERSND)
ICERALL$SOC_cost_US=ICERALL$icer_socLY * ICERALL$US_LYG_cum #LYG from total/cummulative YLL across the years 
ICERALL$MED_cost_US=ICERALL$icer_medLY * ICERALL$US_LYG_cum

ICERALL1<- ICERALL[c("scenario","population","cum_YLL","avg_YLL","cum_SAD","avg_SAD","icer_medQALY","icer_socQALY","icer_medLY","icer_socLY","US_LYG_avg","US_LYG_cum","MED_cost_US","SOC_cost_US" )]
names(ICERALL1) <-c("scenario","population","Cummulative Years of Life Lost (2023-2100)","Average Years of Life Lost (2023-2100)","Cummulative Smoking Attributable Deaths (2023-2100)","Average Smoking Attributable Deaths","ICER Medical Costs per QALY",
                    "ICER Societal Cost per QALY","ICER Medical Cost per LY","ICER Societal Cost per LY","US Life Years Gained on Average","US Cummulative LYG (2023-2100)","Cummulative US Medical Costs (2023-2100)","Cummulative US Societal Costs (2023-2100)" )
write.xlsx(ICERALL1, file = paste0("output/COSTS_Table",whichgender,".xlsx"))






#figures for depressed, not depressed, and total

for (i in 1:3){
  
  typefig=i #1=total, 2= depressed, 3=not depressed
  specialname="Not Depressed"
  if (typefig==1){
    l.results_total <- reformat_model_outputs(l.results)
    specialname="Total Population"
    status1=c("C")
  }else if(typefig==2){
    l.results_total <- reformat_model_outputs(l.results_D)
    specialname="Depressed"
    status1="C_D"
  }else if (typefig==3){
    l.results_total <- reformat_model_outputs(l.results_notD)
    specialname="Not Depressed"
    status1="C"
  }
  #df.model_prevs, smkprobs, combined_data, df.cea
  # Specify which population to generate results for
  #results = l.results
  #results = l.results_D
  
  df.model_prevs=l.results_total[[1]]
  smkprobs=l.results_total[[2]]
  combined_data=l.results_total[[3]]
  df.cea=l.results_total[[4]]
  # FIGURES -----------------------------------------------------------------
  
  # Define ggplot figures
  create_figure <- function(data, y, title, xlim = NULL, ylim = NULL, ylab = NULL) {
    ggplot(data = data) +
      geom_line(aes(x = year, y = y, color = scenario)) +
      scale_x_continuous(name = "Year", limits = xlim) +
      scale_y_continuous(name = ylab, limits = ylim) +
      labs(title = title) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  }
  
  
  p.NC_age <- ggplot(data = smkprobs) +
    geom_line(aes(x = age, y = init, linetype = scenario, color = scenario)) +
    scale_x_continuous(name = "Age", limits = c(0, 30), breaks = seq(0, 99, 10)) +
    labs(title = "Initiation probabilities")
  
  p.CF_age <- ggplot(data = smkprobs) +
    geom_line(aes(x = age, y = cess, linetype = scenario, color = scenario)) +
    scale_x_continuous(name = "Age", limits = c(0, 99), breaks = seq(0, 99, 10)) +
    labs(title = "Cessation probabilities")
  
  
  csprevs <- ggplot() +
    geom_pointrange(data = subset(df.calib_targets, age == 18.99 & (status %in% status1)),#| status == "C" "C_D"
                    aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, linetype = status, shape = status)) +
    geom_line(data = subset(df.model_prevs, age == 18.99 & (status == "C" )),#| status == "C"
              aes(x = year, y = prev, color = scenario)) +
    scale_y_continuous(name = "Prevalence (%)", limits = c(0, 0.5), breaks = seq(0, 0.5, 0.05)) +
    scale_x_continuous(name = "Year", limits = c(2005, 2100), breaks = seq(2005, 2100, 10)) +
    labs(title = paste0("Current smoking - ",whichgender, " ages 18-99")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  aSAD_fig <- create_figure(combined_data, combined_data$aSAD / 1000, "Smoking-attributable deaths (annual)", xlim = c(2020, 2100), ylab = "Smoking-attributable deaths (thousands)")
  aSAD_averted_fig <- create_figure(combined_data, combined_data$aSAD_averted / 1000, "SADs averted", xlim = c(2020, 2100), ylab = "Smoking-attributable deaths (thousands)")
  cSAD_fig <- create_figure(combined_data, combined_data$cSAD / 1000, "Smoking-attributable deaths (cumulative)", xlim = c(2020, 2100), ylab = "Smoking-attributable deaths (thousands)")
  cSAD_averted_fig <- create_figure(combined_data, combined_data$cSAD_averted / 1000, "SADs averted (cumulative)", xlim = c(2020, 2100), ylab = "Smoking-attributable deaths (thousands)")
  
  aLY_fig <- create_figure(combined_data, combined_data$aLY / 1000000, "Life-years (annual)", xlim = c(2020, 2100), ylab = "Life-years (millions)")
  cLY_fig <- create_figure(combined_data, combined_data$cLY / 1000000, "Life-years (cumulative)", xlim = c(2020, 2100), ylab = "Life-years (millions)")
  dLY_fig <- create_figure(combined_data, combined_data$dLY, "Life-years (difference)", xlim = c(2020, 2100), ylab = "Life-years (millions)")
  
  aQALYs_fig <- create_figure(combined_data, combined_data$aQALYs / 1000000, "QALYs (annual)", xlim = c(2020, 2100), ylab = "QALYs (millions)")
  cQALYs_fig <- create_figure(combined_data, combined_data$cQALYs / 1000000, "QALYs (cumulative)", xlim = c(2020, 2100), ylab = "QALYs (millions)")
  dQALYs_fig <- create_figure(combined_data, combined_data$dQALYs / 1000000, "QALYs (difference)", xlim = c(2020, 2100), ylab = "QALYs (millions)")
  
  
  acosts_fig <- create_figure(combined_data, combined_data$aCosts / 1000000, "Costs (annual)", xlim = c(2020, 2100), ylab = "Costs ($ millions)")
  ccosts_fig <- create_figure(combined_data, combined_data$cCosts / 1000000, "Costs (cumulative)", xlim = c(2020, 2100), ylab = "Costs ($ millions)")
  dcosts_fig <- create_figure(combined_data, combined_data$dCosts / 1000000, "Costs (difference)", xlim = c(2020, 2100), ylab = "Costs ($ millions)")
  
  aprod_fig <- create_figure(combined_data, combined_data$aProd / 1000000, "Productivity (annual)", xlim = c(2020, 2100), ylab = "Productivity ($ millions)")
  cprod_fig <- create_figure(combined_data, combined_data$cProd / 1000000, "Productivity (cumulative)", xlim = c(2020, 2100), ylab = "Productivity ($ millions)")
  dprod_fig <- create_figure(combined_data, combined_data$dProd / 1000000, "Productivity (difference)", xlim = c(2020, 2100), ylab = "Productivity ($ millions)")
  
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
  
  # Create PDF of results
  pdf(file = paste0(mainDir,"output/", "policy_general_",specialname,whichgender,"_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
  plot.new()
  text(.5, 0.5, paste0("Policy outcomes \n",specialname,whichgender,"\n",n.i," per birth cohort"), font=1, cex=1.5)
  #text(cea_data)
  grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking probabilities")
  csprevs
  grid_arrange_shared_legend(list(aSAD_averted_fig,cSAD_averted_fig),2,"Smoking-Attributable Deaths")
  grid_arrange_shared_legend(list(acosts_fig, ccosts_fig, dcosts_fig),3, "Medical costs")
  grid_arrange_shared_legend(list(aprod_fig, cprod_fig, dprod_fig),3, "Productivity")
  grid_arrange_shared_legend(list(aQALYs_fig, cQALYs_fig, dQALYs_fig),3, "QALYs")
  aLY_fig
  #grid_arrange_shared_legend(list(aLY_fig, cLY_fig),2, "LYs")
  dev.off()
}
# Save the ggplot figures
# save_plot <- function(plot, filename) {
#   ggsave(filename = filename, plot = plot, dpi = 300, width = 10, height = 6, units = "in")
# }
# save_plot(p.NC_age, paste0(mainDir,"output/","initiation_age.png"))
# save_plot(p.CF_age, paste0(mainDir,"output/","cessation_age.png"))
# save_plot(csprevs, paste0(mainDir,"output/","csprev_targets_18_99.png"))
# save_plot(cSADs, paste0(mainDir,"output/","cSADs.png"))
# save_plot(cXs, paste0(mainDir,"output/","cX.png"))
# save_plot(cLYs, paste0(mainDir,"output/","cLY.png"))
# save_plot(costs, paste0(mainDir,"output/","costs.png"))
# save_plot(QALYs, paste0(mainDir,"output/","QALYs.png"))
# save_plot(prod, paste0(mainDir,"output/","prod.png"))