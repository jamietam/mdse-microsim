## Clean up the workspace and set main working directory
rm(list = ls()) 

mainDir = "/Users/srs249/Documents/GitHub/mds-microsim/"
# mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" # Set working directory
hpc=0
calibration=0 #need to set this to 0 so main_calib works and outputs proper matrix for main function
args <- `if`(hpc == 1, commandArgs(TRUE), c("males", 1000, 2100, 40)) # Parameters for HPV vs non-HPC setup

source(paste0(mainDir,"R/01_environment.R"), echo=FALSE) #
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE) # microsimulation model and probability functions

# Run the model -----------------------------------------------------------
policyyear <- 2025
v.affected_ages <- c(0:99) # affects all ages

# Policy effect sizes:
# Apelberg (2018): Experts estimate 50% (10-85%) decrease in smoking initiation https://doi.org/10.1056/NEJMsr1714617
# Hatsukami (2024): Significantly higher 12-week CO-verified abstinence among those in VLNC vs NNC condition (OR=3.10, 95%: 1.69-5.96) https://doi.org/10.1016/j.lana.2024.100796

params <- list(
  baseline = c(1, 1),
  MPRPM = c(0,100000), # no initiation, everyone quits (setting it to 100000 makes all cessation probabilities set to 1)
  noinit_cess_1=c(0,1)
  # init_0.1 = c(0.9,1), # 10% decrease in initiation
  # init_0.85 = c(0.15,1), # 85% decrease in initiation
  # cess_2.10 = c(1,3.10), # 210% increase in cessation
  # cess_0.69 = c(1,1.69), # 69% increase in cessation
  # cess_4.96 = c(1,5.96), # 496% increase in cessation
  # init_0.5_cess_2.10 = c(0.5, 3.10), # 50% decrease to initiation (rr.init=0.5) , rr.cess=3.10
  # init_0.5_cess_0.69 = c(0.5, 1.69), # rr.cess=1.69
  # init_0.5_cess_4.96 = c(0.5, 5.96), # rr.cess=5.96
  # init_0.1_cess_2.10 = c(0.9, 3.10), # 10% decrease to initiation (rr.init=0.9) 
  # init_0.1_cess_0.69 = c(0.9, 1.69),
  # init_0.1_cess_4.96 = c(0.9, 5.96),
  # init_0.85_cess_2.10 = c(0.15, 3.10), # 85% decrease to initiation (rr.init=0.15)
  # init_0.85_cess_0.69 = c(0.15, 1.69),
  # init_0.85_cess_4.96 = c(0.15, 5.96)
)

scenarios <- names(params)

run_policy <- function(policy) {
  cat(paste0("\n  Scenario: ", policy))
  l.policy_effects <- apply_policy(params[[policy]][1], params[[policy]][2], policyyear, v.affected_ages)
  output <- main(v.params, l.policy_effects)
  return(output)
}

# Run all scenarios and save results
allresults <- lapply(scenarios, run_policy)

names(allresults) <- scenarios
save(allresults, file = paste0("output/rnc_",whichgender,"_",n.i,"_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".Rda"))

#load("output/rnc_males_1000_12.03.24_03.10PM.Rda")

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

# Reformat data for data visualization ------------------------------------

# NSDUH prevalence data
df.calib_targets <- do.call(rbind, lapply(names(l.calib_targets), function(status) {
  cbind(data.frame(l.calib_targets[[status]]), status = status)
}))

reformat_model_outputs <- function(l.results){
  # Combine model prevalences for all health states and all scenarios into one dataframe
  df.model_prevs <- do.call(rbind, lapply(names(l.results), function(name) {
    v.health_states <- names(l.results[[name]]$l.model_prevs)
    df.model_prevs <- do.call(rbind, lapply(v.health_states, function(state) {
      cbind(data.frame(l.results[[name]]$l.model_prevs[[state]]), status = state)
    }))
    df.model_prevs$scenario <- name
    return(df.model_prevs)
  }))
  
  # Smoking initiation and cessation
  smkprobs <- do.call(rbind, lapply(names(l.results), function(name) {
    policy <- l.results[[name]]
    data.frame(cbind(policy$init[, calib_endyear - 1899], policy$cess[, calib_endyear - 1899], 0:99, name))
  }))
  colnames(smkprobs) <- c("init", "cess", "age", "scenario")
  smkprobs[, c("init", "cess", "age")] <- sapply(smkprobs[, c("init", "cess", "age")], as.numeric)
  
  # Mortality (X), life-years (ly), and cost-utility data
  combined_data <- lapply(names(l.results), function(name) {
    policy <- l.results[[name]]
    cuw <- policy$m.cuw
    data.frame(
      year = as.numeric(names(policy$v.lifeyears[paste0(d.year:max(cohorts))])),
      scenario = name,
      aLY = policy$v.lifeyears[paste0(d.year:max(cohorts))],
      cLY = cumsum(policy$v.lifeyears[paste0(d.year:max(cohorts))]),
      aSAD = policy$v.SAD,
      cSAD = cumsum(policy$v.SAD),
      aCosts = cuw[paste0(d.year:max(cohorts)), 1],
      aQALYs = cuw[paste0(d.year:max(cohorts)), 2],
      aProd = cuw[paste0(d.year:max(cohorts)), 3],
      aNonhealth = cuw[paste0(d.year:max(cohorts)), 4],
      cCosts = cumsum(cuw[paste0(d.year:max(cohorts)), 1]),
      cQALYs = cumsum(cuw[paste0(d.year:max(cohorts)), 2]),
      cProd = cumsum(cuw[paste0(d.year:max(cohorts)), 3]),
      cNonhealth = cumsum(cuw[paste0(d.year:max(cohorts)), 4])
    )
  }) %>%
    bind_rows()
  
  # Ensure numeric columns are indeed numeric
  combined_data <- combined_data %>%
    mutate(across(c(year, aLY, cLY, aSAD, cSAD,
                    aCosts,cCosts,aQALYs,aNonhealth, cQALYs,aProd,cProd,cNonhealth), as.numeric))
  
  # Extract baseline values
  baseline_values <- combined_data %>%
    filter(scenario == "baseline") %>%
    select(year, aLY, cLY, cSAD, aSAD,aCosts,cCosts,aQALYs,cQALYs,aProd,cProd,aNonhealth,cNonhealth) %>%
    rename(
      baseline_cLY = cLY,
      baseline_aLY = aLY,
      baseline_cSAD = cSAD, 
      baseline_aSAD = aSAD,
      baseline_aCosts = aCosts,
      baseline_aQALYs = aQALYs,
      baseline_aProd = aProd,
      baseline_cCosts = cCosts,
      baseline_cQALYs = cQALYs,
      baseline_cProd = cProd,
      baseline_aNonhealth = aNonhealth,
      baseline_cNonhealth = cNonhealth
    )
  
  # Join baseline values with main data frame and calculate difference in values
  combined_data <- combined_data %>%
    left_join(baseline_values, by = "year") %>%
    mutate(
      cSAD_averted = baseline_cSAD - cSAD,
      aSAD_averted = baseline_aSAD - aSAD,
      cLYG = cLY - baseline_cLY,
      aLYG = aLY - baseline_aLY,
      dCosts = cCosts - baseline_cCosts,
      dCosts = cNonhealth - baseline_cNonhealth,
      dQALYs = cQALYs - baseline_cQALYs,
      dProd = cProd - baseline_cProd
    ) %>%
    select(-baseline_aLY, -baseline_cLY, -baseline_cSAD, -baseline_aSAD, 
           -baseline_cCosts, -baseline_cQALYs, -baseline_cProd, -baseline_aCosts, -baseline_aQALYs, -baseline_aProd )  # Remove temporary baseline columns
  
  # Calculate ICER
  cea_data <- lapply(names(l.results), function(name) {
    policy <- l.results[[name]]
    data.frame(scenario = name,
               avg_med_costs = policy$v.cea["avg_med_costs"],
               avg_cons_exp = policy$v.cea["avg_cons_exp"],
               avg_prod = policy$v.cea["avg_prod"],
               avg_soc_costs = policy$v.cea["avg_soc_costs"],
               avg_QALYs = policy$v.cea["avg_QALYs"],
               avg_LYs = policy$v.cea["avg_LYs"])
  }) %>%
    bind_rows()
  
  df.cea <- calc_icers(cea_data)
  return(list(df.model_prevs, smkprobs, combined_data, df.cea))
}

calc_icers <- function(cea_data) {
  if (nrow(cea_data) > 1) {
    # cea_data[1, "icer"] <- NA # First scenario "baseline" is the reference case
    for (i in 2:nrow(cea_data)) {
      inc_med_cost <- (cea_data[i, "avg_med_costs"] - cea_data[1, "avg_med_costs"])
      inc_soc_cost <- (cea_data[i, "avg_soc_costs"] - cea_data[1, "avg_soc_costs"])
      inc_cons_exp <- (cea_data[i, "avg_cons_exp"] - cea_data[1, "avg_cons_exp"])
      inc_prod <- (cea_data[i, "avg_prod"] - cea_data[1, "avg_prod"])
      inc_effectQALY <- (cea_data[i, "avg_QALYs"] - cea_data[1, "avg_QALYs"])
      inc_effectLY <- (cea_data[i, "avg_LYs"] - cea_data[1, "avg_LYs"])
      cea_data[i, "inc_med_cost"] <- inc_med_cost
      cea_data[i, "inc_cons_exp"] <- inc_cons_exp
      cea_data[i, "inc_prod"] <- inc_prod
      cea_data[i, "inc_soc_cost"] <- inc_soc_cost
      cea_data[i, "inc_effectQALY"] <- inc_effectQALY
      cea_data[i, "inc_effectLY"] <- inc_effectLY
      cea_data[i, "icer_medQALY"] <- round(inc_med_cost / inc_effectQALY,0)
      cea_data[i, "icer_socQALY"] <- round(inc_soc_cost / inc_effectQALY,0)
      cea_data[i, "icer_medLY"] <- round(inc_med_cost / inc_effectLY,0)
      cea_data[i, "icer_socLY"] <- round(inc_soc_cost / inc_effectLY,0)
    }
  } 
  return(cea_data)
}


#Obtain table of ICERS:
ICERST=  reformat_model_outputs(l.results)[[4]]
ICERSD=reformat_model_outputs(l.results_D)[[4]]
ICERSND=reformat_model_outputs(l.results_notD)[[4]]
ICERSND$population=c(rep("not derpessed",3))
ICERSD$population=c(rep("derpessed",3))
ICERST$population=c(rep("total",3))
ICERALL=rbind(ICERST,ICERSD,ICERSND)



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