## Clean up the workspace and set main working directory
rm(list = ls()) 


mainDir = "/Users/bradleydirks/University of Michigan Dropbox/Sarah Skolnick/GitHub/mdse-microsim/"
# mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" # Set working directory
hpc=0
calibration=0 #need to set this to 0 so main_calib works and outputs proper matrix for main function
args <- `if`(hpc == 1, commandArgs(TRUE), c("females",1000, 2100, 40)) # Parameters for HPV vs non-HPC setup

source(paste0(mainDir,"R/01_environment.R"), echo=FALSE) #
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE) # microsimulation model and probability functions

calib_endyear=2100

# Run the model -----------------------------------------------------------
policyyear <- 2027
v.affected_ages <- c(0:99) # affects all ages
v.years_forfigs=c(2023:2100)
# Policy effect sizes:
# Apelberg (2018): Experts estimate 50% (10-85%) decrease in smoking initiation https://doi.org/10.1056/NEJMsr1714617
# Hatsukami (2024): Significantly higher 12-week CO-verified abstinence among those in VLNC vs NNC condition (OR=3.10, 95%: 1.69-5.96) https://doi.org/10.1016/j.lana.2024.100796

#updated policy effects code:
#apply_policy <- function(rr.init_1,rr.init_s, cess_indicator,rr.cess_1,rr.cess_s, p.CO.CE_1,p.CO.CE_s,p.CO.FE_1,p.CO.FE_s,p.NO.NE_1,p.NO.NE_s, policyyear, v.affected_ages) {
  

params <- list(
  baseline = c(1, 1,"MDSE",1, 1, 1, 1,1, 1,1, 1),
  init_0.1_cess_0.69 = c(1-0.1,1-0.1,"MDSE", 1.69,1.69,  0.9, 0.82, 0.22,0.25, 0.1*0.72,0.1*0.75), #worst case
  init_0.5_cess_2.10 = c(1-0.5,1-0.5,"MDSE", 3.10,3.10, 0.61, 0.51, 0.56,0.58, 0.5*0.5,0.5*0.5), #expected
  init_0.85_cess_4.96 = c(1-0.85,1-0.85,"MDSE", 5.96,5.96, 0.25, 0.19, 0.84, 0.85, 0.85*0.21,0.85*0.2), #best case
  # init_0.1_cess_0.69 = c(1-0.1,1-0.1,"MDSE", 1,1,  1,1,1,1, 1,1), #worst case
  # init_0.5_cess_2.10 = c(1-0.5,1-0.5,"MDSE", 1,1, 1,1,1,1, 1,1), #expected
  # init_0.85_cess_4.96 = c(1-0.85,1-0.85,"MDSE", 1,1, 1,1,1,1, 1,1), #best case
  #FDA
  FDA_est = c(1-0.63,1-0.65,"FDA",0.36,0.34, 0.61, 0.51, 0.56,0.58, 0.63*0.5, 0.65*0.5), #expected
  #initiation (1st,subsequent): -0.63, -0.65
  #cessation:0.36,0.34
  #dual: 0.61, 0.51
  #switching:0.56,0.58
  #vape init:0.5, 0.5
  FDA_best = c(1-0.83,1-0.85,"FDA", 0.61,0.56,0.25, 0.19, 0.84, 0.85, 0.85*0.21,0.85*0.2), #best
  #initiation (1st,subsequent): -0.83, -0.85
  #cessation:0.61,0.56
  #dual:0.25, 0.19
  #switching:0.84, 0.85
  #vape init: 0.21, 0.2
  FDA_worst = c(1-0.38,1-0.39,"FDA", 0.11, 0.11, 0.9, 0.82, 0.22,0.25, 0.38*0.72,0.39*0.75) #worse case
  #initiation (1st,subsequent): -0.38, -0.39
  #cessation:0.11, 0.11
  #dual:0.9, 0.82
  #switching:0.22,0.25
  #vape init: 0.72, 0.75
)

scenarios <- names(params)

run_policy <- function(policy) {
  cat(paste0("\n  Scenario: ", policy))
  l.policy_effects <- apply_policy(params[[policy]][1], params[[policy]][2],params[[policy]][3], params[[policy]][4],
                                   params[[policy]][5], params[[policy]][6],params[[policy]][7], params[[policy]][8],
                                   params[[policy]][9], params[[policy]][10],params[[policy]][11], 
                                   policyyear, v.affected_ages)
  output <- main(v.params, l.policy_effects)
  return(output)
}



#Run all scenarios and save results
allresults <- lapply(scenarios, run_policy)
#
names(allresults) <- scenarios
save(allresults, file = paste0("output/rnc_",whichgender,"_",n.i,"_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".Rda"))



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
      year = as.numeric(names(policy$v.lifeyears[paste0(policyyear:max(cohorts))])),
      scenario = name,
      aLY = policy$v.lifeyears[paste0(policyyear:max(cohorts))],
      cLY = cumsum(policy$v.lifeyears[paste0(policyyear:max(cohorts))]),
      aM_perLY= policy$v.deathrate[paste0(policyyear:max(cohorts))],
      cM_perLY= cumsum(policy$v.deathrate[paste0(policyyear:max(cohorts))]),
      aSAD = policy$v.SAD[paste0(policyyear:max(cohorts))],
      cSAD = cumsum(policy$v.SAD[paste0(policyyear:max(cohorts))]),
      aYLL= policy$v.yll[paste0(policyyear:max(cohorts))],
      cYLL = cumsum(policy$v.yll[paste0(policyyear:max(cohorts))]),
      aCosts = cuw[paste0(policyyear:max(cohorts)), 1], #medical costs
      aQALYs = cuw[paste0(policyyear:max(cohorts)), 2],
      aProd = cuw[paste0(policyyear:max(cohorts)), 3],
      aNonhealth = cuw[paste0(policyyear:max(cohorts)), 4],
      cCosts = cumsum(cuw[paste0(policyyear:max(cohorts)), 1]),
      cQALYs = cumsum(cuw[paste0(policyyear:max(cohorts)), 2]),
      cProd = cumsum(cuw[paste0(policyyear:max(cohorts)), 3]),
      cNonhealth = cumsum(cuw[paste0(policyyear:max(cohorts)), 4]),
      aSocCosts = cuw[paste0(policyyear:max(cohorts)), 5],
      cSocCosts = cuw[paste0(policyyear:max(cohorts)), 5]
    )
  }) %>%
    bind_rows()
  
  # Ensure numeric columns are indeed numeric
  combined_data <- combined_data %>%
    mutate(across(c(year, aLY, cLY, aSAD, cSAD,aYLL,cYLL,cM_perLY,aM_perLY,
                    aCosts,cCosts,aQALYs,aNonhealth, cQALYs,aProd,cProd,cNonhealth,aSocCosts,cSocCosts), as.numeric))
  
  # Extract baseline values
  baseline_values <- combined_data %>%
    filter(scenario == "baseline") %>%
    select(year, aLY, cLY, cSAD, aSAD,cYLL, aYLL,cM_perLY,aM_perLY,aCosts,cCosts,aQALYs,cQALYs,aProd,cProd,aNonhealth,cNonhealth,aSocCosts) %>%
    rename(
      baseline_cLY = cLY,
      baseline_aLY = aLY,
      baseline_cM_perLY = cM_perLY,
      baseline_aM_perLY = aM_perLY,
      baseline_cSAD = cSAD, 
      baseline_aSAD = aSAD,
      baseline_cYLL = cYLL, 
      baseline_aYLL = aYLL,
      baseline_aCosts = aCosts,
      baseline_aQALYs = aQALYs,
      baseline_aProd = aProd,
      baseline_cCosts = cCosts,
      baseline_aSocCosts = aSocCosts,
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
      cYLL_averted_LYG = baseline_cYLL - cYLL,
      aYLL_averted_LYG = baseline_aYLL - aYLL,
      cLYG = cLY - baseline_cLY,
      aLYG = aLY - baseline_aLY,
      #Costs needed for figure here:
      daCosts = aCosts - baseline_aCosts,
      daSocCosts = aSocCosts - baseline_aSocCosts,
      daProd = aProd - baseline_aProd,
      ###
      dCosts = cCosts - baseline_cCosts,
      dCosts = cNonhealth - baseline_cNonhealth,
      dQALYs = cQALYs - baseline_cQALYs,
      dProd = cProd - baseline_cProd,
      # ###ICER for costs figure
      # ICER_medcosts= daCosts/aLYG,
      # ICER_soccosts= daSocCosts/aLYG,
      # ICER_prod= daProd/aLYG,
      # ### US costs for costs figure
      # US_medcosts= ICER_medcosts*aYLL_averted_LYG,
      # US_soccosts= ICER_soccosts*aYLL_averted_LYG,
      # US_prod= ICER_prod*aYLL_averted_LYG,
      
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
               avg_LYs = policy$v.cea["avg_LYs"],
               avg_YLL = mean(policy$v.yll),
               cum_YLL = sum(policy$v.yll), 
               avg_SAD = mean(policy$v.SAD),
               cum_SAD = sum(policy$v.SAD)
               )
               
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
      USSAD_avg <- (cea_data[1, "avg_SAD"] - cea_data[i, "avg_SAD"])
      USSAD_cum <- (cea_data[1, "cum_SAD"] - cea_data[i, "cum_SAD"])
      USLYG_avg <- (cea_data[1, "avg_YLL"] - cea_data[i, "avg_YLL"])
      USLYG_cum <- (cea_data[1, "cum_YLL"] - cea_data[i, "cum_YLL"])
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
      cea_data[i, "icer_consLY"] <- round(inc_cons_exp / inc_effectLY,0)
      cea_data[i, "icer_prodLY"] <- round(inc_prod / inc_effectLY,0)
      cea_data[i, "icer_prodQALY"] <- round(inc_prod / inc_effectQALY,0)
      cea_data[i, "US_LYG_avg"] <- USLYG_avg
      cea_data[i, "US_LYG_cum"] <- USLYG_cum
      cea_data[i, "US_SAD_avert"] <- USSAD_cum
    }
  } 
  return(cea_data)
}

