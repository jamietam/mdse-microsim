rm(list = ls()) 

mainDir = "/Users/srs475/Library/CloudStorage/Dropbox-UniversityofMichigan/Sarah Skolnick/GitHub/mdse-microsim/"
# mainDir = "/Users/jt936/Dropbox/GitHub/mdse-microsim/"
# mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" 
setwd(mainDir)

hpc = 0
n.i <- 20000 # number of people per birth cohort
policyyear <- 2030
v.affected_ages <- c(0:99) # affects all ages
d.c <- d.u <- d.w <- 0.03              # equal discounting of costs and QALYs by 3%
d.year <- 2025 # which year to start discounting from
args <- c("females",n.i, 2100) #no need to change this for now

source(paste0(mainDir,"R/01_environment.R"), echo=FALSE) #
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE)


malefile= paste0("combined_",policyyear,"_male20000.RData")
femalefile=paste0("combined_",policyyear,"_female20000.RData")

# Load and format files for females
load(paste0(mainDir, "output/",femalefile))
dfF=reformat_model_outputs(l.results)
dfF_D=reformat_model_outputs(l.results_D)
dfF_ND=reformat_model_outputs(l.results_ND)

# Load and format files for males
load(paste0(mainDir, "output/",malefile))  
dfM=reformat_model_outputs(l.results)
dfM_D=reformat_model_outputs(l.results_D)
dfM_ND=reformat_model_outputs(l.results_ND)




# Specify the policy year
  # Change this to whatever year you need

# Create the main policy year folder within TCPoutput
dir.create(file.path(paste0(mainDir,"output/TCPoutput"), paste0("policy_", policyyear)), 
           recursive = TRUE, 
           showWarnings = FALSE)

# Define the subfolder names from your image
subfolders <- c(
  "deaths_averted",
  "dual_use_prevalence",
  "economic",
  "lys_gained",
  "smoking_prevalence",
  "vaping_prevalence"
)

# Create each subfolder within the policy year folder
for (folder in subfolders) {
  dir.create(file.path("output/TCPoutput", paste0("policy_", policyyear), folder), 
             recursive = TRUE, 
             showWarnings = FALSE)
}

# Confirmation message
cat("Created TCPoutput/policy_", policyyear, " folder with subfolders:\n", sep = "")
cat(paste("  -", subfolders), sep = "\n")

# Functions ---------------------------------------------------------------

library(dplyr)
library(tidyr)
#test case
# datafM=dfM[[1]]
# datafF=dfF[[1]]
# policy_scenario_name="worst"
# status_filter="C"
# table(datafM$scenario)
# table(datafM$status)

create_comparison_csv <- function(datafM, datafF, policy_scenario_name, status_filter, output_file = NULL) {
  
  # Process male data
  males <- datafM %>%
    filter(status == status_filter) %>%
    filter(scenario %in% c("baseline", policy_scenario_name)) %>%
    select(age, year, scenario, prev) %>%
    pivot_wider(
      names_from = scenario,
      values_from = prev,
      names_prefix = "males_"
    )  %>%
    rename(males_policy = !!paste0("males_", policy_scenario_name)) %>%
    mutate(males_diff = males_policy - males_baseline)
  
  # Process female data
  females <- datafF %>%
    filter(status == status_filter) %>%
    filter(scenario %in% c("baseline", policy_scenario_name)) %>%
    select(age, year, scenario, prev) %>%
    pivot_wider(
      names_from = scenario,
      values_from = prev,
      names_prefix = "females_"
    ) %>%
    rename(females_policy = !!paste0("females_", policy_scenario_name))%>%
    mutate(females_diff = females_policy - females_baseline) 
  
  # Combine male and female data
  combined <- males %>%
    full_join(females, by = c("age", "year")) %>%
    mutate(
      both_baseline = (males_baseline + females_baseline) / 2,
      both_policy = (males_policy + females_policy) / 2,
      both_diff = both_policy - both_baseline
    ) %>%
    select(
      age, year,
      males_baseline, males_policy, males_diff,
      females_baseline, females_policy, females_diff,
      both_baseline, both_policy, both_diff
    )%>%
    mutate(across(where(is.numeric) & !c(age, year), ~signif(., 5)))
  
  combined$cohort<-"All"
  combined$policy_year<-2027
  # Write to CSV if filename provided
  if (!is.null(output_file)) {
    write.csv(combined, output_file, row.names = FALSE)
  }
  
  return(combined)
}

#test case
# datafM=dfM[[3]]
# datafF=dfF[[3]]
# outcome_var="cLYG_new_disc"
# outcome_var_name="cLYG"
# policy_scenario_name="worst"
# output_file="boop"

create_outcome_csv <- function(datafM, datafF, outcome_var,outcome_var_name,policy_scenario_name, output_file) {
  
  # Extract outcome from males
  males_outcome <- datafM %>%
    filter(scenario %in% c(policy_scenario_name)) %>%
    select(year, !!sym(outcome_var)) %>%
    rename(!!paste0(outcome_var_name, "_males") := !!sym(outcome_var))
  
  # Extract outcome from females
  females_outcome <- datafF %>%
    filter(scenario %in% c(policy_scenario_name)) %>%
    select(year, !!sym(outcome_var)) %>%
    rename(!!paste0(outcome_var_name, "_females") := !!sym(outcome_var))
  
  # Combine and calculate both
  combined_outcome <- males_outcome %>%
    full_join(females_outcome, by = c("year")) %>%
    mutate(!!paste0(outcome_var_name, "_both") := (!!sym(paste0(outcome_var_name, "_males")) + !!sym(paste0(outcome_var_name, "_females"))) / 2) %>%
    select( year, 
           !!sym(paste0(outcome_var_name, "_males")), 
           !!sym(paste0(outcome_var_name, "_females")), 
           !!sym(paste0(outcome_var_name, "_both")))
  combined_outcome$cohort<-"All"
  combined_outcome$age<-18.99
  combined_outcome$policy_year<-2027
  
  
  # Write to CSV
  write.csv(combined_outcome, output_file, row.names = FALSE)
  
  return(combined_outcome)
}


create_costoutcome_csv <- function(datafM, datafF, outcome_vars, outcome_var_names, policy_scenario_name, output_file) {
  
  # Initialize with year column
  combined_outcome <- NULL
  
  # Loop through each outcome variable
  for (i in seq_along(outcome_vars)) {
    outcome_var <- outcome_vars[i]
    outcome_var_name <- outcome_var_names[i]
    
    # Extract outcome from males
    males_outcome <- datafM %>%
      filter(scenario %in% c(policy_scenario_name)) %>%
      select(year, !!sym(outcome_var)) %>%
      rename(!!paste0(outcome_var_name, "_males") := !!sym(outcome_var))
    
    # Extract outcome from females
    females_outcome <- datafF %>%
      filter(scenario %in% c(policy_scenario_name)) %>%
      select(year, !!sym(outcome_var)) %>%
      rename(!!paste0(outcome_var_name, "_females") := !!sym(outcome_var))
    
    # Combine and calculate both for this outcome
    temp_outcome <- males_outcome %>%
      full_join(females_outcome, by = c("year")) %>%
      mutate(!!paste0(outcome_var_name, "_both") := (!!sym(paste0(outcome_var_name, "_males")) + !!sym(paste0(outcome_var_name, "_females"))) / 2)
    
    # Join with main dataframe
    if (is.null(combined_outcome)) {
      combined_outcome <- temp_outcome
    } else {
      combined_outcome <- combined_outcome %>%
        full_join(temp_outcome, by = "year")
    }
  }
  
  # Add additional columns

  combined_outcome$policy_year <- 2027
  
  # Write to CSV
  write.csv(combined_outcome, output_file, row.names = FALSE)
  
  return(combined_outcome)
}


# create files ------------------------------------------------------------

#prevalences
setwd(paste0(mainDir,"output/TCPoutput/policy_",policyyear,"/smoking_prevalence"))
create_comparison_csv(dfM[[1]], dfF[[1]], "main", "C", output_file = "sp_overall_real.csv")
create_comparison_csv(dfM[[1]], dfF[[1]], "best", "C", output_file = "sp_overall_opt.csv")
create_comparison_csv(dfM[[1]], dfF[[1]], "worst", "C", output_file = "sp_overall_pess.csv")
create_comparison_csv(dfM_D[[1]], dfF_D[[1]], "main", "C", output_file = "sp_dep_real.csv")
create_comparison_csv(dfM_D[[1]], dfF_D[[1]], "best", "C", output_file = "sp_dep_opt.csv")
create_comparison_csv(dfM_D[[1]], dfF_D[[1]], "worst", "C", output_file = "sp_dep_pess.csv")
create_comparison_csv(dfM_ND[[1]], dfF_ND[[1]], "main", "C", output_file = "sp_nodep_real.csv")
create_comparison_csv(dfM_ND[[1]], dfF_ND[[1]], "best", "C", output_file = "sp_nodep_opt.csv")
create_comparison_csv(dfM_ND[[1]], dfF_ND[[1]], "worst", "C", output_file = "sp_nodep_pess.csv")

setwd(paste0(mainDir,"output/TCPoutput/policy_",policyyear,"/vaping_prevalence"))
create_comparison_csv(dfM[[1]], dfF[[1]], "main", "E", output_file = "vp_overall_real.csv")
create_comparison_csv(dfM[[1]], dfF[[1]], "best", "E", output_file = "vp_overall_opt.csv")
create_comparison_csv(dfM[[1]], dfF[[1]], "worst", "E", output_file = "vp_overall_pess.csv")
create_comparison_csv(dfM_D[[1]], dfF_D[[1]], "main", "E", output_file = "vp_dep_real.csv")
create_comparison_csv(dfM_D[[1]], dfF_D[[1]], "best", "E", output_file = "vp_dep_opt.csv")
create_comparison_csv(dfM_D[[1]], dfF_D[[1]], "worst", "E", output_file = "vp_dep_pess.csv")
create_comparison_csv(dfM_ND[[1]], dfF_ND[[1]], "main", "E", output_file = "vp_nodep_real.csv")
create_comparison_csv(dfM_ND[[1]], dfF_ND[[1]], "best", "E", output_file = "vp_nodep_opt.csv")
create_comparison_csv(dfM_ND[[1]], dfF_ND[[1]], "worst", "E", output_file = "vp_nodep_pess.csv")

setwd(paste0(mainDir,"output/TCPoutput/policy_",policyyear,"/dual_use_prevalence"))
create_comparison_csv(dfM[[1]], dfF[[1]], "main", "CE", output_file = "dup_overall_real.csv")
create_comparison_csv(dfM[[1]], dfF[[1]], "best", "CE", output_file = "dup_overall_opt.csv")
create_comparison_csv(dfM[[1]], dfF[[1]], "worst", "CE", output_file = "dup_overall_pess.csv")
create_comparison_csv(dfM_D[[1]], dfF_D[[1]], "main", "CE", output_file = "dup_dep_real.csv")
create_comparison_csv(dfM_D[[1]], dfF_D[[1]], "best", "CE", output_file = "dup_dep_opt.csv")
create_comparison_csv(dfM_D[[1]], dfF_D[[1]], "worst", "CE", output_file = "dup_dep_pess.csv")
create_comparison_csv(dfM_ND[[1]], dfF_ND[[1]], "main", "CE", output_file = "dup_nodep_real.csv")
create_comparison_csv(dfM_ND[[1]], dfF_ND[[1]], "best", "CE", output_file = "dup_nodep_opt.csv")
create_comparison_csv(dfM_ND[[1]], dfF_ND[[1]], "worst", "CE", output_file = "dup_nodep_pess.csv")

#LYG
setwd(paste0(mainDir,"output/TCPoutput/policy_",policyyear,"/lys_gained"))
create_outcome_csv(dfM[[3]], dfF[[3]], "cLYG_new_disc", "cLYG", "main", output_file="lyg_overall_real_disc.csv")
create_outcome_csv(dfM[[3]], dfF[[3]], "cLYG_new_disc", "cLYG", "best", output_file="lyg_overall_opt_disc.csv")
create_outcome_csv(dfM[[3]], dfF[[3]], "cLYG_new_disc", "cLYG", "worst", output_file="lyg_overall_pess_disc.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cLYG_new_disc", "cLYG", "main", output_file="lyg_dep_real_disc.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cLYG_new_disc", "cLYG", "best", output_file="lyg_dep_opt_disc.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cLYG_new_disc", "cLYG", "worst", output_file="lyg_dep_pess_disc.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cLYG_new_disc", "cLYG", "main", output_file="lyg_nodep_real_disc.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cLYG_new_disc", "cLYG", "best", output_file="lyg_nodep_opt_disc.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cLYG_new_disc", "cLYG", "worst", output_file="lyg_nodep_pess_disc.csv")

create_outcome_csv(dfM[[3]], dfF[[3]], "cLYG_new", "cLYG", "main", output_file="lyg_overall_real.csv")
create_outcome_csv(dfM[[3]], dfF[[3]], "cLYG_new", "cLYG", "best", output_file="lyg_overall_opt.csv")
create_outcome_csv(dfM[[3]], dfF[[3]], "cLYG_new", "cLYG", "worst", output_file="lyg_overall_pess.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cLYG_new", "cLYG", "main", output_file="lyg_dep_real.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cLYG_new", "cLYG", "best", output_file="lyg_dep_opt.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cLYG_new", "cLYG", "worst", output_file="lyg_dep_pess.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cLYG_new", "cLYG", "main", output_file="lyg_nodep_real.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cLYG_new", "cLYG", "best", output_file="lyg_nodep_opt.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cLYG_new", "cLYG", "worst", output_file="lyg_nodep_pess.csv")


#deaths averted
setwd(paste0(mainDir,"output/TCPoutput/policy_",policyyear,"/deaths_averted"))
create_outcome_csv(dfM[[3]], dfF[[3]], "cSAD_averted_new_disc", "deaths_avoided", "main", output_file="da_overall_real_disc.csv")
create_outcome_csv(dfM[[3]], dfF[[3]], "cSAD_averted_new_disc", "deaths_avoided", "best", output_file="da_overall_opt_disc.csv")
create_outcome_csv(dfM[[3]], dfF[[3]], "cSAD_averted_new_disc", "deaths_avoided", "worst", output_file="da_overall_pess_disc.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cSAD_averted_new_disc", "deaths_avoided", "main", output_file="da_dep_real_disc.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cSAD_averted_new_disc", "deaths_avoided", "best", output_file="da_dep_opt_disc.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cSAD_averted_new_disc", "deaths_avoided", "worst", output_file="da_dep_pess_disc.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cSAD_averted_new_disc", "deaths_avoided", "main", output_file="da_nodep_real_disc.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cSAD_averted_new_disc", "deaths_avoided", "best", output_file="da_nodep_opt_disc.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cSAD_averted_new_disc", "deaths_avoided", "worst", output_file="da_nodep_pess_disc.csv")

create_outcome_csv(dfM[[3]], dfF[[3]], "cSAD_averted_new", "deaths_avoided", "main", output_file="da_overall_real.csv")
create_outcome_csv(dfM[[3]], dfF[[3]], "cSAD_averted_new", "deaths_avoided", "best", output_file="da_overall_opt.csv")
create_outcome_csv(dfM[[3]], dfF[[3]], "cSAD_averted_new", "deaths_avoided", "worst", output_file="da_overall_pess.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cSAD_averted_new", "deaths_avoided", "main", output_file="da_dep_real.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cSAD_averted_new", "deaths_avoided", "best", output_file="da_dep_opt.csv")
create_outcome_csv(dfM_D[[3]], dfF_D[[3]], "cSAD_averted_new", "deaths_avoided", "worst", output_file="da_dep_pess.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cSAD_averted_new", "deaths_avoided", "main", output_file="da_nodep_real.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cSAD_averted_new", "deaths_avoided", "best", output_file="da_nodep_opt.csv")
create_outcome_csv(dfM_ND[[3]], dfF_ND[[3]], "cSAD_averted_new", "deaths_avoided", "worst", output_file="da_nodep_pess.csv")


# cost outcomes
setwd(paste0(mainDir,"output/TCPoutput/policy_",policyyear,"/economic"))
create_costoutcome_csv(dfM[[3]], dfF[[3]],outcome_vars = c("cMedCosts", "cSocCosts", "cProd","cNonhealth"), outcome_var_names = c("healthcare", "overall", "productivity","consumer"),policy_scenario_name = "main", output_file = "econ_overall_real.csv")
create_costoutcome_csv(dfM[[3]], dfF[[3]],outcome_vars = c("cMedCosts", "cSocCosts", "cProd","cNonhealth"), outcome_var_names = c("healthcare", "overall", "productivity","consumer"),policy_scenario_name = "best", output_file = "econ_overall_opt.csv")
create_costoutcome_csv(dfM[[3]], dfF[[3]],outcome_vars = c("cMedCosts", "cSocCosts", "cProd","cNonhealth"), outcome_var_names = c("healthcare", "overall", "productivity","consumer"),policy_scenario_name = "worst", output_file = "econ_overall_pess.csv")
create_costoutcome_csv(dfM_D[[3]], dfF_D[[3]],outcome_vars = c("cMedCosts", "cSocCosts", "cProd","cNonhealth"), outcome_var_names = c("healthcare", "overall", "productivity","consumer"),policy_scenario_name = "main", output_file = "econ_dep_real.csv")
create_costoutcome_csv(dfM_D[[3]], dfF_D[[3]],outcome_vars = c("cMedCosts", "cSocCosts", "cProd","cNonhealth"), outcome_var_names = c("healthcare", "overall", "productivity","consumer"),policy_scenario_name = "best", output_file = "econ_dep_opt.csv")
create_costoutcome_csv(dfM_D[[3]], dfF_D[[3]],outcome_vars = c("cMedCosts", "cSocCosts", "cProd","cNonhealth"), outcome_var_names = c("healthcare", "overall", "productivity","consumer"),policy_scenario_name = "worst", output_file = "econ_dep_pess.csv")
create_costoutcome_csv(dfM_ND[[3]], dfF_ND[[3]],outcome_vars = c("cMedCosts", "cSocCosts", "cProd","cNonhealth"), outcome_var_names = c("healthcare", "overall", "productivity","consumer"),policy_scenario_name = "main", output_file = "econ_nodep_real.csv")
create_costoutcome_csv(dfM_ND[[3]], dfF_ND[[3]],outcome_vars = c("cMedCosts", "cSocCosts", "cProd","cNonhealth"), outcome_var_names = c("healthcare", "overall", "productivity","consumer"),policy_scenario_name = "best", output_file = "econ_nodep_opt.csv")
create_costoutcome_csv(dfM_ND[[3]], dfF_ND[[3]],outcome_vars = c("cMedCosts", "cSocCosts", "cProd","cNonhealth"), outcome_var_names = c("healthcare", "overall", "productivity","consumer"),policy_scenario_name = "worst", output_file = "econ_nodep_pess.csv")


