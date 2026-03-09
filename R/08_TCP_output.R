rm(list = ls())

mainDir <- "/Users/srs475/Library/CloudStorage/Dropbox-UniversityofMichigan/Sarah Skolnick/GitHub/mdse-microsim/"
setwd(mainDir)

hpc        <- 0
n.i        <- 20000
policyyearinput <- 2031
policyyear <- 2030
v.affected_ages <- c(0:99)
d.c <- d.u <- d.w <- 0.03
d.year <- 2025
args   <- c("females", n.i, 2100)

source(paste0(mainDir, "R/01_environment.R"),   echo = FALSE)
source(paste0(mainDir, "R/02_model_inputs.R"),  echo = FALSE)
source(paste0(mainDir, "R/03_model_functions.R"), echo = FALSE)

# Load and format results -------------------------------------------------

load_and_format <- function(file) {
  load(paste0(mainDir, "output/", file), envir = e <- new.env())
  list(
    all = reformat_model_outputs(e$l.results),
    D   = reformat_model_outputs(e$l.results_D),
    ND  = reformat_model_outputs(e$l.results_ND)
  )
}

female <- load_and_format(paste0("combined_", policyyearinput, "_female20000.RData"))
male   <- load_and_format(paste0("combined_", policyyearinput, "_male20000.RData"))

# Create output folders ---------------------------------------------------

subfolders <- c("deaths_averted", "dual_use_prevalence", "economic",
                "lys_gained", "smoking_prevalence", "vaping_prevalence")
base_out <- paste0(mainDir, "output/TCPoutput/policy_", policyyear)

for (folder in c("", subfolders)) {
  dir.create(file.path(base_out, folder), recursive = TRUE, showWarnings = FALSE)
}

# Functions ---------------------------------------------------------------

library(dplyr)
library(tidyr)

create_comparison_csv <- function(datafM, datafF, policy_scenario_name, status_filter, output_file = NULL) {
  
  process_gender <- function(data, prefix) {
    data %>%
      filter(status == status_filter,
             scenario %in% c("baseline", policy_scenario_name)) %>%
      select(age, year, scenario, prev) %>%
      pivot_wider(names_from = scenario, values_from = prev, names_prefix = paste0(prefix, "_")) %>%
      rename(!!paste0(prefix, "_policy") := !!paste0(prefix, "_", policy_scenario_name)) %>%
      mutate(!!paste0(prefix, "_diff") := !!sym(paste0(prefix, "_baseline")) - !!sym(paste0(prefix, "_policy")))
  }
  
  combined <- process_gender(datafM, "males") %>%
    full_join(process_gender(datafF, "females"), by = c("age", "year")) %>%
    mutate(
      both_baseline = (males_baseline + females_baseline) / 2,
      both_policy   = (males_policy   + females_policy)   / 2,
      both_diff     = both_baseline - both_policy
    ) %>%
    select(age, year,
           males_baseline, males_policy, males_diff,
           females_baseline, females_policy, females_diff,
           both_baseline, both_policy, both_diff) %>%
    mutate(across(where(is.numeric) & !c(age, year), ~signif(., 5))) %>%
    mutate(cohort = "All", policy_year = policyyear)
  
  if (!is.null(output_file)) write.csv(combined, output_file, row.names = FALSE)
  return(combined)
}

create_outcome_csv <- function(datafM, datafF, outcome_vars, outcome_var_names, policy_scenario_name, output_file) {
  
  # Accepts single or multiple outcome vars (merges create_outcome_csv + create_costoutcome_csv)
  outcome_vars      <- as.character(outcome_vars)
  outcome_var_names <- as.character(outcome_var_names)
  
  combined_outcome <- NULL
  for (i in seq_along(outcome_vars)) {
    ov  <- outcome_vars[i]
    ovn <- outcome_var_names[i]
    
    temp <- datafM %>% filter(scenario == policy_scenario_name) %>% select(year, !!sym(ov)) %>%
      rename(!!paste0(ovn, "_males") := !!sym(ov)) %>%
      full_join(
        datafF %>% filter(scenario == policy_scenario_name) %>% select(year, !!sym(ov)) %>%
          rename(!!paste0(ovn, "_females") := !!sym(ov)),
        by = "year"
      ) %>%
      mutate(!!paste0(ovn, "_both") :=
               (!!sym(paste0(ovn, "_males")) + !!sym(paste0(ovn, "_females"))) )
    
    combined_outcome <- if (is.null(combined_outcome)) temp else full_join(combined_outcome, temp, by = "year")
  }
  
  combined_outcome <- combined_outcome %>% mutate(cohort = "All", policy_year = policyyear)
  write.csv(combined_outcome, output_file, row.names = FALSE)
  return(combined_outcome)
}

# Create files ------------------------------------------------------------

scenarios  <- c("main", "best", "worst")
scen_label <- c("real", "opt",  "pess")
dep_groups <- list(
  list(label = "overall", M = male$all, F = female$all),
  list(label = "dep",     M = male$D,   F = female$D),
  list(label = "nodep",   M = male$ND,  F = female$ND)
)
econ_vars  <- c("cMedCosts", "cSocCosts", "cProd", "cNonhealth")
econ_names <- c("healthcare", "overall", "productivity", "consumer")

for (i in seq_along(scenarios)) {
  scen <- scenarios[i]; slbl <- scen_label[i]
  
  for (grp in dep_groups) {
    lbl <- grp$label; M <- grp$M; F <- grp$F
    out <- paste0(base_out, "/")
    
    # Prevalences
    create_comparison_csv(M[[1]], F[[1]], scen, "C",  paste0(out, "smoking_prevalence/sp_", lbl, "_", slbl, ".csv"))
    create_comparison_csv(M[[1]], F[[1]], scen, "E",  paste0(out, "vaping_prevalence/vp_",  lbl, "_", slbl, ".csv"))
    create_comparison_csv(M[[1]], F[[1]], scen, "CE", paste0(out, "dual_use_prevalence/dup_", lbl, "_", slbl, ".csv"))
    
    # LYG (discounted and undiscounted)
    create_outcome_csv(M[[3]], F[[3]], "cLYG_new_disc", "cLYG", scen, paste0(out, "lys_gained/lyg_", lbl, "_", slbl, "_disc.csv"))
    create_outcome_csv(M[[3]], F[[3]], "cLYG_new",      "cLYG", scen, paste0(out, "lys_gained/lyg_", lbl, "_", slbl, ".csv"))
    
    # Deaths averted (discounted and undiscounted)
    create_outcome_csv(M[[3]], F[[3]], "cSAD_averted_new_disc", "deaths_avoided", scen, paste0(out, "deaths_averted/da_", lbl, "_", slbl, "_disc.csv"))
    create_outcome_csv(M[[3]], F[[3]], "cSAD_averted_new",      "deaths_avoided", scen, paste0(out, "deaths_averted/da_", lbl, "_", slbl, ".csv"))
    
    # Economic
    create_outcome_csv(M[[3]], F[[3]], econ_vars, econ_names, scen, paste0(out, "economic/econ_", lbl, "_", slbl, ".csv"))
  }
}