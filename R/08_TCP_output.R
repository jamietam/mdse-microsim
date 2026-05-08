rm(list = ls())

mainDir <- "/Users/srs475/Library/CloudStorage/Dropbox-UniversityofMichigan/Sarah Skolnick/GitHub/mdse-microsim/"
setwd(mainDir)

hpc        <- 0
n.i        <- 20000
policyyearinput <- 2027
policyyear <- 2027
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
create_econ_csv <- function(datafM, datafF, datafM_icer, datafF_icer, policy_scenario_name, output_file) {
  
  nc <- ncol(datafM)
  
  # Sum male + female cost columns (columns 6:nc), matching the correct approach
  econ_both <- datafM[datafM$scenario == policy_scenario_name, 6:nc] +
    datafF[datafF$scenario == policy_scenario_name, 6:nc]
  econ_both <- cbind(datafM[datafM$scenario == policy_scenario_name, 1:2], econ_both)
  econ_both$gender <- "both"
  
  econ_M <- datafM[datafM$scenario == policy_scenario_name, ]
  econ_M$gender <- "males"
  
  econ_F <- datafF[datafF$scenario == policy_scenario_name, ]
  econ_F$gender <- "females"
  
  # Sum ICER columns (columns 2:nc of [[4]])
  # --- ICER data by gender ---
  nc_icer <- ncol(datafM_icer)
  
  # Both: sum components then recalculate ratios
  icer_both <- datafM_icer[datafM_icer$scenario == policy_scenario_name, 2:nc_icer] +
    datafF_icer[datafF_icer$scenario == policy_scenario_name, 2:nc_icer]
  icer_both$scenario <- policy_scenario_name
  icer_both$gender   <- "both"
  
  # Males and females: just tag gender
  icer_M <- datafM_icer[datafM_icer$scenario == policy_scenario_name, ]
  icer_M$gender <- "males"
  
  icer_F <- datafF_icer[datafF_icer$scenario == policy_scenario_name, ]
  icer_F$gender <- "females"
  
  
  # Recalculate ICERs on summed values
  recalc_icers <- function(df) {
    df$icer_medQALY  <- round(df$inc_med_cost / df$inc_effectQALY, 0)
    df$icer_socQALY  <- round(df$inc_soc_cost / df$inc_effectQALY, 0)
    df$icer_medLY    <- round(df$inc_med_cost / df$inc_effectLY,   0)
    df$icer_socLY    <- round(df$inc_soc_cost / df$inc_effectLY,   0)
    df$icer_prodLY   <- round(df$inc_prod     / df$inc_effectLY,   0)
    df$icer_prodQALY <- round(df$inc_prod     / df$inc_effectQALY, 0)
    df$icer_consLY   <- round(df$inc_cons     / df$inc_effectLY,   0)
    df$icer_consQALY <- round(df$inc_cons     / df$inc_effectQALY, 0)
    return(df)
  }
  
  icer_both <- recalc_icers(icer_both)
  icer_M    <- recalc_icers(icer_M)
  icer_F    <- recalc_icers(icer_F)
  
  # --- Combine econ and icer across genders ---
  econ_all <- bind_rows(econ_both, econ_M, econ_F)
  icer_all <- bind_rows(icer_both, icer_M, icer_F)
  
  # --- Merge and calculate final cost columns ---
  COSTS <- merge(econ_all, icer_all, by = c("scenario", "gender"))
  
  COSTS$healthcare_both   <- round(COSTS$icer_medLY  * COSTS$cLYG_new_disc  , 1)
  COSTS$overall_both      <- round(COSTS$icer_socLY  * COSTS$cLYG_new_disc , 1)
  COSTS$productivity_both <- round(COSTS$icer_prodLY * COSTS$cLYG_new_disc , 1)
  COSTS$consumer_both     <- round(COSTS$icer_consLY * COSTS$cLYG_new_disc , 1)
  
  
  out <- COSTS %>%
    select(year, gender,
           healthcare   = icer_medLY,   # placeholder — overwritten below
           overall      = icer_socLY,
           productivity = icer_prodLY,
           consumer     = icer_consLY) %>%
    # Actually select the computed dollar columns
    { COSTS %>% select(year, gender,
                       healthcare   = healthcare_both,
                       overall      = overall_both,
                       productivity = productivity_both,
                       consumer     = consumer_both) } %>%
    pivot_wider(names_from = gender,
                values_from = c(healthcare, overall, productivity, consumer),
                names_glue = "{.value}_{gender}") %>%
    select(year,
           healthcare_males,   healthcare_females,   healthcare_both,
           overall_males,      overall_females,      overall_both,
           productivity_males, productivity_females, productivity_both,
           consumer_males,     consumer_females,     consumer_both) %>%
    mutate(cohort = "All", policy_year = policyyear)
  
  
  # Write each component to its own sheet or as separate CSVs
  write.csv(out, output_file, row.names = FALSE)
  return(out)
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
    create_econ_csv(M[[3]], F[[3]], M[[4]], F[[4]], scen, paste0(out, "economic/econ_", lbl, "_", slbl, ".csv"))
  }
}

datafM=M[[3]]
datafF=F[[3]]
datafM_icer=M[[4]]
datafF_icer=F[[4]]
policy_scenario_name="main"
output_file

