# --- TOTAL POPULATION (T) ---
nc = ncol(dfM[[3]])

# --- TOTAL POPULATION (T) ---
Health = dfM[[3]][,6:nc] + dfF[[3]][,6:nc]
Health = cbind(dfM[[3]][,1:2], Health)
Health$population = "T"
Health$gender = "MF"

Health_M = dfM[[3]][,6:nc]
Health_M = cbind(dfM[[3]][,1:2], Health_M)
Health_M$population = "T"
Health_M$gender = "M"

Health_F = dfF[[3]][,6:nc]
Health_F = cbind(dfF[[3]][,1:2], Health_F)
Health_F$population = "T"
Health_F$gender = "F"

# --- DEPRESSED POPULATION (D) ---
Health_D = dfM_D[[3]][,6:nc] + dfF_D[[3]][,6:nc]
Health_D = cbind(dfM_D[[3]][,1:2], Health_D)
Health_D$population = "D"
Health_D$gender = "MF"

Health_D_M = dfM_D[[3]][,6:nc]
Health_D_M = cbind(dfM_D[[3]][,1:2], Health_D_M)
Health_D_M$population = "D"
Health_D_M$gender = "M"

Health_D_F = dfF_D[[3]][,6:nc]
Health_D_F = cbind(dfF_D[[3]][,1:2], Health_D_F)
Health_D_F$population = "D"
Health_D_F$gender = "F"

# --- NON-DEPRESSED POPULATION (ND) ---
Health_ND = dfM_ND[[3]][,6:nc] + dfF_ND[[3]][,6:nc]
Health_ND = cbind(dfM_ND[[3]][,1:2], Health_ND)
Health_ND$population = "ND"
Health_ND$gender = "MF"

Health_ND_M = dfM_ND[[3]][,6:nc]
Health_ND_M = cbind(dfM_ND[[3]][,1:2], Health_ND_M)
Health_ND_M$population = "ND"
Health_ND_M$gender = "M"

Health_ND_F = dfF_ND[[3]][,6:nc]
Health_ND_F = cbind(dfF_ND[[3]][,1:2], Health_ND_F)
Health_ND_F$population = "ND"
Health_ND_F$gender = "F"

# --- COMBINE ---
Health_comb <- rbind(
  Health,   Health_M,   Health_F,
  Health_D, Health_D_M, Health_D_F,
  Health_ND, Health_ND_M, Health_ND_F
)

Health_comb_orig <- Health_comb

nc=ncol(dfM[[4]])
# Combined (M+F)
ICERST = dfM[[4]][,2:nc] + dfF[[4]][,2:nc]
ICERST$scenario = dfM[[4]][,1]
ICERST$population = "T"
ICERST$gender = "MF"
ICERALL = ICERST

# Males only
ICERST_M = dfM[[4]][,2:nc]
ICERST_M$scenario = dfM[[4]][,1]
ICERST_M$population = "T"
ICERST_M$gender = "M"
ICERALL_M = ICERST_M

# Females only
ICERST_F = dfF[[4]][,2:nc]
ICERST_F$scenario = dfF[[4]][,1]
ICERST_F$population = "T"
ICERST_F$gender = "F"
ICERALL_F = ICERST_F

# --- DEPRESSED POPULATION (D) ---
# Combined (M+F)
ICERST_D = dfM_D[[4]][,2:nc] + dfF_D[[4]][,2:nc]
ICERST_D$scenario = dfM_D[[4]][,1]
ICERST_D$population = "D"
ICERST_D$gender = "MF"
ICERALL_D = ICERST_D

# Males only
ICERST_D_M = dfM_D[[4]][,2:nc]
ICERST_D_M$scenario = dfM_D[[4]][,1]
ICERST_D_M$population = "D"
ICERST_D_M$gender = "M"
ICERALL_D_M = ICERST_D_M

# Females only
ICERST_D_F = dfF_D[[4]][,2:nc]
ICERST_D_F$scenario = dfF_D[[4]][,1]
ICERST_D_F$population = "D"
ICERST_D_F$gender = "F"
ICERALL_D_F = ICERST_D_F

# --- NON-DEPRESSED POPULATION (ND) ---
# Combined (M+F)
ICERST_ND = dfM_ND[[4]][,2:nc] + dfF_ND[[4]][,2:nc]
ICERST_ND$scenario = dfM_ND[[4]][,1]
ICERST_ND$population = "ND"
ICERST_ND$gender = "MF"
ICERALL_ND = ICERST_ND

# Males only
ICERST_ND_M = dfM_ND[[4]][,2:nc]
ICERST_ND_M$scenario = dfM_ND[[4]][,1]
ICERST_ND_M$population = "ND"
ICERST_ND_M$gender = "M"
ICERALL_ND_M = ICERST_ND_M

# Females only
ICERST_ND_F = dfF_ND[[4]][,2:nc]
ICERST_ND_F$scenario = dfF_ND[[4]][,1]
ICERST_ND_F$population = "ND"
ICERST_ND_F$gender = "F"
ICERALL_ND_F = ICERST_ND_F

# --- RECALCULATE ICERs ---
recalc_icers <- function(df) {
  df$icer_medQALY  <- round(df$inc_med_cost  / df$inc_effectQALY, 0)
  df$icer_socQALY  <- round(df$inc_soc_cost  / df$inc_effectQALY, 0)
  df$icer_medLY    <- round(df$inc_med_cost  / df$inc_effectLY,   0)
  df$icer_socLY    <- round(df$inc_soc_cost  / df$inc_effectLY,   0)
  df$icer_prodLY   <- round(df$inc_prod      / df$inc_effectLY,   0)
  df$icer_prodQALY <- round(df$inc_prod      / df$inc_effectQALY, 0)
  df$icer_consLY   <- round(df$inc_cons      / df$inc_effectLY,   0)
  df$icer_consQALY <- round(df$inc_cons      / df$inc_effectQALY, 0)
  return(df)
}

ICERALL      <- recalc_icers(ICERALL)
ICERALL_M    <- recalc_icers(ICERALL_M)
ICERALL_F    <- recalc_icers(ICERALL_F)
ICERALL_D    <- recalc_icers(ICERALL_D)
ICERALL_D_M  <- recalc_icers(ICERALL_D_M)
ICERALL_D_F  <- recalc_icers(ICERALL_D_F)
ICERALL_ND   <- recalc_icers(ICERALL_ND)
ICERALL_ND_M <- recalc_icers(ICERALL_ND_M)
ICERALL_ND_F <- recalc_icers(ICERALL_ND_F)

# --- COMBINE ALL ---
ICERALL_combined <- bind_rows(
  ICERALL,   ICERALL_M,   ICERALL_F,
  ICERALL_D, ICERALL_D_M, ICERALL_D_F,
  ICERALL_ND, ICERALL_ND_M, ICERALL_ND_F
)

# --- MERGE AND CALCULATE COSTS ---
COSTS <- merge(Health_comb_orig, ICERALL_combined, by = c("population", "scenario", "gender"))
COSTS$SOC_cost_US_new <- round(COSTS$icer_socLY  * COSTS$cLYG_new_disc / 1000000000, 1)
COSTS$MED_cost_US_new <- round(COSTS$icer_medLY  * COSTS$cLYG_new_disc / 1000000000, 1)
COSTS$Prod_US_new     <- round(COSTS$icer_prodLY * COSTS$cLYG_new_disc / 1000000000, 1)
COSTS$cons_US_new     <- round(COSTS$icer_consLY * COSTS$cLYG_new_disc / 1000000000, 1)

# --- FILTER FOR DEPSENS OUTPUT ---
COSTS_depsens <- COSTS %>%
  filter(scenario %in% c("main"), year == 2100) %>%
  select(scenario, population, gender, Prod_US_new, MED_cost_US_new,cons_US_new)

write.csv(COSTS_depsens, file = "/Users/srs475/Library/CloudStorage/Dropbox-UniversityofMichigan/Sarah Skolnick/GitHub/mdse-microsim/output/TCPoutput/policy_2027_new/COSTS_new.csv", row.names = FALSE)