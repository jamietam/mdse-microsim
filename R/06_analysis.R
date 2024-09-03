## Clean up the workspace and set main working directory
rm(list = ls()) 

mainDir = "C:/Users/jt936/Dropbox/GitHub/mds-microsim/"
# mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" # Set working directory
hpc=0
source(paste0(mainDir,"R/01_environment.R"), echo=FALSE) #
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE) # microsimulation model and probability functions

# Run the model -----------------------------------------------------------

# Define constants and helper functions
policyyear <- 2024
scenarios <- c("baseline", "policy_rnc1", "policy_rnc2")
# scenarios <- c("baseline", "policy_init", "policy_cess", "policy_rnc1", "policy_rnc2")
params <- list(
  baseline = c(1.0, 1.0),
  # policy_init = c(0.8, 1.0),
  # policy_cess = c(1.0, 1.2),
  policy_rnc1 = c(0.5, 6.0),
  policy_rnc2 = c(0.5, 3.95)
)

run_policy <- function(policy) {
  main(v.params, apply_policy(params[[policy]][1], params[[policy]][2], policyyear, c(0:99)))
}

# Run all scenarios and save results
results <- lapply(scenarios, run_policy)
names(results) <- scenarios
save(results, file = paste0("scenarios_",whichgender,"_",n.i,".Rda"))

# Reformat data for data visualization ------------------------------------

# Model prevalence data
create_model_prev <- function(policy) {
  df_list <- list(
    cbind(data.frame(policy$model_res$N), status = "N"),
    cbind(data.frame(policy$model_res$C), status = "C"),
    cbind(data.frame(policy$model_res$F), status = "F"),
    cbind(data.frame(policy$model_res$D), status = "D"),
    cbind(data.frame(policy$model_res$ND), status = "ND"),
    cbind(data.frame(policy$model_res$CD), status = "CD"),
    cbind(data.frame(policy$model_res$FD), status = "FD")
  )
  modelprev <- do.call(rbind, df_list)
  return(modelprev)
}

modelprevs <- do.call(rbind, lapply(names(results), function(name) {
  modelprev <- create_model_prev(results[[name]])
  modelprev$scenario <- name
  return(modelprev)
}))

# NSDUH prevalence data
calibtargets <- do.call(rbind, lapply(names(lst_targets), function(status) {
  cbind(data.frame(lst_targets[[status]]), status = status)
}))

# Smoking initiation and cessation
smkprobs <- do.call(rbind, lapply(names(results), function(name) {
  policy <- results[[name]]
  data.frame(cbind(policy$init[, policyyear - 1899], policy$cess[, policyyear - 1899], 0:99, name))
}))
colnames(smkprobs) <- c("init", "cess", "age", "scenario")
smkprobs[, c("init", "cess", "age")] <- sapply(smkprobs[, c("init", "cess", "age")], as.numeric)

# Mortality (X), life-years (ly), and cost-utility data
combined_data <- lapply(names(results), function(name) {
  policy <- results[[name]]
  cuw <- results[[name]]$cuw
  data.frame(
    year = as.numeric(names(policy$n.X[paste0(policyyear:max(cohorts))])),
    scenario = name,
    aX = policy$n.X[paste0(policyyear:max(cohorts))],
    cX = cumsum(policy$n.X[paste0(policyyear:max(cohorts))]),
    aLY = policy$n.lifeyears[paste0(policyyear:max(cohorts))],
    cLY = cumsum(policy$n.lifeyears[paste0(policyyear:max(cohorts))]),
    aSAD = policy$SAD,
    cSAD = cumsum(policy$SAD),
    aCosts = cuw[paste0(policyyear:max(cohorts)), 1],
    aQALYs = cuw[paste0(policyyear:max(cohorts)), 2],
    aProd = cuw[paste0(policyyear:max(cohorts)), 3],
    aNonhealth = cuw[paste0(policyyear:max(cohorts)), 4],
    cCosts = cumsum(cuw[paste0(policyyear:max(cohorts)), 1]),
    cQALYs = cumsum(cuw[paste0(policyyear:max(cohorts)), 2]),
    cProd = cumsum(cuw[paste0(policyyear:max(cohorts)), 3]),
    cNonhealth = cumsum(cuw[paste0(policyyear:max(cohorts)), 4])
  )
}) %>%
  bind_rows()

# Ensure numeric columns are indeed numeric
combined_data <- combined_data %>%
  mutate(across(c(year, aX, cX, aLY, cLY, aSAD, cSAD,
                  aCosts,cCosts,aQALYs,aNonhealth, cQALYs,aProd,cProd,cNonhealth), as.numeric))

# Extract baseline values
baseline_values <- combined_data %>%
  filter(scenario == "baseline") %>%
  select(year, cX, aX, aLY, cLY, cSAD, aSAD,aCosts,cCosts,aQALYs,cQALYs,aProd,cProd,aNonhealth,cNonhealth) %>%
  rename(
    baseline_cX = cX, 
    baseline_aX = aX, 
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
    cX_averted = baseline_cX - cX,
    aX_averted = baseline_aX - aX,
    cLYG = cLY - baseline_cLY,
    aLYG = aLY - baseline_aLY,
    dCosts = cCosts - baseline_cCosts,
    dCosts = cNonhealth - baseline_cNonhealth,
    dQALYs = cQALYs - baseline_cQALYs,
    dProd = cProd - baseline_cProd
  ) %>%
  select(-baseline_cX, -baseline_aX, -baseline_aLY, -baseline_cLY, -baseline_cSAD, -baseline_aSAD, 
         -baseline_cCosts, -baseline_cQALYs, -baseline_cProd, -baseline_aCosts, -baseline_aQALYs, -baseline_aProd )  # Remove temporary baseline columns

# Calculate ICER
cea_data <- lapply(names(results), function(name) {
  policy <- results[[name]]
  avg_healthcare_costs <- policy$avg_healthcare_costs
  avg_societal_costs <- policy$avg_societal_costs
  avg_QALYs <- policy$avg_QALYs
  data.frame(scenario = name,avg_healthcare_costs,avg_societal_costs,avg_QALYs)
}) %>%
  bind_rows()

df_cea_hc <- calculate_icers(cost    = cea_data$avg_healthcare_costs,
                          effect     = cea_data$avg_QALYs,
                          strategies = cea_data$scenario)

df_cea_sc <- calculate_icers(cost    = cea_data$avg_societal_costs,
                          effect     = cea_data$avg_QALYs,
                          strategies = cea_data$scenario)
plot(df_cea_hc)
plot(df_cea_sc)

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
  geom_pointrange(data = subset(calibtargets, age == 18.99 & (status == "CD" | status == "C")),
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, linetype = status, shape = status)) +
  geom_line(data = subset(modelprevs, age == 18.99 & (status == "CD" | status == "C")),
            aes(x = year, y = prev, color = scenario, linetype = status)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0, 0.5), breaks = seq(0, 0.5, 0.05)) +
  scale_x_continuous(name = "Year", limits = c(2005, 2100), breaks = seq(2005, 2100, 10)) +
  labs(title = "Current smoking - Women ages 18-99") +
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
pdf(file = paste0(mainDir,"output/", "policy_",whichgender,"_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
plot.new()
text(.5, 0.5, paste0("Policy outcomes \n",whichgender,"\n",n.i," per birth cohort"), font=1, cex=1.5)
grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking probabilities")
csprevs
grid_arrange_shared_legend(list(aSAD_averted_fig,cSAD_averted_fig),2,"Smoking-Attributable Deaths")
grid_arrange_shared_legend(list(acosts_fig, ccosts_fig, dcosts_fig),3, "Healthcare costs")
grid_arrange_shared_legend(list(aprod_fig, cprod_fig, dprod_fig),3, "Productivity")
grid_arrange_shared_legend(list(aQALYs_fig, cQALYs_fig, dQALYs_fig),3, "QALYs")
dev.off()

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