rm(list = ls()) 

mainDir = "/Users/srs475/Library/CloudStorage/Dropbox-UniversityofMichigan/Sarah Skolnick/GitHub/mdse-microsim/"

setwd(mainDir)


library(plyr)
library(dplyr)
library(tidyselect)
library(survey)
options(survey.lonely.psu="adjust")

load("data/mdseprevs0523.rda")

# Data visualization and results figures -------------------------------------------------------------------
library(ggplot2)
library(grid)
library(gridBase)
library(gridExtra)

# Prepare data for all three age groups, Total population, status=="D"
efig3_data <- subset(mdseprevs, 
                     age %in% c(18.25, 18.99, 26.34) & 
                       subpopulation == "totalpop" & 
                       status == "D")

# Make age a factor with labels for the legend
efig3_data$age_group <- factor(efig3_data$age, 
                               levels = c(18.25, 18.99, 26.34),
                               labels = c("18-25", "18-99", "26-34"))

# Rename gender levels to match panel labels
efig3_data$gender_label <- ifelse(efig3_data$gender == "Men", "Men", "Women")
efig3_data$gender_label <- factor(efig3_data$gender_label, levels = c("Men", "Women"))

eFigure3 <- ggplot(efig3_data, 
                   aes(x = survey_year, 
                       y = prev, 
                       ymin = prev_lowCI, 
                       ymax = prev_highCI,
                       shape = age_group,
                       color = age_group,
                       fill  = age_group)) +
  geom_pointrange(size = 0.4) +
  facet_wrap(~ gender_label) +
  scale_shape_manual(values = c(19, 17, 15)) +        # circle, triangle, square
  scale_color_manual(values = c("black", "gray50", "gray70")) +
  scale_y_continuous(name = "Prevalence",
                     limits = c(0, 0.30),
                     breaks = seq(0, 0.28, 0.02)) +
  # scale_x_continuous(name = "Year",
  #                    limits = c(min(xaxisbreaks), max(xaxisbreaks)),
  #                    breaks = xaxisbreaks) +
  labs(title = "",
       shape = "Age", color = "Age", fill = "Age") +
  theme_bw() +
  theme(
    axis.text.x      = element_text(angle = 60, hjust = 1),
    legend.title     = element_text(),          # remove the blank override so the title shows
    text             = element_text(size = 12),
    strip.background = element_rect(fill = "gray85"),
    strip.text       = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

jpeg(filename = paste0("output/eFigure3_MDEprev_.jpg"),
     width = 9, height = 4, units = "in", res = 1000)
print(eFigure3)
dev.off()


rm(list = ls()) 
#this doesn't work right now?
# Sys.setenv(RGL_USE_NULL=TRUE) 
# Sys.setenv('R_MAX_VSIZE'=64000000000)
# Set working directory
mainDir = "/Users/srs475/Library/CloudStorage/Dropbox-UniversityofMichigan/Sarah Skolnick/GitHub/mdse-microsim/"
# mainDir = "/Users/jt936/Dropbox/GitHub/mdse-microsim/"
# mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" 
setwd(mainDir)

hpc = 0
calibration = 0 # need to set this to 0 so main_calib works and outputs proper matrix for main function
run_scenarios = 0 # set to 0 if you want to use pre-generated results, set to 1 to simulate all scenarios

#set seed
seednew <<- 1
n.i <- 10000 # number of people per birth cohort
#n.i <- 100

policyyear <- 2027
v.affected_ages <- c(0:99) # affects all ages
d.c <- d.u <- d.w <- 0.03              # equal discounting of costs and QALYs by 3%
d.year <- 2025 # which year to start discounting from


args <- c("females",n.i, 2100) #no need to change this for now
source(paste0(mainDir,"R/01_environment.R"), echo=FALSE) #
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE)
load(paste0(mainDir,"data/nsduh_calib_targets_both.RData")) # Load NSDUH data

df.calib_targets <- do.call(rbind, lapply(names(l.calib_targets), function(status) {
  cbind(data.frame(l.calib_targets[[status]]), status = status)
}))
load(paste0(mainDir, "output/","combined_2027_male20000.RData"))  
dfM=reformat_model_outputs(l.results)
dfM_D=reformat_model_outputs(l.results_D)
dfM_ND=reformat_model_outputs(l.results_ND)

load(paste0(mainDir, "output/","combined_2027_female20000.RData"))  
dfF=reformat_model_outputs(l.results)
dfF_D=reformat_model_outputs(l.results_D)
dfF_ND=reformat_model_outputs(l.results_ND)

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

#calculate depressions averted: prevalence of depression each year time population for taht year
df.prevs_D<- df.prevs_comb %>%
  filter(population=="T", age == 18.99, status=="D", year%in% c(2026,2027,2028), scenario%in% c("baseline","main","best","worst"))%>%select(year,prev,scenario)
df.prevs_D<- df.prevs_comb %>%
  filter(population=="T", age == 18.99, status=="D")%>%select(year,prev,scenario)

pop_sums <- colSums(pop)
merged_df <- merge(data.frame(year = names(pop_sums), population1 = pop_sums, stringsAsFactors = FALSE), df.prevs_D, by = "year")
# Multiplying the population by the prevalence for each year
merged_df$result <- merged_df$population1 * merged_df$prev

diffDprevworst=merged_df$prev[merged_df$year==2100 & merged_df$scenario=="baseline"]-merged_df$prev[merged_df$year==2100 & merged_df$scenario=="worst"]
diffDprevbest=merged_df$prev[merged_df$year==2100 & merged_df$scenario=="baseline"]-merged_df$prev[merged_df$year==2100 & merged_df$scenario=="best"]
diffDprev=merged_df$prev[merged_df$year==2100 & merged_df$scenario=="baseline"]-merged_df$prev[merged_df$year==2100 & merged_df$scenario=="main"]


compute_scenario_difference <- function(df, scenario_a, scenario_b) {
  df_a <- subset(df, scenario == scenario_a)
  df_b <- subset(df, scenario == scenario_b)
  colnames(df_a)[which(colnames(df_a) == "result")] <- "result_a"
  colnames(df_b)[which(colnames(df_b) == "result")] <- "result_b"
  merged_df <- merge(df_a, df_b, by = "year")
  merged_df$difference <- merged_df$result_b - merged_df$result_a
  merged_df$scenario_comparison <- paste0(scenario_a)
  result_df <- merged_df[, c("year", "result_a", "result_b", "difference", "scenario_comparison")]
  return(result_df)
}

# Combine all scenario differences into one dataframe
depression_difference <- bind_rows(
  compute_scenario_difference(merged_df, "main",  "baseline"),
  compute_scenario_difference(merged_df, "worst", "baseline"),
  compute_scenario_difference(merged_df, "best",  "baseline")
)

# Cumulative sum from 2027 to 2100 for each scenario
depression_difference_cumsum <- depression_difference %>%
  filter(year >= 2027 & year <= 2100) %>%
  arrange(scenario_comparison, year) %>%
  group_by(scenario_comparison) %>%
  mutate(cumsum_difference = cumsum(difference)) %>%
  ungroup()

depression_difference_cumsum_p<- subset(depression_difference_cumsum, year==2100)
diffDprev*100
diffDprevworst*100
diffDprevbest*100

library(openxlsx)

# Create summary table
summary_table <- subset(depression_difference_cumsum_p, year == 2100) %>%
  select(scenario_comparison, result_a, result_b, difference, cumsum_difference) %>%
  mutate(prev_diff_pct = c(diffDprev*100, diffDprevworst*100, diffDprevbest*100))

write.xlsx(summary_table, file = "output/depression_difference_summary.xlsx", rowNames = FALSE)
# cum_dep<-data.frame(sum(depressiondifference$difference[depressiondifference$year%in% c(2028:2100)]))
# 
# depressiondifference$cumsum<-cumsum(depressiondifference$difference)
# depressiondifference$year <- as.numeric(depressiondifference$year)

# nc=ncol(dfM[[3]])
# Health=dfM[[3]][,6:nc]+dfF[[3]][,6:nc]
# Health=cbind(dfM[[3]][,1:2],Health)
# Health_D=dfM_D[[3]][,6:nc]+dfF_D[[3]][,6:nc]
# Health_D=cbind(dfM_D[[3]][,1:2],Health_D)
# Health_ND=dfM_ND[[3]][,6:nc]+dfF_ND[[3]][,6:nc]
# Health_ND=cbind(dfM_ND[[3]][,1:2],Health_ND)
# #combine by depression status
# Health_ND$population="ND"
# Health_D$population="D"
# Health$population="T"
# 
# 
# Health_comb<-rbind(Health,Health_D,Health_ND)
# 
# df.Health_comb<-Health_comb%>% filter(scenario %in% c("main"), population%in% c("D"))


df.prevs_comb_1<-df.prevs_comb%>% filter(age==18.99,scenario %in% c("main", "baseline", "best","worst"), population=="T",status == "D")

df_filtered2 <- subset(df.prevs_comb_1, status == "D" & age == 18.99 & year==2100) #check percentage
df_filtered <- subset(df.prevs_comb_1, status == "D" & age == 18.99) %>%
  select(year, status, population, scenario, prev) %>%
  pivot_wider(names_from = scenario, values_from = prev)

Depression_2016_2023_baseline <- ggplot() +
  geom_pointrange(data = subset(df.calib_targets, status == "D" & age == 18.99),
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = "NSDUH Data"),
                  size = .5, alpha = 0.5) +
  geom_line(data = df_filtered, aes(x = year, y = main,     color = "Nicotine Product Standard"), size = 1, linetype = "dashed") +
  geom_line(data = df_filtered, aes(x = year, y = baseline, color = "Status Quo"),                size = 1) +
  geom_ribbon(data = df_filtered,
              aes(x = year, ymin = worst, ymax = best),
              fill = "lightblue", alpha = 0.6) +
  scale_color_manual(values = c("NSDUH Data"                = "black",
                                "Nicotine Product Standard"  = "blue",
                                "Status Quo"                 = "black"),
                     breaks = c("NSDUH Data", "Nicotine Product Standard", "Status Quo")) +
  labs(title = "Prevalence of MD",
       x     = "Year",
       y     = "MD Prevalence",
       color = "") +
  scale_x_continuous(limits = c(2005, 2100), breaks = c(2005, seq(2025, 2100, 25))) +
  scale_y_continuous(limits = c(0.06, 0.12), breaks = seq(0.06, 0.12, 0.02)) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  guides(
    color = guide_legend(override.aes = list(linetype = c("dashed", "solid", "solid"), size = .5), keywidth = 2.5, keyheight = 1),
    linetype = guide_legend(override.aes = list(size = .5), keywidth = 2.5, keyheight = 1)
  )
depression_difference_cumsum <- depression_difference_cumsum %>%
  mutate(year = as.numeric(year))

depression_difference_cumsum_wide <- depression_difference_cumsum %>%
  select(year, scenario_comparison, cumsum_difference) %>%
  pivot_wider(names_from = scenario_comparison, values_from = cumsum_difference)

Depression_sad <- ggplot() +
  geom_ribbon(data = depression_difference_cumsum_wide,
              aes(x = year, ymin = worst / 1000000, ymax = best / 1000000),
              fill = "lightblue", alpha = 0.6) +
  geom_line(data = subset(depression_difference_cumsum, scenario_comparison == "main"),
            aes(x = year, y = cumsum_difference / 1000000), color = "blue", size = 1, linetype = "dashed") +
  scale_y_continuous(name = "Cumulative MD cases averted (in millions)") +
  scale_x_continuous(name = "Year", limits = c(2025, 2100), breaks = c(2025, seq(2025, 2100, 25))) +
  labs(title = "Cumulative MD cases averted") +
  theme_minimal()

Depression_2016_2023_baselinelegend<-Depression_2016_2023_baseline
shared_legend <-get_legend(Depression_2016_2023_baselinelegend)
Depression_2016_2023_baseline<-Depression_2016_2023_baseline+ theme(legend.position = "none")

pdf(paste0("output/Depressionfig.pdf"), width = 7, height = 5)
grid.arrange(arrangeGrob(Depression_2016_2023_baseline, Depression_sad, ncol = 2,nrow=1), shared_legend,  # Add shared legend
             ncol = 1, heights = c(4, 0.5) )
dev.off()
