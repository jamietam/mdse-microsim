
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

load(paste0(mainDir, "output/","1_2027rnc_1males_depression_10000_03.11.26_11.51AM.RData"))  
dfM=reformat_model_outputs(l.results)
dfM_D=reformat_model_outputs(l.results_D)
dfM_ND=reformat_model_outputs(l.results_ND)

load(paste0(mainDir, "output/","1_2027rnc_1females_depression_10000_03.11.26_01.24AM.RData"))  
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

df.Health_comb<-Health_comb%>% filter(scenario %in% c("main"), population%in% c("D"))


df.prevs_comb_1<-df.prevs_comb%>% filter(age==18.99,scenario %in% c("main", "baseline"), population=="T",status == "D")


Depression_2016_2023_baseline <-ggplot() +
  geom_line(data = df.prevs_comb_1, 
            aes(x = year, y = prev, color = scenario, linetype = scenario)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0.09, 0.12)) +
  scale_x_continuous(name = "Year", limits = c(2027, 2100)) +
  labs(title = "Prevalence of Depression") +
  scale_color_manual(values = c("main"     = "blue", 
                                "baseline" = "blue"),
                     name = "",
                     labels = c("main"     = "Nicotine Product Standard", 
                                "baseline" = "Status Quo")) +
  scale_linetype_manual(values = c("main"     = "dashed", 
                                   "baseline" = "solid"),
                        name = "",
                        labels = c("main"     = "Nicotine Product Standard", 
                                   "baseline" = "Status Quo")) +
  theme_minimal() +
  theme(legend.position = "bottom")

Depression_sad <-ggplot() +
  geom_line(data = df.Health_comb, 
            aes(x = year, y = cSAD_averted_new/1000000), color = "blue") +
  scale_y_continuous(name = "Deaths Averted (in millions)") +
  scale_x_continuous(name = "Year", limits = c(2027, 2100))+
  labs(title = "Cummulative Deaths Averted") +
  theme_minimal()

Depression_2016_2023_baselinelegend<-Depression_2016_2023_baseline
shared_legend <-get_legend(Depression_2016_2023_baselinelegend)
Depression_2016_2023_baseline<-Depression_2016_2023_baseline+ theme(legend.position = "none")

pdf(paste0("output/",figDir,"/Depressionfig.pdf"), width = 7, height = 5)
grid.arrange(arrangeGrob(Depression_2016_2023_baseline, Depression_sad, ncol = 2,nrow=1), shared_legend,  # Add shared legend
             ncol = 1, heights = c(4, 0.5) )
dev.off()
