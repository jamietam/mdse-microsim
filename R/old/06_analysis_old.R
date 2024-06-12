## Clean up the workspace and set main working directory
rm(list = ls()) 

## RUN MAIN MODEL
mainDir = "/Users/jt936/Dropbox/GitHub/mds-microsim/"
# mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" # Set working directory

source(paste0(mainDir,"R/01_environment.R"), echo=FALSE)
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE) # microsimulation model and probability functions

## Run policy scenarios
policyyear = 2024

scenarios = c("baseline","policy_init","policy_cess", "policy_rnc1","policy_rnc2")

baseline <- main(v.params, apply_policy(1.0, 1.0, policyyear, c(0:99)))
policy_init <- main(v.params, apply_policy(0.8, 1.0, policyyear, c(0:99)))
policy_cess <- main(v.params, apply_policy(1.0, 1.2, policyyear, c(0:99)))
policy_rnc1 <- main(v.params, apply_policy(0.5, 6.0, policyyear, c(0:99)))
policy_rnc2 <- main(v.params, apply_policy(0.5, 3.95, policyyear, c(0:99)))

save(baseline,policy_cess, policy_init,policy_rnc1,policy_rnc2, file="scenarios_females_1000.Rda")
# load("scenarios_females_10000.Rda")


## REFORMAT DATA for data visualization

# prevalences
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
modelprevs0 <- create_model_prev(baseline)
modelprevs1 <- create_model_prev(policy_init)
modelprevs2 <- create_model_prev(policy_cess)
modelprevs3 <- create_model_prev(policy_rnc1)
modelprevs4 <- create_model_prev(policy_rnc2)
modelprevs0$scenario <- "baseline"
modelprevs1$scenario <- "policy_init"
modelprevs2$scenario <- "policy_cess"
modelprevs3$scenario <- "policy_rnc1"
modelprevs4$scenario <- "policy_rnc2"
modelprevs <- rbind(modelprevs0, modelprevs1, modelprevs2,modelprevs3, modelprevs4)

# costs, utilities, productivities
modelcuw <- as.data.frame(rbind(cbind(baseline$cuw,cumsum(baseline$cuw[,1]),cumsum(baseline$cuw[,2]),cumsum(baseline$cuw[,3]),rownames(baseline$cuw),"baseline"),
                                cbind(policy_init$cuw,cumsum(policy_init$cuw[,1]),cumsum(policy_init$cuw[,2]),cumsum(policy_init$cuw[,3]),rownames(policy_init$cuw),"policy_init"),
                                cbind(policy_cess$cuw,cumsum(policy_cess$cuw[,1]),cumsum(policy_cess$cuw[,2]),cumsum(policy_cess$cuw[,3]),rownames(policy_cess$cuw),"policy_cess"),
                                cbind(policy_init$cuw,cumsum(policy_init$cuw[,1]),cumsum(policy_init$cuw[,2]),cumsum(policy_init$cuw[,3]),rownames(policy_init$cuw),"policy_rnc1"),
                                cbind(policy_cess$cuw,cumsum(policy_cess$cuw[,1]),cumsum(policy_cess$cuw[,2]),cumsum(policy_cess$cuw[,3]),rownames(policy_cess$cuw),"policy_rnc2")
                                ))
colnames(modelcuw) <- c("aCosts","aQALYs","aProductivity","cCosts","cQALYs","cProductivity","year","scenario")
modelcuw[c("aCosts","aQALYs","aProductivity","cCosts","cQALYs","cProductivity","year")] <- sapply(modelcuw[c("aCosts","aQALYs","aProductivity","cCosts","cQALYs","cProductivity","year")],as.numeric) # convert the columns to numeric
cuw_diff <- as.data.frame(rbind(
  cbind(cumsum(baseline$cuw[,1]) - cumsum(baseline$cuw[,1]),# calculate change compared to baseline scenario
        cumsum(baseline$cuw[,2]) - cumsum(baseline$cuw[,2]),
        cumsum(baseline$cuw[,3]) - cumsum(baseline$cuw[,3]),rownames(baseline$cuw),"baseline"),
  cbind(cumsum(policy_init$cuw[,1]) - cumsum(baseline$cuw[,1]),# calculate change compared to baseline scenario
        cumsum(policy_init$cuw[,2]) - cumsum(baseline$cuw[,2]),
        cumsum(policy_init$cuw[,3]) - cumsum(baseline$cuw[,3]),rownames(baseline$cuw),"policy_init"),
  cbind(cumsum(policy_cess$cuw[,1]) - cumsum(baseline$cuw[,1]),
        cumsum(policy_cess$cuw[,2]) - cumsum(baseline$cuw[,2]),
        cumsum(policy_cess$cuw[,3]) - cumsum(baseline$cuw[,3]),rownames(baseline$cuw),"policy_cess"),
  cbind(cumsum(policy_rnc1$cuw[,1]) - cumsum(baseline$cuw[,1]),
        cumsum(policy_rnc1$cuw[,2]) - cumsum(baseline$cuw[,2]),
        cumsum(policy_rnc1$cuw[,3]) - cumsum(baseline$cuw[,3]),rownames(baseline$cuw),"policy_rnc1"),
  cbind(cumsum(policy_rnc2$cuw[,1]) - cumsum(baseline$cuw[,1]),
        cumsum(policy_rnc2$cuw[,2]) - cumsum(baseline$cuw[,2]),
        cumsum(policy_rnc2$cuw[,3]) - cumsum(baseline$cuw[,3]),rownames(baseline$cuw),"policy_rnc2")
                  ))
colnames(cuw_diff) <- c("dCosts","dQALYs","dProductivity","year","scenario")
cuw_diff[c("dCosts","dQALYs","dProductivity","year")] <- sapply(cuw_diff[c("dCosts","dQALYs","dProductivity","year")],as.numeric) # convert the columns to numeric

              
# smoking initiation and cessation
smkprobs <- as.data.frame(rbind(cbind(baseline$init[,policyyear-1899],baseline$cess[,policyyear-1899],0:99,"baseline"),
                                 cbind(policy_init$init[,policyyear-1899],policy_init$cess[,policyyear-1899],0:99,"policy_init"),
                                 cbind(policy_cess$init[,policyyear-1899],policy_cess$cess[,policyyear-1899],0:99,"policy_cess"),
                                 cbind(policy_rnc1$init[,policyyear-1899],policy_rnc1$cess[,policyyear-1899],0:99,"policy_rnc1"),
                                 cbind(policy_rnc2$init[,policyyear-1899],policy_rnc2$cess[,policyyear-1899],0:99,"policy_rnc2")
                                ))
colnames(smkprobs) <- c("init","cess","age", "scenario")
smkprobs[c("init","cess","age")] <- sapply(smkprobs[c("init","cess","age")],as.numeric) # convert the columns to numeric


# Smoking-attributable mortality
mortcounts <- as.data.frame(rbind(
              cbind(names(baseline$SAD),baseline$SAD,    cumsum(baseline$SAD),   "baseline"),
              cbind(names(baseline$SAD),policy_init$SAD, cumsum(policy_init$SAD),"policy_init"),
              cbind(names(baseline$SAD),policy_cess$SAD, cumsum(policy_cess$SAD),"policy_cess"),
              cbind(names(baseline$SAD),policy_rnc1$SAD, cumsum(policy_rnc1$SAD),"policy_rnc1"),
              cbind(names(baseline$SAD),policy_rnc2$SAD, cumsum(policy_rnc2$SAD),"policy_rnc2")
              ))
names(mortcounts) <- c("year","annualSADs", "cSADs","scenario")
mortcounts[c("year","annualSADs","cSADs")] <- sapply(mortcounts[c("year","annualSADs","cSADs")],as.numeric) # convert the columns to numeric

# Mortality
X_counts <- as.data.frame(rbind(
                          cbind(names(baseline$n.X[paste0(policyyear:max(cohorts))]), baseline$n.X[paste0(policyyear:max(cohorts))],
                               cumsum(baseline$n.X[paste0(policyyear:max(cohorts))]),"baseline",baseline$SAD,cumsum(baseline$SAD)),
                          cbind(names(policy_init$n.X[paste0(policyyear:max(cohorts))]),policy_init$n.X[paste0(policyyear:max(cohorts))],
                               cumsum(policy_init$n.X[paste0(policyyear:max(cohorts))]),"policy_init",policy_init$SAD,cumsum(policy_init$SAD)),
                          cbind(names(policy_cess$n.X[paste0(policyyear:max(cohorts))]),policy_cess$n.X[paste0(policyyear:max(cohorts))],
                               cumsum(policy_cess$n.X[paste0(policyyear:max(cohorts))]),"policy_cess",policy_cess$SAD,cumsum(policy_cess$SAD)),
                          cbind(names(policy_rnc1$n.X[paste0(policyyear:max(cohorts))]),policy_rnc1$n.X[paste0(policyyear:max(cohorts))],
                                cumsum(policy_rnc1$n.X[paste0(policyyear:max(cohorts))]),"policy_rnc1",policy_rnc1$SAD,cumsum(policy_rnc1$SAD)),
                          cbind(names(policy_rnc2$n.X[paste0(policyyear:max(cohorts))]),policy_rnc2$n.X[paste0(policyyear:max(cohorts))],
                                cumsum(policy_rnc2$n.X[paste0(policyyear:max(cohorts))]),"policy_rnc2",policy_rnc2$SAD,cumsum(policy_rnc2$SAD))))
names(X_counts) <- c("year","aX","cX","scenario","aSAD","cSAD")
X_counts[c("year","aX","cX","aSAD","cSAD")] <- sapply(X_counts[c("year","aX","cX","aSAD","cSAD")],as.numeric) # convert the columns to numeric
X_counts <- cbind(X_counts, cX_averted = c(X_counts[X_counts$scenario=="baseline",]$cX - X_counts[X_counts$scenario=="baseline",]$cX, # calculate change compared to baseline scenario
                                      X_counts[X_counts$scenario=="baseline",]$cX - X_counts[X_counts$scenario=="policy_init",]$cX,
                                      X_counts[X_counts$scenario=="baseline",]$cX - X_counts[X_counts$scenario=="policy_cess",]$cX,
                                      X_counts[X_counts$scenario=="baseline",]$cX - X_counts[X_counts$scenario=="policy_rnc1",]$cX,
                                      X_counts[X_counts$scenario=="baseline",]$cX - X_counts[X_counts$scenario=="policy_rnc2",]$cX))
X_counts <- cbind(X_counts, aX_averted = c(X_counts[X_counts$scenario=="baseline",]$aX - X_counts[X_counts$scenario=="baseline",]$aX,# calculate change compared to baseline scenario
                                           X_counts[X_counts$scenario=="baseline",]$aX - X_counts[X_counts$scenario=="policy_init",]$aX,
                                           X_counts[X_counts$scenario=="baseline",]$aX - X_counts[X_counts$scenario=="policy_cess",]$aX,
                                           X_counts[X_counts$scenario=="baseline",]$aX - X_counts[X_counts$scenario=="policy_rnc1",]$aX,
                                           X_counts[X_counts$scenario=="baseline",]$aX - X_counts[X_counts$scenario=="policy_rnc2",]$aX))
X_counts <- cbind(X_counts, aSAD_averted = c(X_counts[X_counts$scenario=="baseline",]$aSAD - X_counts[X_counts$scenario=="baseline",]$aSAD,
                                           X_counts[X_counts$scenario=="baseline",]$aSAD - X_counts[X_counts$scenario=="policy_init",]$aSAD,
                                           X_counts[X_counts$scenario=="baseline",]$aSAD - X_counts[X_counts$scenario=="policy_cess",]$aSAD,
                                           X_counts[X_counts$scenario=="baseline",]$aSAD - X_counts[X_counts$scenario=="policy_rnc1",]$aSAD,
                                           X_counts[X_counts$scenario=="baseline",]$aSAD - X_counts[X_counts$scenario=="policy_rnc2",]$aSAD
                                           ))
X_counts <- cbind(X_counts, cSAD_averted = c(X_counts[X_counts$scenario=="baseline",]$cSAD - X_counts[X_counts$scenario=="baseline",]$cSAD, # calculate change compared to baseline scenario
                                             X_counts[X_counts$scenario=="baseline",]$cSAD - X_counts[X_counts$scenario=="policy_init",]$cSAD,
                                             X_counts[X_counts$scenario=="baseline",]$cSAD - X_counts[X_counts$scenario=="policy_cess",]$cSAD,
                                             X_counts[X_counts$scenario=="baseline",]$cSAD - X_counts[X_counts$scenario=="policy_rnc1",]$cSAD,
                                             X_counts[X_counts$scenario=="baseline",]$cSAD - X_counts[X_counts$scenario=="policy_rnc2",]$cSAD))
# Life-years
ly_counts <- as.data.frame(rbind(cbind(names(baseline$n.lifeyears[paste0(policyyear:max(cohorts))]),
                                          baseline$n.lifeyears[paste0(policyyear:max(cohorts))],
                                          cumsum(baseline$n.lifeyears[paste0(policyyear:max(cohorts))]),"baseline"),
                                    cbind(names(policy_init$n.lifeyears[paste0(policyyear:max(cohorts))]),
                                          policy_init$n.lifeyears[paste0(policyyear:max(cohorts))],
                                          cumsum(policy_init$n.lifeyears[paste0(policyyear:max(cohorts))]),"policy_init"),
                                    cbind(names(policy_cess$n.lifeyears[paste0(policyyear:max(cohorts))]),
                                          policy_cess$n.lifeyears[paste0(policyyear:max(cohorts))],
                                          cumsum(policy_cess$n.lifeyears[paste0(policyyear:max(cohorts))]),"policy_cess"),
                                    cbind(names(policy_rnc1$n.lifeyears[paste0(policyyear:max(cohorts))]),
                                          policy_rnc1$n.lifeyears[paste0(policyyear:max(cohorts))],
                                          cumsum(policy_rnc1$n.lifeyears[paste0(policyyear:max(cohorts))]),"policy_rnc1"),
                                    cbind(names(policy_rnc2$n.lifeyears[paste0(policyyear:max(cohorts))]),
                                          policy_rnc2$n.lifeyears[paste0(policyyear:max(cohorts))],
                                          cumsum(policy_rnc2$n.lifeyears[paste0(policyyear:max(cohorts))]),"policy_rnc2")))

names(ly_counts) <- c("year","aLY", "cLY","scenario")
ly_counts[c("year","aLY","cLY")] <- sapply(ly_counts[c("year","aLY","cLY")],as.numeric) 
ly_counts <- cbind(ly_counts, lyg = c(ly_counts[ly_counts$scenario=="baseline",]$cLY - ly_counts[ly_counts$scenario=="baseline",]$cLY, # calculate change compared to baseline scenario
                   ly_counts[ly_counts$scenario=="policy_init",]$cLY - ly_counts[ly_counts$scenario=="baseline",]$cLY,
                   ly_counts[ly_counts$scenario=="policy_cess",]$cLY - ly_counts[ly_counts$scenario=="baseline",]$cLY,
                   ly_counts[ly_counts$scenario=="policy_rnc1",]$cLY - ly_counts[ly_counts$scenario=="baseline",]$cLY,
                   ly_counts[ly_counts$scenario=="policy_rnc2",]$cLY - ly_counts[ly_counts$scenario=="baseline",]$cLY))

# NSDUH prevalence data
calibtargets = rbind(cbind(data.frame(lst_targets[["N"]]),status="N"),
                     cbind(data.frame(lst_targets[["C"]]),status="C"),
                     cbind(data.frame(lst_targets[["F"]]),status="F"),
                     cbind(data.frame(lst_targets[["D"]]),status="D"),
                     cbind(data.frame(lst_targets[["ND"]]),status="ND"),
                     cbind(data.frame(lst_targets[["CD"]]),status="CD"),
                     cbind(data.frame(lst_targets[["FD"]]),status="FD"))

## FIGURES
# Initiation and cessation
p.NC_age <- ggplot(data=smkprobs) +  geom_line( aes(x=age, y=init, linetype=scenario, color=scenario)) +
  scale_x_continuous(name="Age", limits=c(0,30), breaks=seq(0,99,10)) +
  labs(title="Initiation probabilities")
p.CF_age <- ggplot(data=smkprobs) +  geom_line( aes(x=age, y=cess, linetype=scenario, color=scenario)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Cessation probabilities")

# Adult smoking prevalence
csprevs <- ggplot() +
  geom_pointrange(data=subset(calibtargets,age==18.99 & (status=="CD" | status=="C")), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, linetype=status, shape=status),)+
  geom_line(data = subset(modelprevs, age==18.99 & (status=="CD" | status=="C")),  aes(x=year, y= prev,color=scenario,linetype=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.5),breaks=seq(0,0.5,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2100),breaks=seq(2005,2100,10))  +
  labs(title=paste0("Current smoking - Women ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))

# Smoking-attributable mortality
cSADs <- ggplot() +
  geom_line(data=X_counts,aes(x=year,y=cSAD/1000000, color=scenario))+
  scale_y_continuous(name="Cumulative SADs (Millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Cumulative - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

aSADs <- ggplot() +
  geom_line(data=mortcounts,aes(x=year,y=annualSADs/1000, color=scenario))+
  scale_y_continuous(name="Annual SADs (thousands)", limits=c(0,300),breaks=seq(0,300,20)) +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Annual SADs among women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cSAD_averted_fig <- ggplot()+
  geom_line(data=X_counts,aes(x=year,y=cSAD_averted/1000, color=scenario))+
  scale_y_continuous(name="Cumulative SADs averted (thousands)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Cumulative SADs averted")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

aSAD_averted_fig <- ggplot()+
  geom_line(data=X_counts,aes(x=year,y=aSAD_averted/1000, color=scenario))+
  scale_y_continuous(name="Annual SADs averted (thousands)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Annual SADs averted")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

# Overall mortality
cX_fig <- ggplot()+
  geom_line(data=X_counts,aes(x=year,y=cX, color=scenario))+
  scale_y_continuous(name="Total deaths") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Cumulative deaths")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cX_averted_fig <- ggplot()+
  geom_line(data=X_counts,aes(x=year,y=cX_averted, color=scenario))+
  scale_y_continuous(name="Total deaths averted") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Cumulative deaths averted")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

## SOMETHING IS WRONG WITH NUMBER OF ANNUAL DEATHS AVERTED - NEGATIVE values 2060-2100
aX_fig <- ggplot()+
  geom_line(data=X_counts,aes(x=year,y=aX, color=scenario))+
  scale_y_continuous(name="Total deaths averted") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Annual deaths")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

aX_averted_fig <- ggplot()+
  geom_line(data=X_counts,aes(x=year,y=aX_averted, color=scenario))+
  scale_y_continuous(name="Annual deaths") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Annual deaths")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

# Person Life-Years
cLY_fig <- ggplot()+
  geom_line(data=ly_counts,aes(x=year,y=lyg, color=scenario))+
  scale_y_continuous(name="Total life-years") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Cumulative Life-years Gained")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

## SOMETHING IS WRONG WITH NUMBER OF ANNUAL DEATHS AVERTED - NEGATIVE values 2060-2100
aX_fig <- ggplot()+
  geom_line(data=X_counts,aes(x=year,y=aX_averted, color=scenario))+
  scale_y_continuous(name="Annual deaths") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Annual deaths")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

# Economic costs and QALYs
acosts_fig <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=aCosts/1000000,color=scenario))+
  scale_y_continuous(name=" Total costs ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Annual medical expenditures - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

aprod_fig <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=aProductivity/1000000,color=scenario))+
  scale_y_continuous(name=" Total productivity ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Annual productivity - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

aQALYs_fig <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=aQALYs,color=scenario))+
  scale_y_continuous(name=" Total QALYS") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("QALYS - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

ccosts_fig <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=cCosts/1000000,color=scenario))+
  scale_y_continuous(name=" Total costs ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Cumulative medical expenditures - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cprod_fig <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=cProductivity/1000000,color=scenario))+
  scale_y_continuous(name=" Total productivity ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Cumulative productivity - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cQALYs_fig <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=cQALYs,color=scenario))+
  scale_y_continuous(name=" Total QALYS") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Cumulative QALYS - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dcosts_fig <- ggplot()+
  geom_line(data=cuw_diff,aes(x=year,y=dCosts/1000000,color=scenario))+
  scale_y_continuous(name=" Total costs ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Change in Medical Costs compared to baseline")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dprod_fig <- ggplot()+
  geom_line(data=cuw_diff,aes(x=year,y=dProductivity/1000000,color=scenario))+
  scale_y_continuous(name=" Total productivity ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Change in Productivity compared to baseline")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dQALYs_fig <- ggplot()+
  geom_line(data=cuw_diff,aes(x=year,y=dQALYs,color=scenario))+
  scale_y_continuous(name=" Total QALYS") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Change in QALYS compared to baseline")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

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
pdf(file = paste0(mainDir,"output/", "policy_",whichgender,"_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I:%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
plot.new()
text(.5, 0.5, paste0("Policy outcomes \n",whichgender,"\n",n.i," per birth cohort"), font=1, cex=1.5)
grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking probabilities")
csprevs
grid_arrange_shared_legend(list(aX_fig,cX_fig),2,"All Deaths")
grid_arrange_shared_legend(list(aSAD_averted_fig,cSAD_averted_fig),2,"Smoking-Attributable Deaths")
grid_arrange_shared_legend(list(acosts_fig, ccosts_fig, dcosts_fig),3, "Healthcare costs")
grid_arrange_shared_legend(list(aprod_fig, cprod_fig, dprod_fig),3, "Productivity")
grid_arrange_shared_legend(list(aQALYs_fig, cQALYs_fig, dQALYs_fig),3, "QALYs")
dev.off()
