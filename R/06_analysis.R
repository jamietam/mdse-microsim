d## Clean up the workspace and set main working directory
rm(list = ls()) 

## RUN MAIN MODEL
# mainDir = "/Users/jt936/Dropbox/GitHub/mds-microsim/"
mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" # Set working directory

source(paste0(mainDir,"R/01_environment.R"), echo=FALSE)
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo = FALSE) # microsimulation model and probability functions

## Run policy scenarios
policyyear = 2024
baseline <- main(v.params, apply_policy(1.0, 1.0, policyyear, c(0:99)))
policy_init <- main(v.params, apply_policy(0.8, 1.0, policyyear, c(0:99)))
policy_cess <- main(v.params, apply_policy(1.0, 1.2, policyyear, c(0:99)))

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
modelprevs0$scenario <- "baseline"
modelprevs1$scenario <- "policy_init"
modelprevs2$scenario <- "policy_cess"
modelprevs <- rbind(modelprevs0, modelprevs1, modelprevs2)

# costs, utilities, productivities
modelcuw <- as.data.frame(rbind(cbind(baseline$cuw,cumsum(baseline$cuw[,1]),cumsum(baseline$cuw[,2]),cumsum(baseline$cuw[,3]),rownames(baseline$cuw),"baseline"),
                                cbind(policy_init$cuw,cumsum(policy_init$cuw[,1]),cumsum(policy_init$cuw[,2]),cumsum(policy_init$cuw[,3]),rownames(policy_init$cuw),"policy_init"),
                                cbind(policy_cess$cuw,cumsum(policy_cess$cuw[,1]),cumsum(policy_cess$cuw[,2]),cumsum(policy_cess$cuw[,3]),rownames(policy_cess$cuw),"policy_cess")))
colnames(modelcuw) <- c("aCosts","aQALYs","aProductivity","cCosts","cQALYs","cProductivity","year","scenario")
modelcuw[c("aCosts","aQALYs","aProductivity","cCosts","cQALYs","cProductivity","year")] <- sapply(modelcuw[c("aCosts","aQALYs","aProductivity","cCosts","cQALYs","cProductivity","year")],as.numeric) # convert the columns to numeric
cuw_diff <- as.data.frame(rbind(
                  cbind(cumsum(policy_init$cuw[,1]) - cumsum(baseline$cuw[,1]),# calculate change compared to baseline scenario
                        cumsum(policy_init$cuw[,2]) - cumsum(baseline$cuw[,2]),
                        cumsum(policy_init$cuw[,3]) - cumsum(baseline$cuw[,3]),rownames(baseline$cuw),"policy_init"),
                  cbind(cumsum(policy_cess$cuw[,1]) - cumsum(baseline$cuw[,1]),
                        cumsum(policy_cess$cuw[,2]) - cumsum(baseline$cuw[,2]),
                        cumsum(policy_cess$cuw[,3]) - cumsum(baseline$cuw[,3]),rownames(baseline$cuw),"policy_cess")))
colnames(cuw_diff) <- c("dCosts","dQALYs","dProductivity","year","scenario")
cuw_diff[c("dCosts","dQALYs","dProductivity","year")] <- sapply(cuw_diff[c("dCosts","dQALYs","dProductivity","year")],as.numeric) # convert the columns to numeric

              
# smoking initiation and cessation
smkprobs <- as.data.frame(rbind(cbind(baseline$init[,policyyear-1899],baseline$cess[,policyyear-1899],0:99,"baseline"),
                                 cbind(policy_init$init[,policyyear-1899],policy_init$cess[,policyyear-1899],0:99,"policy_init"),
                                 cbind(policy_cess$init[,policyyear-1899],policy_cess$cess[,policyyear-1899],0:99,"policy_cess")))
colnames(smkprobs) <- c("init","cess","age", "scenario")
smkprobs[c("init","cess","age")] <- sapply(smkprobs[c("init","cess","age")],as.numeric) # convert the columns to numeric

# NSDUH prevalence data
calibtargets = rbind(cbind(data.frame(lst_targets[["N"]]),status="N"),
                     cbind(data.frame(lst_targets[["C"]]),status="C"),
                     cbind(data.frame(lst_targets[["F"]]),status="F"),
                     cbind(data.frame(lst_targets[["D"]]),status="D"),
                     cbind(data.frame(lst_targets[["ND"]]),status="ND"),
                     cbind(data.frame(lst_targets[["CD"]]),status="CD"),
                     cbind(data.frame(lst_targets[["FD"]]),status="FD"))

# Smoking-attributable mortality
mortcounts <- as.data.frame(rbind(cbind(names(baseline$SAD),baseline$SAD,cumsum(baseline$SAD),"baseline"),
              cbind(names(baseline$SAD),policy_init$SAD, cumsum(policy_init$SAD),"policy_init"),
              cbind(names(baseline$SAD),policy_cess$SAD, cumsum(policy_cess$SAD),"policy_cess")))
names(mortcounts) <- c("year","annualSADs", "cSADs","scenario")
mortcounts[c("year","annualSADs","cSADs")] <- sapply(mortcounts[c("year","annualSADs","cSADs")],as.numeric) # convert the columns to numeric

# Mortality
X_counts <- as.data.frame(rbind(cbind(names(baseline$n.X[paste0(policyyear:max(cohorts))]),
                                     baseline$n.X[paste0(policyyear:max(cohorts))],
                                     cumsum(baseline$n.X[paste0(policyyear:max(cohorts))]),"baseline"),
                               cbind(names(policy_init$n.X[paste0(policyyear:max(cohorts))]),policy_init$n.X[paste0(policyyear:max(cohorts))],
                                     cumsum(policy_init$n.X[paste0(policyyear:max(cohorts))]),"policy_init"),
                               cbind(names(policy_cess$n.X[paste0(policyyear:max(cohorts))]),policy_cess$n.X[paste0(policyyear:max(cohorts))],
                                     cumsum(policy_cess$n.X[paste0(policyyear:max(cohorts))]),"policy_cess")))
names(X_counts) <- c("year","annual", "cumulative","scenario")
X_counts[c("year","annual","cumulative")] <- sapply(X_counts[c("year","annual","cumulative")],as.numeric) # convert the columns to numeric

# calculate change compared to baseline scenario
X_diff <- as.data.frame(rbind(cbind(policyyear:max(cohorts), "policy_init",
                                    X_counts[X_counts$scenario=="baseline",]$cumulative - X_counts[X_counts$scenario=="policy_init",]$cumulative),
                              cbind(policyyear:max(cohorts), "policy_cess",
                                    X_counts[X_counts$scenario=="baseline",]$cumulative - X_counts[X_counts$scenario=="policy_cess",]$cumulative)))
colnames(X_diff) <- c("year","scenario","deaths_averted")
X_diff[c("year","deaths_averted")] <- sapply(X_diff[c("year","deaths_averted")],as.numeric) # convert the columns to numeric

## FIGURES
# Initiation and cessation
p.NC_age <- ggplot(data=smkprobs) +  geom_line( aes(x=age, y=init, linetype=scenario, color=scenario)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
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

# Overall mortality
cX_fig <- ggplot()+
  geom_line(data=X_diff,aes(x=year,y=deaths_averted, color=scenario))+
  scale_y_continuous(name="Cumulative deaths") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Cumulative deaths")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


aX_fig <- ggplot()+
  geom_line(data=X_counts,aes(x=year,y=annual, color=scenario))+
  scale_y_continuous(name="Annual deaths") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Annual deaths")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

# Smoking-attributable mortality
cSADs <- ggplot() +
  geom_line(data=mortcounts,aes(x=year,y=cSADs/1000000, color=scenario))+
  scale_y_continuous(name="Cumulative SADs (Millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Cumulative - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

annualSADs <- ggplot() +
  geom_line(data=mortcounts,aes(x=year,y=annualSADs/1000, color=scenario))+
  scale_y_continuous(name="Annual SADs (thousands)", limits=c(0,350),breaks=seq(0,350,20)) +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Annual - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

# Economic costs and QALYs
acosts <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=aCosts/1000000,color=scenario))+
  scale_y_continuous(name=" Total costs ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Annual medical expenditures - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

aprods <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=aProductivity/1000000,color=scenario))+
  scale_y_continuous(name=" Total productivity ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Annual productivity - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

aQALYs <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=aQALYs,color=scenario))+
  scale_y_continuous(name=" Total QALYS") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("QALYS - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

ccosts <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=cCosts/1000000,color=scenario))+
  scale_y_continuous(name=" Total costs ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Cumulative medical expenditures - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cprods <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=cProductivity/1000000,color=scenario))+
  scale_y_continuous(name=" Total productivity ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Cumulative productivity - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cQALYs <- ggplot()+
  geom_line(data=modelcuw,aes(x=year,y=cQALYs,color=scenario))+
  scale_y_continuous(name=" Total QALYS") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title=paste0("Cumulative QALYS - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dcosts <- ggplot()+
  geom_line(data=cuw_diff,aes(x=year,y=dCosts/100000,color=scenario))+
  scale_y_continuous(name=" Total costs ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Change in Medical Costs compared to baseline")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dprods <- ggplot()+
  geom_line(data=cuw_diff,aes(x=year,y=dProductivity/100000,color=scenario))+
  scale_y_continuous(name=" Total productivity ($ millions)") +
  scale_x_continuous(name="Year",limits=c(2020,2100),breaks=seq(2020,2100,10))  +
  labs(title="Change in Productivity compared to baseline")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dQALYs <- ggplot()+
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
text(.5, 0.5, paste0("Policy outcomes \n",whichgender), font=1, cex=1.5)
grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking probabilities")
csprevs
grid_arrange_shared_legend(list(annualSADs,cSADs),2,"Smoking-Attributable Deaths")
grid_arrange_shared_legend(list(acosts, aprods, aQALYs),3, "Annual Economic and Morbidity Outcomes")
grid_arrange_shared_legend(list(dcosts, dprods),2, "Cumulative")
grid_arrange_shared_legend(list(ccosts, cprods, cQALYs),3, "Cumulative Economic and Morbidity Outcomes")
grid_arrange_shared_legend(list(dcosts, dprods, dQALYs),3, "Change in Economic and Morbidity Outcomes")
dev.off()