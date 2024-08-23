## VALIDATION
# Internal validation to compare model-predicted outputs evaluated at calibrated parameters vs the calibration targets

## Run the model ---------------------------------------------------
  
l.model_prevs<-main_calib(v.params)

v.gof <- v.ssd <- numeric(n.target)   # Calculate goodness-of-fit of model outputs to targets

for (r in 1:length(l.calib_targets)){ # sum of squared differences - removes model years after 2022 (where we don't have NSDUH data for calibration)
  ssd <- sum((l.calib_targets[[r]][,"prev"] - subset(l.model_prevs[[r]],l.model_prevs[[r]][,2]>=calib_startyear & l.model_prevs[[r]][,2]<=calib_endyear)[,"prev"])^2) # prevalence by age group
  gof = gof_norm_loglike(target_mean = l.calib_targets[[r]][,"prev"],
                         model_output = subset(l.model_prevs[[r]],l.model_prevs[[r]][,"year"]>=calib_startyear & l.model_prevs[[r]][,"year"]<=calib_endyear)[,"prev"],
                         target_sd = l.calib_targets[[r]][,"se"])
  v.gof[r] <- gof
  v.ssd[r] <- ssd
}
names(v.gof) <- paste0(names(l.calib_targets),".loglik")
names(v.ssd) <- paste0(names(l.calib_targets),".ssd")
loglik_value <- sum(v.gof)
ssd_value <- sum(v.ssd)
print(loglik_value)
print(v.gof)
print(ssd_value)

## Process model inputs ----------------------------------------------------

# Helper function for parameter selection
select_param <- function(param_name) {
  ifelse(m.calib_inputs[param_name, "calib"] == 1, v.params[param_name], m.calib_inputs[param_name, "value"])
}

# Initiation and cessation probabilities
s.NC_9.17 <- select_param("s.NC_9.17")
s.NC_18.25 <- select_param("s.NC_18.25")
s.CF_18.25 <- select_param("s.CF_18.25")
s.CF_26.34 <- select_param("s.CF_26.34")
s.CF_35.49 <- select_param("s.CF_35.49")
s.CF_50.64 <- select_param("s.CF_50.64")
s.CF_65.99 <- select_param("s.CF_65.99")
p.NC <- smk_init * c(rep(s.NC_9.17, 18), rep(s.NC_18.25, 8), rep(0, 74))
p.CF <- smk_cess * c(rep(0, 16), rep(s.CF_18.25, 10), rep(s.CF_26.34, 9), rep(s.CF_35.49, 15), rep(s.CF_50.64, 15), rep(s.CF_65.99, 35))

# Recovery
p.DR <- rep(0,100)
p.DR[13:18] <- select_param("p.DR_12.17")
p.DR[19:26] <- select_param("p.DR_18.25")
p.DR[27:35] <- select_param("p.DR_26.34")
p.DR[36:50] <- select_param("p.DR_35.49")
p.DR[51:65] <- select_param("p.DR_50_64")
p.DR[66:99] <- select_param("p.DR_65_99")

# Incidence
s.HD_12.17 <- select_param("s.HD_12.17")
s.HD_18.25 <- select_param("s.HD_18.25")
s.HD_26.34 <- select_param("s.HD_26.34")

yearinc_p.HD <- select_param("yearinc_p.HD")
for (bc in cohorts){   # scale up incidence by year (p.HD is in age-cohort format)
  bc1 = bc-1899
  for (age in 0:34){ # increase applies to youth and young adults ages 0-25
    if ((bc+age)>=yearinc_p.HD & age>=18){ # starting in 2016
      p.HD[(age+1),bc1] = s.HD_18.25*p.HD[(age+1),bc1]
    }
    if ((bc+age)>=yearinc_p.HD & age<18){
      p.HD[(age+1),bc1] = s.HD_12.17*p.HD[(age+1),bc1]
    }
    if ((bc+age)>=yearinc_p.HD & age>=26){
      p.HD[(age+1),bc1] = s.HD_26.34*p.HD[(age+1),bc1]
    }
  }
}
yearinc_p.HD <- round(select_param("yearinc_p.HD"))

# Interaction effects
rr.ND.CD <- select_param("rr.ND.CD")
rr.CH.CD <- select_param("rr.CH.CD")
rr.CR.CD <- select_param("rr.CR.CD")
rr.CD.FD <- select_param("rr.CD.FD")

# Model prevalence
df.model_prevs <- do.call(rbind, lapply(names(l.model_prevs), function(status) {
  cbind(data.frame(l.model_prevs[[status]]), status = status)
}))

# Calibration targets
df.calibtargets <- do.call(rbind, lapply(names(l.calib_targets), function(status) {
  cbind(data.frame(l.calib_targets[[status]]), status = status)
}))

## Data visualization ------------------------------------------------------

# Figures for initiation and cessation

initprobs <- as.data.frame(cbind(c(p.NC[,100],rr.ND.CD*p.NC[,100],smk_init[,100]),c(rep("calibrated",100),rep("rr.ND.CD",100),rep("CISNET",100)),c(rep(0:99,3))))
names(initprobs) <- c("prob","inputs","age")
initprobs$prob<-as.numeric(as.character(initprobs$prob))
initprobs$age<-as.numeric(as.character(initprobs$age))
p.NC_age <- ggplot(data=initprobs) +  geom_line( aes(x=age, y=prob, linetype=inputs, color=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,50), breaks=seq(0,99,10)) +
  labs(title="Initiation probabilities")

cessprobs <- as.data.frame(cbind(c(p.CF[,100],rr.CD.FD*p.CF[,100],smk_cess[,100]),c(rep("calibrated",100),rep("rr.CD.FD",100),rep("CISNET",100)),c(rep(0:99,3))))
names(cessprobs) <- c("prob","inputs","age")
cessprobs$prob<-as.numeric(as.character(cessprobs$prob))
cessprobs$age<-as.numeric(as.character(cessprobs$age))
p.CF_age <- ggplot(data=cessprobs) +  geom_line( aes(x=age, y=prob, linetype=inputs, color=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Cessation probabilities")

ns_age <-ggplot() +
  geom_pointrange(data= subset(df.calibtargets,status=="N"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, status=="N" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("Never smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs_age <-ggplot() +
  geom_pointrange(data= subset(df.calibtargets,status=="C"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, status=="C" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("Current smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_age <-ggplot() +
  geom_pointrange(data= subset(df.calibtargets,status=="F"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, status=="F" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("Former smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


ncf_total <- ggplot() +
  geom_pointrange(data=subset(df.calibtargets,age==18.99 & (status=="N" | status=="C" | status=="F")), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, age==18.99 & (status=="N" | status=="C" | status=="F") ),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("Smoking distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


# Figure for incidence inputs
p.HD_age <- ggplot() + geom_line(aes(x=0:99,y=p.HD[,(2020-1900)],col="bc 2020")) +
  geom_line(aes(x=0:99,y=p.HD[,(1980-1900)],col="bc 1980")) +
  geom_line(aes(x=0:99,y=p.HD[,(1995-1900)],col="bc 1995")) +
  scale_y_continuous(name="Annual incidence probability (p.HD)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Incidence, calibrated estimates",color=NULL)

p.HD_ageC <- ggplot() + geom_line(aes(x=0:99,y=rr.CH.CD*p.HD[,(2020-1900)],col="bc 2020")) +
  geom_line(aes(x=0:99,y=rr.CH.CD*p.HD[,(1980-1900)],col="bc 1980")) +
  geom_line(aes(x=0:99,y=rr.CH.CD*p.HD[,(1995-1900)],col="bc 1995")) +
  scale_y_continuous(name="Annual incidence probability (p.HD, rr.CH.CD)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Incidence among current smokers, rr.CH.CD*",color=NULL)

# Figure for recovery inputs
p.DR_age <- ggplot() +  geom_line( aes(x=0:99, y=p.DR)) + 
  scale_y_continuous(name="Probability of recovery (p.DR)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recovery, calibrated estimates, p.DR" )

# Figure for recurrence inputs
p.RD_age <- ggplot() +  geom_line( aes(x=0:99, y=p.RD)) + geom_point(aes(x=0:99),y=rr.CR.CD*p.RD)+
  scale_y_continuous(name="Probability of recurrence (p.RD)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recurrence, calibrated estimates, rr.CR.CD")

D_age <-ggplot() +
  geom_pointrange(data= subset(df.calibtargets,status=="D"&age!=18.99), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), 
                      shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, status=="D" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.4),breaks=seq(0,0.4,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("Current MDE - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

D_total <- ggplot() +
  geom_pointrange(data=subset(df.calibtargets, status=="D" & age==18.99), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, status=="D" & age==18.99),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3), breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("MDE distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


# Figures for depressed population
ns_ageD <-ggplot() +
  geom_pointrange(data= subset(df.calibtargets,status=="ND"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, status=="ND" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("Never smokers - deppop, ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs_ageD <-ggplot() +
  geom_pointrange(data= subset(df.calibtargets,status=="CD"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, status=="CD" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("Current smokers - deppop, ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_ageD <-ggplot() +
  geom_pointrange(data= subset(df.calibtargets,status=="FD"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, status=="FD" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("Former smokers - deppop, ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


ncf_totalD <- ggplot() +
  geom_pointrange(data=subset(df.calibtargets,age==18.99 & (status=="ND" | status=="CD" | status=="FD")), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, age==18.99 & (status=="ND" | status=="CD" | status=="FD")),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,calib_endyear),breaks=seq(calib_startyear,calib_endyear,1))  +
  labs(title=paste0("Smoking distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

# Figures for mortality by smoking and dep status  
Xprobs <- as.data.frame(cbind(c(p.NX[,100],p.CX[,100],a_p.FX.ysq[,100,5]),c(rep("p.NX",100),rep("p.CX",100),rep("p.FX.ysq",100)),c(rep(0:99,3))))
names(Xprobs) <- c("prob","status","age")
Xprobs$prob <- as.numeric(Xprobs$prob)
Xprobs$age <- as.numeric(Xprobs$age)
Xprobs_age <- ggplot(data=Xprobs) +  geom_line( aes(x=age, y=prob, color=status)) + 
  geom_line(aes(x=age,y=prob,color=status),linetype=2)+
  scale_color_manual(values=c('red', 'blue', 'springgreen3'))+
  scale_y_continuous(name="Annual mortality by smoking status (rr.DX)", limits=c(0,1), breaks=seq(0,1,0.1)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Mortality probabilities by smoking and MDE status")


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

df.calib <- merge(as.data.frame(v.params),as.data.frame(m.calib_inputs),by="row.names",all.x=TRUE,all.y=TRUE,sort=FALSE)
colnames(df.calib)[1:3] <- c("parameters", "est","initial")

pdf(file = paste0(mainDir,"output/", whichgender,"_mds_calib_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
plot.new()
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration fit values", font=2, cex=1.5)
grid.table(cbind(c(v.gof, loglik_value), c(v.ssd, ssd_value)),rows=c(names(l.calib_targets),"Overall Fit"), cols=c("loglik","sum of sq diffs"))
plot.new()
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration parameters", font=2, cex=1.5)
grid.table(m.calib_inputs)
grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking inputs")
grid_arrange_shared_legend(list(ns_age, cs_age, fs_age),3,"Smoking distribution")
ncf_total
grid_arrange_shared_legend(list(p.HD_age,p.HD_ageC),2,"Incidence by smoking status")
grid.arrange(p.DR_age,p.RD_age,ncol=2)
grid_arrange_shared_legend(list(D_age, D_total),2,"MDE")
grid_arrange_shared_legend(list(ns_ageD, cs_ageD, fs_ageD),3,"Smoking distribution among people with depression")
grid_arrange_shared_legend(list(ncf_totalD, Xprobs_age),2,"NCF")
dev.off()
