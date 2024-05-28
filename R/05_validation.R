## VALIDATION
# Internal validation to compare model-predicted outputs evaluated at calibrated parameters vs the calibration targets

# Run the model ---------------------------------------------------
  
model_res<-main_calib(v.params)

v.GOF <- numeric(n.target)   # Calculate goodness-of-fit of model outputs to targets
for (r in 1:length(lst_calibtargets)){ # sum of squared differences
  gof<- sum((lst_calibtargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2) # prevalence by age group
  v.GOF[r] <-gof 
}
names(v.GOF) <- paste0(names(lst_targets),".fit_value")
fit_value <- sum(v.GOF)
print(fit_value)
print(v.GOF)

# Data visualization ------------------------------------------------------

## Figures for initiation and cessation
s.NC_9.17 <- ifelse(calib_inputs["s.NC_9.17","calib"]==1,v.params["s.NC_9.17"],calib_inputs["s.NC_9.17","value"])
s.NC_18.25 <- ifelse(calib_inputs["s.NC_18.25","calib"]==1,v.params["s.NC_18.25"],calib_inputs["s.NC_18.25","value"])

s.CF_18.25 <- ifelse(calib_inputs["s.CF_18.25","calib"]==1,v.params["s.CF_18.25"],calib_inputs["s.CF_18.25","value"])
s.CF_26.34 <- ifelse(calib_inputs["s.CF_26.34","calib"]==1,v.params["s.CF_26.34"],calib_inputs["s.CF_26.34","value"])
s.CF_35.49 <- ifelse(calib_inputs["s.CF_35.49","calib"]==1,v.params["s.CF_35.49"],calib_inputs["s.CF_35.49","value"])
s.CF_50.64 <- ifelse(calib_inputs["s.CF_50.64","calib"]==1,v.params["s.CF_50.64"],calib_inputs["s.CF_50.64","value"])
s.CF_65.99 <- ifelse(calib_inputs["s.CF_65.99","calib"]==1,v.params["s.CF_65.99"],calib_inputs["s.CF_65.99","value"])

# Recovery
p.DR=NULL
p.DR[1:12] <- p.DR[100] <- 0 # final value = 0 because mortality prob = 1
p.DR[13:18] <- ifelse(calib_inputs["p.DR_12.17","calib"]==1,v.params["p.DR_12.17"],calib_inputs["p.DR_12.17","value"])
p.DR[19:26] <- ifelse(calib_inputs["p.DR_18.25","calib"]==1,v.params["p.DR_18.25"],calib_inputs["p.DR_18.25","value"])
p.DR[27:35] <- ifelse(calib_inputs["p.DR_26.34","calib"]==1,v.params["p.DR_26.34"],calib_inputs["p.DR_26.34","value"])
p.DR[36:50] <- ifelse(calib_inputs["p.DR_35.49","calib"]==1,v.params["p.DR_35.49"],calib_inputs["p.DR_35.49","value"])
p.DR[51:65] <- ifelse(calib_inputs["p.DR_50_64","calib"]==1,v.params["p.DR_50_64"],calib_inputs["p.DR_50_64","value"])
p.DR[66:99] <- ifelse(calib_inputs["p.DR_65_99","calib"]==1,v.params["p.DR_65_99"],calib_inputs["p.DR_65_99","value"])

# Recurrence
p.RD = NULL
p.RD[1:12] <- p.RD[100] <- 0 # final value = 0 because mortality prob = 1
p.RD[13:18] <- ifelse(calib_inputs["p.RD_12.17","calib"]==1,v.params["p.RD_12.17"],calib_inputs["p.RD_12.17","value"])
p.RD[19:26] <- ifelse(calib_inputs["p.RD_18.25","calib"]==1,v.params["p.RD_18.25"],calib_inputs["p.RD_18.25","value"])
p.RD[27:35] <- ifelse(calib_inputs["p.RD_26.34","calib"]==1,v.params["p.RD_26.34"],calib_inputs["p.RD_26.34","value"])
p.RD[36:50] <- ifelse(calib_inputs["p.RD_35.49","calib"]==1,v.params["p.RD_35.49"],calib_inputs["p.RD_35.49","value"])
p.RD[51:65] <- ifelse(calib_inputs["p.RD_50_64","calib"]==1,v.params["p.RD_50_64"],calib_inputs["p.RD_50_64","value"])
p.RD[66:99]  <- ifelse(calib_inputs["p.RD_65_99","calib"]==1,v.params["p.RD_65_99"],calib_inputs["p.RD_65_99","value"])

s.HD_12.17 <-  ifelse(calib_inputs["s.HD_12.17","calib"]==1,v.params["s.HD_12.17"],calib_inputs["s.HD_12.17","value"])
s.HD_18.25 <-  ifelse(calib_inputs["s.HD_18.25","calib"]==1,v.params["s.HD_18.25"],calib_inputs["s.HD_18.25","value"])
s.HD_26.34 <-  ifelse(calib_inputs["s.HD_26.34","calib"]==1,v.params["s.HD_26.34"],calib_inputs["s.HD_26.34","value"])

rr.DX_18.25 <- ifelse(calib_inputs["rr.DX_18.25","calib"]==1,v.params["rr.DX_18.25"],calib_inputs["rr.DX_18.25","value"])
rr.DX_26.34 <- ifelse(calib_inputs["rr.DX_26.34","calib"]==1,v.params["rr.DX_26.34"],calib_inputs["rr.DX_26.34","value"])
rr.DX_35.49 <- ifelse(calib_inputs["rr.DX_35.49","calib"]==1,v.params["rr.DX_35.49"],calib_inputs["rr.DX_35.49","value"])
rr.DX_50.64 <- ifelse(calib_inputs["rr.DX_50.64","calib"]==1,v.params["rr.DX_50.64"],calib_inputs["rr.DX_50.64","value"])
rr.DX_65.99 <- ifelse(calib_inputs["rr.DX_65.99","calib"]==1,v.params["rr.DX_65.99"],calib_inputs["rr.DX_65.99","value"])

rr.ND.CD <- ifelse(calib_inputs["rr.ND.CD","calib"]==1,v.params["rr.ND.CD"],calib_inputs["rr.ND.CD","value"])
rr.CH.CD <- ifelse(calib_inputs["rr.CH.CD","calib"]==1,v.params["rr.CH.CD"],calib_inputs["rr.CH.CD","value"])
rr.CR.CD <- ifelse(calib_inputs["rr.CR.CD","calib"]==1,v.params["rr.CR.CD"],calib_inputs["rr.CR.CD","value"])
rr.CD.FD <- ifelse(calib_inputs["rr.CD.FD","calib"]==1,v.params["rr.CD.FD"],calib_inputs["rr.CD.FD","value"])

yearinc_p.HD <- ifelse(calib_inputs["yearinc_p.HD","calib"]==1,v.params["yearinc_p.HD"],calib_inputs["yearinc_p.HD","value"])
yearinc_p.HD <- round(yearinc_p.HD)

## Initiation - No initiation after 25
p.NC = smk_init*c(rep(s.NC_9.17,18),rep(s.NC_18.25,8),rep(0,74))

## Cessation - No cessation before 18
p.CF = smk_cess*c(rep(0,16),rep(s.CF_18.25,10), rep(s.CF_26.34,9),rep(s.CF_35.49,15),rep(s.CF_50.64,15),rep(s.CF_65.99,35))

rr.DX = c(rep(1,18),rep(rr.DX_18.25,8),rep(rr.DX_26.34,9),rep(rr.DX_35.49,15),rep(rr.DX_50.64,15),rep(rr.DX_65.99,34),1)
modelprev <- rbind(cbind(data.frame(model_res$N),status="neversmoker"),
                   cbind(data.frame(model_res$C),status="currentsmoker"),
                   cbind(data.frame(model_res$F),status="formersmoker"),
                   cbind(data.frame(model_res$D),status="depressed"),
                   cbind(data.frame(model_res$ND),status="neversmokerD"),
                   cbind(data.frame(model_res$CD),status="currentsmokerD"),
                   cbind(data.frame(model_res$FD),status="formersmokerD"))

calibtargets = rbind(cbind(data.frame(lst_targets[["N"]]),status="neversmoker"),
                     cbind(data.frame(lst_targets[["C"]]),status="currentsmoker"),
                     cbind(data.frame(lst_targets[["F"]]),status="formersmoker"),
                     cbind(data.frame(lst_targets[["D"]]),status="depressed"),
                     cbind(data.frame(lst_targets[["ND"]]),status="neversmokerD"),
                     cbind(data.frame(lst_targets[["CD"]]),status="currentsmokerD"),
                     cbind(data.frame(lst_targets[["FD"]]),status="formersmokerD"))

p.NCsmk_init <- as.data.frame(cbind(c(p.NC[,100],rr.ND.CD*p.NC[,100],smk_init[,100]),c(rep("calibrated",100),rep("rr.ND.CD",100),rep("CISNET",100)),c(rep(0:99,3))))
names(p.NCsmk_init) <- c("prob","inputs","age")
p.NCsmk_init$prob<-as.numeric(as.character(p.NCsmk_init$prob))
p.NCsmk_init$age<-as.numeric(as.character(p.NCsmk_init$age))
p.NC_age <- ggplot(data=p.NCsmk_init) +  geom_line( aes(x=age, y=prob, linetype=inputs, color=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Initiation probabilities")

p.CFsmk_cess <- as.data.frame(cbind(c(p.CF[,100],rr.CD.FD*p.CF[,100],smk_cess[,100]),c(rep("calibrated",100),rep("rr.CD.FD",100),rep("CISNET",100)),c(rep(0:99,3))))
names(p.CFsmk_cess) <- c("prob","inputs","age")
p.CFsmk_cess$prob<-as.numeric(as.character(p.CFsmk_cess$prob))
p.CFsmk_cess$age<-as.numeric(as.character(p.CFsmk_cess$age))
p.CF_age <- ggplot(data=p.CFsmk_cess) +  geom_line( aes(x=age, y=prob, linetype=inputs, color=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Cessation probabilities")

ns_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="neversmoker"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="neversmoker" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Never smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="currentsmoker"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="currentsmoker" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Current smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="formersmoker"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="formersmoker" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Former smokers - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


ncf_total <- ggplot() +
  geom_pointrange(data=subset(calibtargets,age==18.99 & (status=="neversmoker" | status=="currentsmoker" | status=="formersmoker")), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, age==18.99 & (status=="neversmoker" | status=="currentsmoker" | status=="formersmoker") ),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Smoking distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


## Figure for incidence inputs
## Incidence
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

## Figure for recovery inputs
p.DR_age <- ggplot() +  geom_line( aes(x=0:99, y=p.DR)) + 
  scale_y_continuous(name="Probability of recovery (p.DR)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recovery, calibrated estimates, p.DR" )

## Figure for recurrence inputs
p.RD_age <- ggplot() +  geom_line( aes(x=0:99, y=p.RD)) + geom_point(aes(x=0:99),y=rr.CR.CD*p.RD)+
  scale_y_continuous(name="Probability of recurrence (p.RD)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recurrence, calibrated estimates, rr.CR.CD")

D_age <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="depressed"&age!=18.99), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), 
                      shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="depressed" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.4),breaks=seq(0,0.4,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Current MDE - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

D_total <- ggplot() +
  geom_pointrange(data=subset(calibtargets, status=="depressed" & age==18.99), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="depressed" & age==18.99),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3), breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("MDE distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


## Figures for depressed population
ns_ageD <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="neversmokerD"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="neversmokerD" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Never smokers - deppop, ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs_ageD <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="currentsmokerD"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="currentsmokerD" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Current smokers - deppop, ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_ageD <-ggplot() +
  geom_pointrange(data= subset(calibtargets,status=="formersmokerD"&age!=18.99), aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="formersmokerD" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Former smokers - deppop, ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


ncf_totalD <- ggplot() +
  geom_pointrange(data=subset(calibtargets,age==18.99 & (status=="neversmokerD" | status=="currentsmokerD" | status=="formersmokerD")), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, age==18.99 & (status=="neversmokerD" | status=="currentsmokerD" | status=="formersmokerD")),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Smoking distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())


## Figures for mortality by smoking and dep status  
p.NCFX <- as.data.frame(cbind(c(p.NX[,100],p.CX[,100],a_p.FX.ysq[,100,5]),c(rep("NX",100),rep("CX",100),rep("FX",100)),c(rep(0:99,3))))
names(p.NCFX) <- c("prob","status","age")
p.NCFX$prob <- as.numeric(p.NCFX$prob)
p.NCFX$age <- as.numeric(p.NCFX$age)
p.NCFX_age <- ggplot(data=p.NCFX) +  geom_line( aes(x=age, y=prob, color=status)) + 
  geom_line(aes(x=age,y=prob*rr.DX,linetype=status,color=status))+
  scale_y_continuous(name="Annual mortality by smoking status (rr.DX)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,99,10)) +
  labs(title="Mortality probabilities by smoking and MDE status, rr.DX")


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

df.calib <- merge(as.data.frame(v.params),as.data.frame(calib_inputs),by="row.names",all.x=TRUE,all.y=TRUE,sort=FALSE)
colnames(df.calib)[1:3] <- c("parameters", "est","initial")

pdf(file = paste0(mainDir,"output/", whichgender,"_mds_calib_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I:%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
plot.new()
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration fit values", font=2, cex=1.5)
grid.table(c(v.GOF, fit_value),rows=c(names(lst_targets),"Overall Fit"))
plot.new()
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration parameters", font=2, cex=1.5)
grid.table(df.calib)
grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking inputs")
grid_arrange_shared_legend(list(ns_age, cs_age, fs_age),3,"Smoking distribution")
ncf_total
grid_arrange_shared_legend(list(p.HD_age,p.HD_ageC),2,"Incidence by smoking status")
grid.arrange(p.DR_age,p.RD_age,ncol=2)
grid_arrange_shared_legend(list(D_age, D_total),2,"MDE")
grid_arrange_shared_legend(list(ns_ageD, cs_ageD, fs_ageD),3,"Smoking distribution among people with depression")
grid_arrange_shared_legend(list(ncf_totalD, p.NCFX_age),2,"NCF")
dev.off()
