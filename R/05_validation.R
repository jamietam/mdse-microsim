## VALIDATION
# Internal validation to compare model-predicted outputs evaluated at calibrated parameters vs the calibration targets

## Run the model ---------------------------------------------------  
l.model_prevs<-main_calib(v.params,NULL)[[2]]

# Apply filtering to relevant e-cig states ages <50 to avoid NA / Inf log likelihood values
l.calib_targets_under_50 <- l.calib_targets
l.model_prevs_under_50 <- l.model_prevs
l.calib_targets_under_50[c("NE","FE", "E_D", "NE_D", "CE_D", "FE_D")] <- lapply(l.calib_targets_under_50[c("NE","FE", "E_D", "NE_D", "CE_D", "FE_D")], filter_under_50)
l.model_prevs_under_50[c("NE","FE", "E_D", "NE_D", "CE_D", "FE_D")] <- lapply(l.model_prevs_under_50[c("NE","FE", "E_D", "NE_D", "CE_D", "FE_D")], filter_under_50)

v.gof <- v.ssd <- numeric(n.target)   # Calculate goodness-of-fit of model outputs to targets

for (r in 1:length(l.calib_targets)){ # sum of squared differences - removes model years after 2022 (where we don't have NSDUH data for calibration)
  if (names(l.calib_targets)[r] %in% c("NE","FE", "E_D", "NE_D", "CE_D", "FE_D")){
    gof = gof_norm_loglike(target_mean = l.calib_targets_under_50[[r]][,"prev"],
                           model_output = subset(l.model_prevs_under_50[[r]],l.model_prevs_under_50[[r]][,"year"]>=calib_startyear & l.model_prevs_under_50[[r]][,"year"]<=endyear)[,"prev"],
                           target_sd = l.calib_targets_under_50[[r]][,"se"])
    
  } else {
    gof = gof_norm_loglike(target_mean = l.calib_targets[[r]][,"prev"],
                         model_output = subset(l.model_prevs[[r]],l.model_prevs[[r]][,"year"]>=calib_startyear & l.model_prevs[[r]][,"year"]<=endyear)[,"prev"],
                         target_sd = l.calib_targets[[r]][,"se"])
  }
  ssd <- sum((l.calib_targets[[r]][,"prev"] - subset(l.model_prevs[[r]],l.model_prevs[[r]][,2]>=calib_startyear & l.model_prevs[[r]][,2]<=endyear)[,"prev"])^2) # prevalence by age group
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

s.NC_D_9.17 <- select_param("s.NC_D_9.17")
s.NC_D_18.25 <- select_param("s.NC_D_18.25")
s.NC_D_26.34 <- select_param("s.NC_D_26.34")
rr.CD.FD <- select_param("rr.CD.FD")
p.NC_D = smk_init*c(rep(s.NC_D_9.17,18),rep(s.NC_D_18.25,8),rep(s.NC_D_26.34,9),rep(0,65))


# Recovery
p.DR <- rep(0,100)
p.DR[13:65] <- select_param("p.DR_12.64")
p.DR[66:99] <- select_param("p.DR_65.99")

# Vaping transition probabilities
p.NO.NE[13:18,c("2020","2021")] <- select_param("p.NO.NE_20.21_12.17")
p.NO.NE[13:18,paste0(2022:endyear)] <- select_param("p.NO.NE_22.23_12.17")

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

# Interaction effects
rr.CH.CD <- select_param("rr.CH.CD")
rr.CR.CD <- select_param("rr.CR.CD")


# Model prevalence
df.model_prevs <- do.call(rbind, lapply(names(l.model_prevs), function(status) {
  cbind(data.frame(l.model_prevs[[status]]), status = status)
}))

##sum prevalences to create the states for exclvap, exclsmk, and neither
# Ensure 'prev' column is numeric
df.model_prevs$prev[df.model_prevs$prev == ""] <- NA
df.model_prevs$prev <- as.numeric(as.character(df.model_prevs$prev))

# Extract column names from df.model_prevs
column_names <- names(df.model_prevs)

# Loop through years and ages
for(y in c(2020:endyear)) { # loop through years
  for(a in c(18.25, 26.34, 35.49, 50.64, 65.99, 18.99)) { # loop through ages
    # neither
    neither <- sum(subset(df.model_prevs, year == y & age == a & (status == "NO" | status == "NQ" | status == "FO" | status == "FQ"))[,"prev"], na.rm = TRUE)
    new_row_neither <- data.frame(age = a, year = y, prev = neither, counts = NA, alive = NA, dead = NA, status = "neither", stringsAsFactors = FALSE)
    names(new_row_neither) <- column_names
    df.model_prevs <- rbind(df.model_prevs, new_row_neither)
    
    # exclvap
    exclvap <- sum(subset(df.model_prevs, year == y & age == a & (status == "NE" | status == "FE"))[,"prev"], na.rm = TRUE)
    new_row_exclvap <- data.frame(age = a, year = y, prev = exclvap, counts = NA, alive = NA, dead = NA, status = "exclvap", stringsAsFactors = FALSE)
    names(new_row_exclvap) <- column_names
    df.model_prevs <- rbind(df.model_prevs, new_row_exclvap)
    
    # exclsmk
    exclsmk <- sum(subset(df.model_prevs, year == y & age == a & (status == "CO" | status == "CQ"))[,"prev"], na.rm = TRUE)
    new_row_exclsmk <- data.frame(age = a, year = y, prev = exclsmk, counts = NA, alive = NA, dead = NA, status = "exclsmk", stringsAsFactors = FALSE)
    names(new_row_exclsmk) <- column_names
    df.model_prevs <- rbind(df.model_prevs, new_row_exclsmk)
  }
}

# Calibration targets
df.calib_targets <- do.call(rbind, lapply(names(l.calib_targets), function(status) {
  cbind(data.frame(l.calib_targets[[status]]), status = status)
}))


# Calibration targets
# df.brfss_targets <- do.call(rbind, lapply(names(l.brfss_targets), function(status) {
#   cbind(data.frame(l.brfss_targets[[status]]), status = status)
# }))

#turn all numbers into numeric values
df.calib_targets$survey_year <- as.numeric(df.calib_targets$survey_year)
df.model_prevs$year <- as.numeric(df.model_prevs$year)
cohorts <- as.numeric(cohorts)
df.calib_targets$prev <- as.numeric(df.calib_targets$prev)
df.calib_targets$prev_lowCI <- as.numeric(df.calib_targets$prev_lowCI)
df.calib_targets$prev_highCI <- as.numeric(df.calib_targets$prev_highCI)
## Data visualization ------------------------------------------------------


# Model inputs ------------------------------------------------------------

# Figures for initiation and cessation for smoking
initprobs <- as.data.frame(cbind(c(p.NC[,100],p.NC_D[,100],smk_init[,100]),c(rep("No MDE",100),rep("MDE",100),rep("CISNET (NHIS)",100)),c(rep(0:99,3))))
names(initprobs) <- c("prob","inputs","age")
initprobs$prob<-as.numeric(as.character(initprobs$prob))
initprobs$age<-as.numeric(as.character(initprobs$age))
p.NC_age <- ggplot(data=initprobs) +  geom_line( aes(x=age, y=prob, linetype=inputs, color=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,50), breaks=seq(0,100,10)) + theme_light() +
  scale_color_manual(values=c("red","black","gray"))+
  labs(title="Initiation probabilities")+ylab("Annual probability")

cessprobs <- as.data.frame(cbind(c(p.CF[,100],rr.CD.FD*p.CF[,100],smk_cess[,100]),c(rep("No MDE",100),rep("MDE",100),rep("CISNET (NHIS)",100)),c(rep(0:99,3))))
names(cessprobs) <- c("prob","inputs","age")
cessprobs$prob<-as.numeric(as.character(cessprobs$prob))
cessprobs$age<-as.numeric(as.character(cessprobs$age))
p.CF_age <- ggplot(data=cessprobs) +  geom_line( aes(x=age, y=prob, linetype=inputs, color=inputs)) + 
  scale_x_continuous(name="Age", limits=c(0,99), breaks=seq(0,100,10)) + theme_light()+
  scale_color_manual(values=c("red","black","gray"))+
  labs(title="Cessation probabilities")+ylab("Annual probability")

# Figure for incidence inputs
p.HD_age <- ggplot() + geom_line(aes(x=0:99,y=p.HD[,(2020-1900)],col="bc 2020")) +
  geom_line(aes(x=0:99,y=p.HD[,(1980-1900)],col="bc 1980")) +
  geom_line(aes(x=0:99,y=p.HD[,(1995-1900)],col="bc 1995")) +
  scale_y_continuous(name="Annual incidence probability (p.HD)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Incidence of Depression, calibrated estimates",color=NULL)

p.HD_ageC <- ggplot() + geom_line(aes(x=0:99,y=rr.CH.CD*p.HD[,(2020-1900)],col="bc 2020")) +
  geom_line(aes(x=0:99,y=rr.CH.CD*p.HD[,(1980-1900)],col="bc 1980")) +
  geom_line(aes(x=0:99,y=rr.CH.CD*p.HD[,(1995-1900)],col="bc 1995")) +
  scale_y_continuous(name="Annual incidence probability (p.HD*rr.CH.CD)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Incidence of Depression among current smokers",color=NULL)

# Figure for recovery inputs
p.DR_age <- ggplot() +  geom_line( aes(x=0:99, y=p.DR)) + 
  scale_y_continuous(name="Probability of recovery (p.DR)", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recovery, calibrated estimates, p.DR" )

# Figure for recurrence inputs
p.RD_age <- ggplot() +  geom_line( aes(x=0:99, y=p.RD,color='black')) + geom_point(aes(x=0:99,y=rr.CR.CD*p.RD,color='red'))+
  scale_y_continuous(name="Probability of recurrence", limits=c(0,1), breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="Recurrence among Smokers and non-smokers, calibrated estimates")+
  scale_colour_manual(values =c('black'='black','red'='red'), labels = c('non smokers: p.RD','smokers: p.RD*rr.CR.CD'))+
  theme(legend.position="bottom")

# Figure for e-cig transitions
eciginit <- as.data.frame(c(p.NO.NE[,"2021"],p.NO.NE[,"2023"],p.CO.CE[,"2021"],p.CO.CE[,"2023"],p.FO.FE[,"2021"],p.FO.FE[,"2023"]))
eciginit <- cbind(rep(seq(0:99),6),
                  rep(c(2021, 2023), each = 100, times = 3),
                  eciginit,c(rep("p.NO.NE",200),rep("p.CO.CE",200),rep("p.FO.FE",200)))
names(eciginit) <- c('age', 'year', 'value','prob')
p.OE_age <- ggplot(data=eciginit) +  geom_line(aes(x=age,y=value,color=prob,linetype = factor(year)))+
  scale_y_continuous(name="Probability of e-cig initiation", limits=c(0,1),seq(0,1,0.05)) +
  scale_x_continuous(name="Age", limits=c(0,99), breaks=c(0,12,26,36,50,65,100)) +
  labs(title="E-cig initiation, 2021 vs 2023")

# Figures for mortality by smoking and dep status  
Xprobs <- as.data.frame(cbind(c(p.NX[,100],p.CX[,100],a_p.FX.ysq[,100,5]),c(rep("Never",100),rep("Current",100),rep("Former",100)),c(rep(0:99,3))))
names(Xprobs) <- c("prob","status","age")
Xprobs$prob <- as.numeric(Xprobs$prob)
Xprobs$age <- as.numeric(Xprobs$age)
Xprobs_age <- ggplot(data=Xprobs) +  geom_line( aes(x=age, y=prob, color=status)) + 
  geom_line(aes(x=age,y=prob,color=status),linetype=2)+
  scale_color_manual(values=c('#990000', 'blue3','#66FF66'))+
  scale_y_continuous(name="Annual mortality by smoking status", limits=c(0,1), breaks=seq(0,1,0.1)) +
  scale_x_continuous(name="Age", limits=c(0,100), breaks=seq(0,100,10)) + theme_light()+
  labs(title="Mortality probabilities by smoking status")

# Model prevalences -------------------------------------------------------

generate_plot1 <- function(data1, data2, status_filter, age_filter, title_suffix, y_label, shape_text, whichgender, y_limits, y_breaks) {
  xmin <- ifelse(grepl("[E]", status_filter),2020, calib_startyear)
  ggplot() +
    geom_pointrange(data = subset(data1, status == status_filter & age != age_filter), 
                    aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, 
                        colour = factor(age), shape = shape_text)) +
    geom_line(data = subset(data2, status == status_filter & age != age_filter),  
              aes(x = year, y = prev, colour = factor(age))) +

    scale_y_continuous(name = y_label, limits = y_limits, breaks = y_breaks) +
    scale_x_continuous(name = "Year", limits = c(xmin, endyear), breaks = seq(calib_startyear, endyear, 1)) +
    labs(title = paste0(title_suffix, " - ", whichgender)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
}
# Distribution in total population
N_age <- generate_plot1(df.calib_targets, df.model_prevs, "N", 18.99, "Never smoked", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,1), seq(0,1,0.05))
C_age <- generate_plot1(df.calib_targets, df.model_prevs, "C", 18.99, "Current smoking", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,1), seq(0,1,0.05))
F_age <- generate_plot1(df.calib_targets, df.model_prevs, "F", 18.99, "Former smoking", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,1), seq(0,1,0.05))
D_age <- generate_plot1(df.calib_targets, df.model_prevs, "D", 18.99, "Current MDE", "Prevalence (%)",  "National Survey on Drug Use and Health", whichgender, c(0,0.3), seq(0,0.3,0.05))

E_age <- generate_plot1(df.calib_targets, df.model_prevs, "E", 18.99, "Current vaping", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,0.3), seq(0,1,0.05))
NE_age <- generate_plot1(df.calib_targets, df.model_prevs, "NE", 18.99, "NE - Total pop", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,0.3), seq(0,1,0.05))
CE_age <- generate_plot1(df.calib_targets, df.model_prevs, "CE", 18.99, "CE - Total pop", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,0.3), seq(0,1,0.05))
FE_age <- generate_plot1(df.calib_targets, df.model_prevs, "FE", 18.99, "FE - Total pop", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,0.3), seq(0,1,0.05))

# Distribution in MDE population
N_D_age <- generate_plot1(df.calib_targets, df.model_prevs, "N_D", 18.99, "N_D - MDE Pop", "Prevalence (%)",  "National Survey on Drug Use and Health", whichgender, c(0,1), seq(0,1,0.05))
C_D_age <- generate_plot1(df.calib_targets, df.model_prevs, "C_D", 18.99, "C_D - MDE Pop", "Prevalence (%)",  "National Survey on Drug Use and Health", whichgender, c(0,1), seq(0,1,0.05))
F_D_age <- generate_plot1(df.calib_targets, df.model_prevs, "F_D", 18.99, "F_D - MDE Pop", "Prevalence (%)",  "National Survey on Drug Use and Health", whichgender, c(0,1), seq(0,1,0.05))
E_D_age <- generate_plot1(df.calib_targets, df.model_prevs, "E_D", 18.99, "E_D - MDE Pop", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,0.5), seq(0,1,0.05))

NE_D_age <- generate_plot1(df.calib_targets, df.model_prevs, "NE_D", 18.99, "NE_D - MDE Pop", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,0.3), seq(0,1,0.05))
CE_D_age <- generate_plot1(df.calib_targets, df.model_prevs, "CE_D", 18.99, "CE_D - MDE Pop", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,0.3), seq(0,1,0.05))
FE_D_age <- generate_plot1(df.calib_targets, df.model_prevs, "FE_D", 18.99, "FE_D - MDE Pop", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, c(0,0.3), seq(0,1,0.05))

CED_total <- ggplot() +
  geom_pointrange(data = subset(df.calib_targets, age == 18.99 & (status == "C" |status =="E" | status=="D")), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = status, shape = "NSDUH")) +
  geom_line(data = subset(df.model_prevs, age == 18.99 & (status == "C" | status =="E" | status=="D")),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence", limits = c(0, 0.45), breaks = seq(0, 0.45, 0.05)) +
  scale_color_manual(values=c("#8B0000","#000000","#A58AFF"))+
  scale_x_continuous(name = "Year", limits = c(calib_startyear, endyear), breaks = seq(calib_startyear, endyear, 1)) +
  labs(title = paste0("Health state prevalence - ", whichgender, " ages 18-99")) + theme_light()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

CED_18.25 <- ggplot() +
  geom_pointrange(data = subset(df.calib_targets, age == 18.25 & (status == "C" |status =="E" | status=="D")), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = status, shape = "NSDUH")) +
  geom_line(data = subset(df.model_prevs, age == 18.25 & (status == "C" | status =="E" | status=="D")),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence", limits = c(0, 0.45), breaks = seq(0, 0.45, 0.05)) +
  scale_color_manual(values=c("#8B0000","#000000","#A58AFF"))+
  scale_x_continuous(name = "Year", limits = c(calib_startyear, endyear), breaks = seq(calib_startyear, endyear, 1)) +
  labs(title = paste0("Health state prevalence - ", whichgender, " ages 18-25")) + theme_light()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

NCFE_total <- ggplot() +
  geom_pointrange(data = subset(df.calib_targets, age == 18.99 & (status == "N" | status == "C" | status == "F" | status =="E")), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = status, shape = "National Survey on Drug Use and Health")) +
  geom_line(data = subset(df.model_prevs, age == 18.99 & (status == "N" | status == "C" | status == "F"| status =="E")),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0, 1), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Year", limits = c(calib_startyear, endyear), breaks = seq(calib_startyear, endyear, 1)) +
  labs(title = paste0("Tobacco use prevalence - ", whichgender, " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
D_total <- ggplot() +
  geom_pointrange(data=subset(df.calib_targets, status=="D" & age==18.99), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, status=="D" & age==18.99),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3), breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(calib_startyear,endyear),breaks=seq(calib_startyear,endyear,1))  +
  labs(title=paste0("MDE distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

NCFE_D_total <- ggplot() +
  geom_pointrange(data = subset(df.calib_targets, age == 18.99 & (status == "N_D" | status == "C_D" | status == "F_D" | status == "E_D" )), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = status, shape = "National Survey on Drug Use and Health")) +
  geom_line(data = subset(df.model_prevs, age == 18.99 & (status == "N_D" | status == "C_D" | status == "F_D" | status == "E_D")),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0, 1), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Year", limits = c(calib_startyear, endyear), breaks = seq(calib_startyear, endyear, 1)) +
  labs(title = paste0("Tobacco use prevalence - Current MDE pop ", whichgender, " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

# Vaping Distribution
E_total <- ggplot() +
  geom_line(data = subset(df.model_prevs, age==18.99 & (status == "E"| status == "NE" | status == "CE" | status == "FE") ),  aes(x=year, y= prev,color=status))+
  geom_pointrange(data = subset(df.calib_targets, age == 18.99 & (status == "E"| status == "NE" | status == "CE" | status == "FE")), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = status, shape = "National Survey on Drug Use and Health")) +
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.12),breaks=seq(0,0.12,0.005)) +
  scale_x_continuous(name="Year",limits=c(2020,endyear),breaks=seq(2020,endyear,1))  +
  labs(title=paste0("Vaping distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

E_D_total <- ggplot() +
  geom_pointrange(data=subset(df.calib_targets, status=="E_D" & age==18.99), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(df.model_prevs, age == 18.99 &  status == "E_D" ),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0, 0.3), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Year", limits = c(2020, endyear), breaks = seq(2020, endyear, 1)) +
  labs(title = paste0("Vaping distribution Among Depressed- ", whichgender, " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

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


# Create PDF output -------------------------------------------------------

df.calib <- merge(as.data.frame(v.params),as.data.frame(m.calib_inputs),by="row.names",all.x=TRUE,all.y=TRUE,sort=FALSE)
colnames(df.calib)[1:3] <- c("parameters", "est","initial")

pdf(file = paste0(mainDir,"output/", whichgender,"_mds_calib_",n.i,"_", loglik_value, "_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
# plot.new()
# text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
# text(.5, 1.0, "Calibration fit values", font=2, cex=1.5)
# grid.table(c(v.gof[1:22], fit_value),rows=c(names(l.calib_targets[1:22]),"Overall Fit"))

# Create the first table
table1 <- tableGrob(c(v.gof[1:11], loglik_value), 
                    rows = c(names(l.calib_targets[1:11]), "Overall Fit"),
                    theme = ttheme_minimal(base_size = 10))

# Create the second table
table2 <- tableGrob(c(v.gof[12:22], loglik_value), 
                    rows = c(names(l.calib_targets[12:22]), "Overall Fit"),
                    theme = ttheme_minimal(base_size = 10))

# Title for the first table
title1 <- textGrob(paste0("mds_microsim \n",whichgender), gp = gpar(fontsize = 15))
subtitle1 <- textGrob("Calibration fit values", gp = gpar(fontsize = 15, fontface = "bold"))

# Title for the second table
title2 <- textGrob(paste0("mds_microsim \n",whichgender), gp = gpar(fontsize = 15))
subtitle2 <- textGrob("Calibration fit values (Cont.)", gp = gpar(fontsize = 15, fontface = "bold"))

# Combine title and table for the first plot
table1_with_titles <- arrangeGrob(grobs = list(title1, subtitle1, table1), 
                                  nrow = 3, heights = c(0.3, 0.3, 1))

# Combine title and table for the second plot
table2_with_titles <- arrangeGrob(grobs = list(title2, subtitle2, table2), 
                                  nrow = 3, heights = c(0.3, 0.3, 1))

# Arrange the two tables side by side
grid.arrange(table1_with_titles, table2_with_titles, ncol = 2)

plot.new()
grid.table(cbind(c(v.gof, loglik_value), c(v.ssd, ssd_value)),rows=c(names(l.calib_targets),"Overall Fit"), cols=c("loglik","sum of sq diffs"))

plot.new()                        
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration parameters", font=2, cex=1.5)
grid.table(df.calib[df.calib$calib == 1, ]) #only calibrated parameters
grid_arrange_shared_legend(list(CED_total,CED_18.25),2,"")
grid_arrange_shared_legend(list(N_age, C_age, F_age),3,"Smoking distribution")
grid_arrange_shared_legend(list(D_age, D_total),2,"MDE")
grid_arrange_shared_legend(list(N_D_age, C_D_age, F_D_age),3,"Smoking distribution among people with depression")
grid_arrange_shared_legend(list(NCFE_total, NCFE_D_total),2,"Smoking distribution")
p.OE_age
grid_arrange_shared_legend(list(E_age,E_D_age),2,"Vaping by age and Depression Status")
grid_arrange_shared_legend(list(E_total,E_D_total),2,"Vaping by Smoking Status - all ages")
grid_arrange_shared_legend(list(NE_age, CE_age, FE_age),3,"Smoking and Vaping Status Age Distribution in the Total Population")
grid_arrange_shared_legend(list(NE_D_age, CE_D_age, FE_D_age),3,"Smoking and Vaping Status Age Distribution in the Depressed Population")
# inputs
grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking inputs")
grid_arrange_shared_legend(list(p.HD_age,p.HD_ageC),2,"Incidence by smoking status")
grid.arrange(p.DR_age,p.RD_age,ncol=2) # depression recover and recurrence
Xprobs_age # mortality probabilities
dev.off()
