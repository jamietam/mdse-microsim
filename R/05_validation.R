## VALIDATION
# Internal validation to compare model-predicted outputs evaluated at calibrated parameters vs the calibration targets
library(ggplot2)
library(ggnewscale)
## Run the model ---------------------------------------------------
model_res<-main_calib(v.params)

v.GOF <- numeric(n.target)   # Calculate goodness-of-fit of model outputs to targets
for (r in 1:length(model_res)){ # sum of squared differences
  gof<- sum((lst_calibtargets[[r]][,"prev"] - model_res[[r]][,"prev"])^2) # prevalence by age group
  v.GOF[r] <-gof 
}
names(v.GOF) <- paste0(names(lst_calibtargets[1:22]),".fit_value")
fit_value <- sum(v.GOF)
print(fit_value)
print(v.GOF)

## Process model inputs ----------------------------------------------------

# Helper function for parameter selection
select_param <- function(param_name) {
  ifelse(calib_inputs[param_name, "calib"] == 1, v.params[param_name], calib_inputs[param_name, "value"])
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

# Recurrence
# p.RD <- rep(0,100)
# p.RD[13:18] <- select_param("p.RD_12.17")
# p.RD[19:26] <- select_param("p.RD_18.25")
# p.RD[27:35] <- select_param("p.RD_26.34")
# p.RD[36:50] <- select_param("p.RD_35.49")
# p.RD[51:65] <- select_param("p.RD_50_64")
# p.RD[66:99] <- select_param("p.RD_65_99")

# Incidence
s.HD_12.17 <- select_param("s.HD_12.17")
s.HD_18.25 <- select_param("s.HD_18.25")
s.HD_26.34 <- select_param("s.HD_26.34")
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

# Mortality
rr.DX <- rep(1, 100)
# rr.DX[19:26] <- select_param("rr.DX_18.25")
# rr.DX[27:35] <- select_param("rr.DX_26.34")
# rr.DX[36:50] <- select_param("rr.DX_35.49")
# rr.DX[51:65] <- select_param("rr.DX_50.64")
# rr.DX[66:99] <- select_param("rr.DX_65.99")

# Interaction effects
rr.ND.CD <- select_param("rr.ND.CD")
rr.CH.CD <- select_param("rr.CH.CD")
rr.CR.CD <- select_param("rr.CR.CD")
rr.CD.FD <- select_param("rr.CD.FD")


# Model prevalence
modelprev <- do.call(rbind, lapply(names(model_res), function(status) {
  cbind(data.frame(model_res[[status]]), status = status)
}))

##sum prevalences to create the states for exclvap, exclsmk, and neither
# Ensure 'prev' column is numeric
modelprev$prev[modelprev$prev == ""] <- NA
modelprev$prev <- as.numeric(as.character(modelprev$prev))

# Extract column names from modelprev
column_names <- names(modelprev)

# Loop through years and ages
for(y in c(2005:2100)) { # loop through years
  for(a in c(18.25, 26.34, 35.49, 50.64, 65.99, 18.99)) { # loop through ages
    # neither
    neither <- sum(subset(modelprev, year == y & age == a & (status == "NO" | status == "NQ" | status == "FO" | status == "FQ"))[,"prev"], na.rm = TRUE)
    new_row_neither <- data.frame(age = a, year = y, prev = neither, counts = NA, alive = NA, dead = NA, status = "neither", stringsAsFactors = FALSE)
    names(new_row_neither) <- column_names
    modelprev <- rbind(modelprev, new_row_neither)
    
    # exclvap
    exclvap <- sum(subset(modelprev, year == y & age == a & (status == "NE" | status == "FE"))[,"prev"], na.rm = TRUE)
    new_row_exclvap <- data.frame(age = a, year = y, prev = exclvap, counts = NA, alive = NA, dead = NA, status = "exclvap", stringsAsFactors = FALSE)
    names(new_row_exclvap) <- column_names
    modelprev <- rbind(modelprev, new_row_exclvap)
    
    # exclsmk
    exclsmk <- sum(subset(modelprev, year == y & age == a & (status == "CO" | status == "CQ"))[,"prev"], na.rm = TRUE)
    new_row_exclsmk <- data.frame(age = a, year = y, prev = exclsmk, counts = NA, alive = NA, dead = NA, status = "exclsmk", stringsAsFactors = FALSE)
    names(new_row_exclsmk) <- column_names
    modelprev <- rbind(modelprev, new_row_exclsmk)
  }
}

# Calibration targets
calibtargets <- do.call(rbind, lapply(names(lst_targets), function(status) {
  cbind(data.frame(lst_targets[[status]]), status = status)
}))

#turn all numbers into numeric values
calibtargets$survey_year <- as.numeric(calibtargets$survey_year)
modelprev$year <- as.numeric(modelprev$year)
cohorts <- as.numeric(cohorts)
calibtargets$prev <- as.numeric(calibtargets$prev)
calibtargets$prev_lowCI <- as.numeric(calibtargets$prev_lowCI)
calibtargets$prev_highCI <- as.numeric(calibtargets$prev_highCI)
## Data visualization ------------------------------------------------------

# Figures for initiation and cessation for smoking

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


#Smoking distribution
generate_plot1 <- function(data1, data2, status_filter, age_filter, title_suffix, y_label, shape_text, whichgender, cohorts) {
  ggplot() +
    geom_pointrange(data = subset(data1, status == status_filter & age != age_filter), 
                    aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, 
                        colour = factor(age), shape = shape_text)) +
    geom_line(data = subset(data2, status == status_filter & age != age_filter),  
              aes(x = year, y = prev, colour = factor(age))) +
    scale_y_continuous(name = y_label, limits = c(0, 1), breaks = seq(0, 1, 0.05)) +
    scale_x_continuous(name = "Year", limits = c(2005, max(cohorts)), breaks = seq(2005, max(cohorts), 1)) +
    labs(title = paste0(title_suffix, " - ", whichgender)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
}

ns_age <- generate_plot1(calibtargets, modelprev, "N", 18.99, "Never smokers", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, cohorts)
cs_age <- generate_plot1(calibtargets, modelprev, "C", 18.99, "Current smokers", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, cohorts)
fs_age <- generate_plot1(calibtargets, modelprev, "F", 18.99, "Former smokers", "Prevalence (%)", "National Survey on Drug Use and Health", whichgender, cohorts)

ncf_total <- ggplot() +
  geom_pointrange(data = subset(calibtargets, age == 18.99 & (status == "N" | status == "C" | status == "F")), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = status, shape = "National Survey on Drug Use and Health")) +
  geom_line(data = subset(modelprev, age == 18.99 & (status == "N" | status == "C" | status == "F")),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0, 1), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Year", limits = c(2005, max(cohorts)), breaks = seq(2005, max(cohorts), 1)) +
  labs(title = paste0("Smoking distribution - ", whichgender, " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

##function for data visualization
generate_plot2 <- function(calibtargets, modelprev, cohorts, whichgender, age_range = c(18.99, 99), status_filter, title, y_limits, y_breaks, annotations = list()) {
  ggplot() +
    # NSDUH
    geom_pointrange(data = subset(calibtargets, status == status_filter$nsduh & age != age_range[1] & survey_year >= 2020), shape = 16, 
                    aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, colour = factor(age), shape = "NSDUH")) +
    scale_color_manual(name = "NSDUH", values = c("red", "blue", "green", "purple", "orange")) +
    scale_shape_manual(name = "NSDUH", values = c(1)) +
    new_scale_color() +
    # NHIS
    geom_pointrange(data = subset(calibtargets, status == status_filter$nhis & age != age_range[1]), shape = 5, 
                    aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, colour = factor(age), shape = "NHIS")) +
    scale_color_manual(name = "NHIS", values = c("red", "blue", "green", "purple", "orange")) +
    scale_shape_manual(name = "NHIS", values = c(1)) +
    new_scale_color() +
    # BRFSS
    geom_pointrange(data = subset(calibtargets, status == status_filter$brfss & age != age_range[1]), shape = 1, 
                    aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, colour = factor(age), shape = "BRFSS")) +
    scale_color_manual(name = "BRFSS", values = c("red", "blue", "green", "purple", "orange")) +
    scale_shape_manual(name = "BRFSS", values = c(1)) +
    new_scale_color() +
    # Model line
    geom_line(data = subset(modelprev, age != age_range[1] & (status == status_filter$model)), 
              aes(x = year, y = prev, color = factor(age))) +
    scale_color_manual(name = "Age Group", values = c("red", "blue", "green", "purple", "orange")) +
    # Annotations
    lapply(annotations, function(annot) {
      annotate("text", x = annot$x, y = annot$y, label = annot$label, color = annot$color, size = annot$size, hjust=0)
    }) +
    scale_y_continuous(name = "Prevalence (%)", limits = y_limits, breaks = y_breaks) +
    scale_x_continuous(name = "Year", limits = c(2005, max(cohorts)), breaks = seq(2005, max(cohorts), 1)) +
    labs(title = paste0(title, " - ", whichgender, ", ages 18-99")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

# Define the parameters for each plot
status_filters <- list(
  excl_vap = list(nsduh = "nsduhexclvap", nhis = "n_exclvap", brfss = "b_exclvap", model = "exclvap"),
  excl_smk = list(nsduh = "nsduhexclcig", nhis = "n_exclsmk", brfss = "b_exclsmk", model = "exclsmk"),
  dual = list(nsduh = "nsduhdual", nhis = "n_dual", brfss = "b_dual", model = "CE"),
  neither = list(nsduh = "nsduhneither", nhis = "n_neither", brfss = "b_neither", model = "neither"),
  nevervap = list(nsduh = "O", nhis = "nhisO", brfss = "brfssO", model = "O"),
  currentvap = list(nsduh = "E", nhis = "nhisE", brfss = "brfssE", model = "E"),
  formervap = list(nsduh = "Q", nhis = "nhisQ", brfss = "brfssQ", model = "Q"))

# add annotations
annotations <- list(
  list(list(x = 2005, y = 0.20, label = "NSDUH - Vaped within past 30 days", color = "black", size = 4),
  list(x = 2005, y = 0.19, label = "NHIS - currently vaping", color = "black", size = 4),
  list(x = 2005, y = 0.18, label = "BRFSS - every day or some days", color = "black", size = 4)),
  list(list(x = 2007, y = 0.14, label = "NSDUH - Vaped within past 30 days", color = "black", size = 5),
    list(x = 2007, y = 0.13, label = "NHIS - currently vaping", color = "black", size = 5),
    list(x = 2007, y = 0.12, label = "BRFSS - every day or some days", color = "black", size = 5))
)

# Generate the plots for Vaping
# Never Vaping
nevervap_plot <- generate_plot2(calibtargets, modelprev, cohorts, whichgender, c(18.99, 99), status_filters$nevervap,
                               "Never Vaping distribution", c(0,1), seq(0,1,0.05))
# Current Vaping
currentvap_plot <- generate_plot2(calibtargets, modelprev, cohorts, whichgender, c(18.99, 99), status_filters$currentvap,
                                 "Current Vaping distribution", c(0,0.3), seq(0,1,0.05), annotations[[1]])
# Former Vaping
formervap_plot <- generate_plot2(calibtargets, modelprev, cohorts, whichgender, c(18.99, 99), status_filters$formervap, 
                                "Former Vaping distribution", c(0,1), seq(0,1,0.05))
# Exclusive Vaping
excl_vap_plot <- generate_plot2(calibtargets, modelprev, cohorts, whichgender, c(18.99, 99), status_filters$excl_vap, 
                               "Exclusive Vaping distribution", c(0, 0.2), seq(0, 1, 0.05), annotations[[2]])
# Exclusive Smoking
excl_smk_plot <- generate_plot2(calibtargets, modelprev, cohorts, whichgender, c(18.99, 99), status_filters$excl_smk, 
                               "Exclusive Smoking distribution", c(0, 0.35), seq(0, 1, 0.05))
# Dual
dual_plot <- generate_plot2(calibtargets, modelprev, cohorts, whichgender, c(18.99, 99), status_filters$dual, 
                           "Dual Vaping and Smoking distribution", c(0, 1), seq(0, 1, 0.05))
# Neither 
neither_plot <- generate_plot2(calibtargets, modelprev, cohorts, whichgender, c(18.99, 99), status_filters$neither, 
                              "Neither Vaping and Smoking distribution", c(0, 1), seq(0, 1, 0.05))
# Vaping Distribution
v_total <- ggplot() +
  geom_line(data = subset(modelprev, age==18.99 & (status=="O" | status=="E" | status=="Q") ),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Vaping distribution - ",whichgender," ages 18-99"))+
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
  geom_pointrange(data= subset(calibtargets,status=="D"&age!=18.99), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=factor(age), 
                      shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="D" & age!=18.99),  aes(x=year, y= prev, colour=factor(age)))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.4),breaks=seq(0,0.4,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("Current MDE - ",whichgender))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

D_total <- ggplot() +
  geom_pointrange(data=subset(calibtargets, status=="D" & age==18.99), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, color=status,shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(modelprev, status=="D" & age==18.99),  aes(x=year, y= prev,color=status))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3), breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,max(cohorts)),breaks=seq(2005,max(cohorts),1))  +
  labs(title=paste0("MDE distribution - ",whichgender," ages 18-99"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

##function to generate plots
generate_plot3 <- function(data1, data2, status_filter, age_filter, title_suffix, y_label, shape_text) {
  ggplot() +
    geom_pointrange(data= subset(data1, status == status_filter & age != age_filter), 
                    aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, 
                        colour = factor(age), shape = shape_text)) +
    geom_line(data = subset(data2, status == status_filter & age != age_filter),  
              aes(x = year, y = prev, colour = factor(age))) +
    scale_y_continuous(name = y_label, limits = c(0, 1), breaks = seq(0, 1, 0.05)) +
    scale_x_continuous(name = "Year", limits = c(2005, max(cohorts)), breaks = seq(2005, max(cohorts), 1)) +
    labs(title = paste0(title_suffix, ", ", whichgender)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
}

# Figures for depressed population in relation to smoking
ns_ageD <- generate_plot3(calibtargets, modelprev, "ND", 18.99, "Never smokers - deppop", "Prevalence (%)", "National Survey on Drug Use and Health")
cs_ageD <- generate_plot3(calibtargets, modelprev, "CD", 18.99, "Current smokers - deppop", "Prevalence (%)", "National Survey on Drug Use and Health")
fs_ageD <- generate_plot3(calibtargets, modelprev, "FD", 18.99, "Former smokers - deppop", "Prevalence (%)", "National Survey on Drug Use and Health")

ncf_totalD <- ggplot() +
  geom_pointrange(data = subset(calibtargets, age == 18.99 & (status == "ND" | status == "CD" | status == "FD")), 
                  aes(x = survey_year, y = prev, ymin = prev_lowCI, ymax = prev_highCI, color = status, shape = "National Survey on Drug Use and Health")) +
  geom_line(data = subset(modelprev, age == 18.99 & (status == "ND" | status == "CD" | status == "FD")),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0, 1), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Year", limits = c(2005, max(cohorts)), breaks = seq(2005, max(cohorts), 1)) +
  labs(title = paste0("Smoking distribution - ", whichgender, " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

# Figures for depressed population in relation to vaping
nv_ageD <- generate_plot3(calibtargets, modelprev, "OD", 18.99, "Never vapers - deppop", "Prevalence (%)", "")
cv_ageD <- generate_plot3(calibtargets, modelprev, "ED", 18.99, "Current vapers - deppop", "Prevalence (%)", "")
fv_ageD <- generate_plot3(calibtargets, modelprev, "QD", 18.99, "Former vapers - deppop", "Prevalence (%)", "")

v_totalD <- ggplot() +
  geom_line(data = subset(modelprev, age == 18.99 & (status == "OD" | status == "ED" | status == "QD")),  
            aes(x = year, y = prev, color = status)) +
  scale_y_continuous(name = "Prevalence (%)", limits = c(0, 1), breaks = seq(0, 1, 0.05)) +
  scale_x_continuous(name = "Year", limits = c(2005, max(cohorts)), breaks = seq(2005, max(cohorts), 1)) +
  labs(title = paste0("Vaping distribution - ", whichgender, " ages 18-99")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())

# Figures for mortality by smoking and dep status  
Xprobs <- as.data.frame(cbind(c(p.NX[,100],p.CX[,100],a_p.FX.ysq[,100,5]),c(rep("p.NX",100),rep("p.CX",100),rep("p.FX.ysq",100)),c(rep(0:99,3))))
names(Xprobs) <- c("prob","status","age")
Xprobs$prob <- as.numeric(Xprobs$prob)
Xprobs$age <- as.numeric(Xprobs$age)
Xprobs_age <- ggplot(data=Xprobs) +  geom_line( aes(x=age, y=prob, color=status)) + 
  geom_line(aes(x=age,y=prob*rr.DX,color=status),linetype=2)+
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

df.calib <- merge(as.data.frame(v.params),as.data.frame(calib_inputs),by="row.names",all.x=TRUE,all.y=TRUE,sort=FALSE)
colnames(df.calib)[1:3] <- c("parameters", "est","initial")

pdf(file = paste0(mainDir,"output/", whichgender,"_mds_calib_",format(as.POSIXct(Sys.time()), "%m.%d.%y_%I.%M%p"),".pdf"),width=10, height=6,onefile = TRUE)
# plot.new()
# text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
# text(.5, 1.0, "Calibration fit values", font=2, cex=1.5)
# grid.table(c(v.GOF[1:22], fit_value),rows=c(names(lst_targets[1:22]),"Overall Fit"))

# Divide the plotting area into 2 columns
par(mfrow=c(1,2))

# Display the first half of the table
plot.new()
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration fit values", font=2, cex=1.5)
grid.table(c(v.GOF[1:11], fit_value), rows=c(names(lst_targets[1:11]), "Overall Fit"))

# Display the second half of the table
plot.new()
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration fit values (Cont.)", font=2, cex=1.5)
grid.table(c(v.GOF[12:22], fit_value), rows=c(names(lst_targets[12:22]), "Overall Fit"))


plot.new()
text(.9, 0.5, paste0("mds_microsim \n",whichgender), font=1, cex=1.5)
text(.5, 1.0, "Calibration parameters", font=2, cex=1.5)
grid.table(df.calib)
grid_arrange_shared_legend(list(p.NC_age,p.CF_age),2,"Smoking inputs")
grid_arrange_shared_legend(list(ns_age, cs_age, fs_age),3,"Smoking distribution")
ncf_total
grid_arrange_shared_legend(list(nevervap_plot, currentvap_plot, formervap_plot),3,"Vaping distribution")
v_total
grid_arrange_shared_legend(list(p.HD_age,p.HD_ageC),2,"Incidence by smoking status")
grid.arrange(p.DR_age,p.RD_age,ncol=2)
grid_arrange_shared_legend(list(D_age, D_total),2,"MDE")
grid_arrange_shared_legend(list(ns_ageD, cs_ageD, fs_ageD),3,"Smoking distribution among people with depression")
grid_arrange_shared_legend(list(ncf_totalD, Xprobs_age),2,"NCF Smoking")
grid_arrange_shared_legend(list(nv_ageD, cv_ageD, fv_ageD),3,"Vaping distribution among people with depression")
grid_arrange_shared_legend(list(v_totalD),3,"OEQ Vaping")
dev.off()

