rm(list = ls())  # remove any variables in R's memory
# mainDir <- "C:/Users/JT936/Dropbox/GitHub/Microsimulation-tutorial"
mainDir <- "C:/Users/jamietam/Dropbox/GitHub/smk-dep-model"
setwd(file.path(mainDir))
namethisrun <- "11.23.2021"

# Convert matrix from cohort-age to cohort-year
m.cohortbyyear <- matrix(nrow = n.i*length(cohorts), ncol = 301)
for (b in 1:length(cohorts)){
  m.cohortbyyear[(n.i*(b-1)+1):(n.i*b),b:(100+b)] <- m.cohortbyage[(n.i*(b-1)+1):(n.i*b),]
}
colnames(m.cohortbyyear) <- c(1900:2200)
rownames(m.cohortbyyear) <- paste(sort(rep(cohorts,n.i)),1:n.i, sep = ".") 


# Get counts/prevalence of individuals in a health state by age group and year
generate_prev_counts <- function(state,m.cohortbyyear,minyear,maxyear){
  agerownames<-c("18to25", "26to34", "35to49", "50to64",  "65plus", "total")
  agegroupstart <- c(18,26,35,50,65,18)
  agegroupend <- c(25,34,49,64,99,99)
  m.M.prevs <- NULL 
  for (age in 1:length(agegroupstart)){
    for (year in minyear:maxyear){
      cohortmin = year-agegroupend[age]
      if(cohortmin<1900) {next}
      cohortmax = year-agegroupstart[age]
      select = m.cohortbyyear[(n.i*(cohortmin-1900)+1):(n.i*(cohortmax-1900)+n.i),paste(year)] # birth cohort 1905 begins in row 26, and birth cohort 1912 ends in row 65
      alive <- sum(select!="X",na.rm=TRUE)
      dead <- sum(select=="X",na.rm=TRUE) 
      counts <- sum(str_count(select,state),na.rm=TRUE)
      prev <- sum(str_count(select,state),na.rm=TRUE)/sum(select!="X",na.rm=TRUE)
      m.M.prevs<-rbind(m.M.prevs,c(state,whichgender,agerownames[age],year, prev,counts,alive,dead))
    }
  }
  colnames(m.M.prevs)<-c("state","gender","agegroup","year", "prev","counts","alive","dead")
  return(m.M.prevs) 
}
m.M.prevs <- NULL 
for (i in c(v.n,"N","C","F","H","D","U","R")){
  m.M.prevs = rbind(m.M.prevs, generate_prev_counts(i, m.cohortbyyear,1900,2100))
}

library(ggplot2)
load("C:/Users/jamietam/Dropbox/Analysis/NSDUH/depsmkprevs_2005-2020.rda")

nsduh <- subset(depsmkprevs_by_year, gender==whichgender & status=="currentsmoker" & subpopulation=="totalpop")

model <- as.data.frame(m.M.prevs)
model$prev <- as.numeric(model$prev)
model$year <- as.numeric(model$year)

cs_age <-ggplot() +
  geom_pointrange(data= subset(depsmkprevs_by_year, gender==whichgender & status=="currentsmoker" & subpopulation=="totalpop" &age!="total"), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender &agegroup!="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.4),breaks=seq(0,0.4,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

cs <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="currentsmoker" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.4),breaks=seq(0,0.4,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current smokers - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs_age <-ggplot() +
  geom_pointrange(data= subset(depsmkprevs_by_year, gender==whichgender & status=="formersmoker" & subpopulation=="totalpop" &age!="total"), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender &agegroup!="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.8),breaks=seq(0,0.8,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Former smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

fs <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="formersmoker" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.8),breaks=seq(0,0.8,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Former smokers - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

ns_age <-ggplot() +
  geom_pointrange(data= subset(depsmkprevs_by_year, gender==whichgender & status=="neversmoker" & subpopulation=="totalpop" &age!="total"), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender &agegroup!="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="never smokers - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

ns <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="neversmoker" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="C"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,1),breaks=seq(0,1,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="never smokers - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dep_age <-ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="dep" & subpopulation=="totalpop" & age!="total"), 
                  aes(x = survey_year, y = prev, ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="D"&gender==whichgender &agegroup!="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3),breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current MDE - Women ")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())

dep <- ggplot() +
  geom_pointrange(data=subset(depsmkprevs_by_year, gender==whichgender & status=="dep" & subpopulation=="totalpop" & age=="total"), 
                  aes(x = survey_year, y = prev,ymin=prev_lowCI, ymax=prev_highCI, colour=age, shape="National Survey on Drug Use and Health"))+
  geom_line(data = subset(model,state=="D"&gender==whichgender & agegroup=="total"), 
            aes(x=year, y= as.numeric(prev), colour=agegroup))+
  scale_y_continuous(name="Prevalence (%)",limits=c(0,0.3),breaks=seq(0,0.3,0.05)) +
  scale_x_continuous(name="Year",limits=c(2005,2020),breaks=seq(2005,2020,1))  +
  labs(title="Current MDE - Women")+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.title = element_blank())
