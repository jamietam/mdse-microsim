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
library(plyr)

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

jpeg(filename = paste0("eFigure3_MDEprev_", date, ".jpg"),
     width = 9, height = 4, units = "in", res = 1000)
print(eFigure3)
dev.off()
