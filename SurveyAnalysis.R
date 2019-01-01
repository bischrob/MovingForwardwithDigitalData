################################################################################
#' Code for survey analysis in the paper 'Moving Forward with Digital Data'
#' Author: Robert J. Bischoff

library(tidyverse)
setwd("~/GitHub/MovingForwardwithDigitalData")

# import data
surveydf <- read_csv('Digital Data Questionnaire.csv')

# clean data
metadata <- surveydf[1,]
surveydf <- surveydf[-1,]

# Q1
# margin of error
z = 1.645 # 90% confidence score
n = sum(surveydf$Q1 == "Yes" | surveydf$Q1 == "No", na.rm = T) # sample size
p = sum(surveydf$Q1 == "No", na.rm = T) / n # proportion
s = sqrt(p * (1-p)) 
e = z * s / sqrt(n) # margin of error


