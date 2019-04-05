################################################################################
#' Code for survey analysis in the paper 'Moving Forward with Digital Data'
#' Author: Robert J. Bischoff

library(tidyverse)
setwd("~/GitHub/MovingForwardwithDigitalData")

# import data
surveydf <- read_csv('Digital Data Questionnaire.csv')

# clean data
metadata <- surveydf[1,] # move full questions to new variable
surveydf <- surveydf[-1,] # remove above from data
unique(surveydf$Education) 
surveydf$Education <- gsub('ABD',"Master's",surveydf$Education) # Change "ABD" to closest applicable response
unique(surveydf$Experience)
surveydf$Experience <- gsub("[^0-9\\.]", "", surveydf$Experience) # remove non-numeric elements
surveydf <- surveydf %>% mutate_at(vars(Experience),as.numeric)
unique(surveydf$Sector)
surveydf$Sector <- gsub('THPO','Government', surveydf$Sector)
surveydf$Sector <- gsub('Tribal','Government', surveydf$Sector)
surveydf$Sector <- gsub('museum director','Academic/Research Institution', surveydf$Sector)
surveydf$Sector <- gsub('all of the above but mostly CRM','Private/CRM', surveydf$Sector)
surveydf$Sector <- gsub('Academic/CRM', 'Other', surveydf$Sector)
surveydf$Sector <- gsub('retired', 'Other', surveydf$Sector)
surveydf$Sector <- gsub('Non-profit', 'Other', surveydf$Sector)
unique(surveydf$Q10)
surveydf <- surveydf %>% mutate_at(vars(Education,Sector:Q5,Q10),as.factor)

################################################################################
# Demographic info
# Education
summary(surveydf$Education)
summary(surveydf$Experience)
ggplot(surveydf, aes(Experience)) + 
  geom_histogram(binwidth = 10, color = "black", fill = '#787878') +
  theme_bw() + ylab('') + 
  ggtitle('Years of experience in archaeology')
summary(surveydf$Sector)
surveydf$Sector <- factor(surveydf$Sector,levels(surveydf$Sector)[c(1:2,4,3)])
ggplot(surveydf, aes(Sector)) + 
  geom_bar(color = "black", fill = '#787878') +
  theme_bw() + ylab('') + xlab('') +
  ggtitle('Employment sector') 
ggsave('plots/Employment Sector.svg')

# error margin formula
moe <- function(x, r){
  # margin of error
  z = 1.645 # 90% confidence score
  n = length(!is.na(x)) # sample size
  p = sum(x == r, na.rm = T) / n # proportion
  s = sqrt(p * (1-p)) 
  e = z * s / sqrt(n) # margin of error  
  return(list(moe = e, p = p, plus = p + e, minus = p - e))
}

# title wrap text function
wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}

# Q1
table(surveydf$Q1)
Qmoe1 <- moe(surveydf$Q1,"Yes")
Qmoe2 <- moe(surveydf$Q1,"No")
plotdf <- tibble(Q = factor(levels(surveydf$Q1), levels = c("Yes","No")), 
                 per = c(Qmoe1$p, Qmoe2$p),
                 min = c(Qmoe1$minus, Qmoe2$minus),
                 max = c(Qmoe1$plus, Qmoe2$plus))
ggplot(plotdf, aes(x = Q, y = per)) +
  geom_bar(stat = "identity",
           color = "black", fill = '#787878') + 
  scale_y_continuous(labels=scales::percent) +
  geom_errorbar(aes(ymin = min,
                    ymax = max), width=.2,
                position=position_dodge(.9)) +
  theme_bw() + ylab('') + xlab('') +
  ggtitle(wrapper(metadata$Q1,70)) 
ggsave('plots/Q1.svg')

# Q2
table(surveydf$Q2)
Qmoe1 <- moe(surveydf$Q2,"Yes")
Qmoe2 <- moe(surveydf$Q2,"No")
plotdf <- tibble(Q = factor(levels(surveydf$Q2), levels = c("Yes","No")), 
                 per = c(Qmoe1$p, Qmoe2$p),
                 min = c(Qmoe1$minus, Qmoe2$minus),
                 max = c(Qmoe1$plus, Qmoe2$plus))
ggplot(plotdf, aes(x = Q, y = per)) +
  geom_bar(stat = "identity",
           color = "black", fill = '#787878') + 
  scale_y_continuous(labels=scales::percent) +
  geom_errorbar(aes(ymin = min,
                    ymax = max), width=.2,
                position=position_dodge(.9)) +
  theme_bw() + ylab('') + xlab('') +
  ggtitle(wrapper(metadata$Q2,70)) 
ggsave('plots/Q2.svg')

# Q3
table(surveydf$Q3)
Qmoe1 <- moe(surveydf$Q3,levels(surveydf$Q3)[1])
Qmoe2 <- moe(surveydf$Q3,levels(surveydf$Q3)[2])
Qmoe3 <- moe(surveydf$Q3,levels(surveydf$Q3)[3])
plotdf <- tibble(Q = factor(levels(surveydf$Q3), levels(surveydf$Q3)), 
                 per = c(Qmoe1$p, Qmoe2$p, Qmoe3$p),
                 min = c(Qmoe1$minus, Qmoe2$minus, Qmoe3$minus),
                 max = c(Qmoe1$plus, Qmoe2$plus, Qmoe3$plus))
ggplot(plotdf, aes(x = Q, y = per)) +
  geom_bar(stat = "identity",
           color = "black", fill = '#787878') + 
  scale_y_continuous(labels=scales::percent) +
  geom_errorbar(aes(ymin = min,
                    ymax = max), width=.2,
                position=position_dodge(.9)) +
  theme_bw() + ylab('') + xlab('') +
  ggtitle(wrapper(metadata$Q3,70)) 
ggsave('plots/Q3.svg')

# Q11
sum(grepl("yes",surveydf$Q11, ignore.case = T)) 
sum(grepl("no",surveydf$Q11, ignore.case = T)) + 1 # one of the other responses was a negative

reorder(surveydf$Sector,c("Academic/Research Institution",
                 "Government",
                 "Private/CRM",
                 "Other"))
