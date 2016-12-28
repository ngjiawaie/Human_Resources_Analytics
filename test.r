#COPIED
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)
library(corrplot)
library(DT)
dat <- read.csv("HR_comma_sep.csv")
str(dat)
summary(dat)
HR_correlation <- dat %>% select(satisfaction_level:promotion_last_5years)
M <- cor(HR_correlation)
corrplot(M, method="circle")
cor(HR_correlation)

hr_hist <- dat %>% filter(left==1)
par(mfrow=c(1,3))
hist(hr_hist$satisfaction_level,col="#3090C7", main = "Satisfaction level") 
hist(hr_hist$last_evaluation,col="#3090C7", main = "Last evaluation")
hist(hr_hist$average_montly_hours,col="#3090C7", main = "Average montly hours")
#COPIED

#Basic Idea:
#1. Create the 'big' picture first, that is: list out which att has strong correlation with the other. In this case, left<->satisfaction level
#2. Narrow down our focus on those who left. Analyze each relevant attribute that we might label as important.
#3. Identify which person to keep (those with high evaluation score, stays in the company for long, worked on many projects, and left)
#4. Narrow down our scope to the minor few(those we want to retain), replot correlation graph

#END OF PRELIMINARY ANALYSIS