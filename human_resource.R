#Dependencies
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggvis")
install.packages("corrplot")
install.packages("DT")
install.packages("caret")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)
library(corrplot)
library(DT)
library(caret)

#Load data
data <- read.csv(file = "HR_comma_sep.csv")

#Look at the internal structure and summary of the data
str(data)
summary(data)
head(data)

#Correct the column names
colnames(data)[names(data) == "sales"] <- "department"
colnames(data)[names(data) == "time_spend_company"] <- "years_spent_in_company"
colnames(data)[names(data) == "left"] <- "left_job"
colnames(data)[names(data) == "average_montly_hours"] <- "average_monthly_hours"


###Data Cleaning
#Check and remove duplicates
sum(duplicated(data)) #Number of duplicates
data <- data[!duplicated(data),] #Remove

#Check for NA
any(is.na(data)) #Since no NAs, no imputation required. Moving on...

#Check for outliers
boxplot(data$satisfaction_level)
boxplot(data$last_evaluation)
boxplot(data$number_project)
boxplot(data$average_monthly_hours)
boxplot(data$years_spent_in_company) #This shows some outliers

#Further checking years_spent_in_company
boxplot.stats(data$years_spent_in_company)
levels(factor(data$years_spent_in_company)) #Looks okay, moving on


###Data transformation
#Box cox transform
#preprocessParams <- preProcess(data[,1:8], method=c("BoxCox"))
#data <- predict(preprocessParams, data[,1:8])
#summary(data)

#find the satisfaction level of ppl who left the job
hr_hist <- data %>% filter(left_job==1)
hist(hr_hist$satisfaction_level,col="#3090C7", main = "Satisfaction level : left job")

#find the satisfaction level of ppl who left the job and have >0.5 evaluation(we need high performance worker, as we want high performance worker to stay)
hr_hist <- hr_hist %>% filter(last_evaluation > 0.5, left_job==1)
hist(hr_hist$satisfaction_level,col="#3090C7", main = "Satisfaction level : left job + >0.5 performance")

#find the relationship between salary and ppl who left the job and >0.5 evaluation
hr_bar <- data %>% filter(last_evaluation > 0.5, left_job==1)
barplot(table(hr_bar$salary), col="#3090C7", main = "Salary : left job + >0.5 performance")

#which department has most high performance worker left their job
hr_bar <- data %>% filter(last_evaluation > 0.5, left_job==1)
barplot(table(hr_bar$department), col="#3090C7", main = "Department : left job + >0.5 performance")

# we know that sales has the most number of worker left, hence we need to explore the what is the main cause of sales being the highest worker left
hr_temp <- data %>% filter(last_evaluation > 0.5, left_job==1, department=="sales")
hr_cor <- hr_temp %>% select(satisfaction_level:Work_accident, promotion_last_5years)
plot <- cor(hr_cor)
corrplot(plot, method="circle")


