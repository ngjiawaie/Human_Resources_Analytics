#Dependencies
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggvis")
install.packages("corrplot")
install.packages("DT")
install.packages("caret")
install.packages("radarchart")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)
library(corrplot)
library(DT)
library(caret)
library(radarchart)

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
#Normalize the data
preprocessParams <- preProcess(data[,1:8], method=c("range"))
normData <- predict(preprocessParams, data[,1:8])
#Without normalize, the boxplot looks like this
boxplot(data[1:5], names = names(data[1:5]), las = 2, col = c("#7E57C2","#42A5F5","#9CCC65","#EA80FC","#EC407A"))
#With normalize
boxplot(normData[1:5], names = names(normData[1:5]), las = 2, col = c("#7E57C2","#42A5F5","#9CCC65","#EA80FC","#EC407A"))
#years_spent_in_company has some outliers shown in the boxplot, examining it...
(outliers <- levels(factor(boxplot.stats(data[,'years_spent_in_company'])$out)))
(oriValues <- levels(factor(data[,'years_spent_in_company'])))  #The outliers are legit data, moving on...

#First, we want to find the correlation of attributes in the dataset
corrplot(cor(data[,1:8]), method="circle")

#find the satisfaction level of ppl who left the job
hr_hist <- data %>% filter(left_job==1)
hist(hr_hist$satisfaction_level,col="#3090C7", main = "Satisfaction level : left job")
#as depicted, those who left have low satisfaction level, but we wouldn't wan't to keep everybody.

#find the satisfaction level of ppl who left the job and have >0.5 evaluation(we need high performance worker, as we want high performance worker to stay)
hr_hist <- hr_hist %>% filter(last_evaluation > 0.5, left_job==1)
hist(hr_hist$satisfaction_level,col="#3090C7", main = "Satisfaction level : left job + >0.5 performance")
#Oddly enough, there is quite a number of competent workers(evaluation > 0.5) with moderate to high satisfaction toward the company who left still.

#find the relationship between salary and ppl who left the job and >0.5 evaluation
hr_bar <- data %>% filter(last_evaluation > 0.5, left_job==1)
barplot(table(hr_bar$salary), col="#3090C7", main = "Salary : left job + >0.5 performance")
#there are alot of people from low/medium salary group who left the job

#which department has most high performing worker leaving their job(in number and percentages)
hr_left <- data %>% filter(left_job==1, last_evaluation > 0.5)

left_stats <- hr_left %>% 
  group_by(department) %>% 
  summarize(left_counts = sum(left_job)) %>% 
  mutate(perc_left = left_counts/sum(left_counts) *100)

chartJSRadar(left_stats, showToolTipLabel = T)
#sales department shows the highest number of people leaving thier job, is it just because sales department has too many workers? Or sales department has a higher percentage of employees quiting their job

#we want to check all the department whether there exsist a department with a high percentage of employees quiting their job
#aes fill is to get a clear picture of the ratio of workers who left their jobs and the total employees in certan department 
ggplot(data,aes(department,fill=as.factor(left_job)))+geom_bar()
#add geom_bar position fill make it easier to compare proportions
ggplot(data,aes(department,fill=as.factor(left_job)))+geom_bar(position="fill")

#summary: Although sales department shows a high number of employees quitting their jobs, but we found out that this is mainly due to the high number of employees in sales department. The ratio of total employees of the department to the employees of the department who left their jobs is quite balance among all the department.