#Load data
data <- read.csv(file = "HR_comma_sep.csv")

#Look at the internal structure of the data
str(data)
head(data)

#Correct the column names
colnames(data)[names(data) == "sales"] <- "department"
colnames(data)[names(data) == "time_spend_company"] <- "years_spent_in_company"
colnames(data)[names(data) == "left"] <- "left_job"
colnames(data)[names(data) == "average_montly_hours"] <- "average_monthly_hours"

#Remove duplicates
sum(duplicated(data)) #Number of duplicates
data <- data[!duplicated(data),] #Remove

#Check for NA
any(is.na(data))