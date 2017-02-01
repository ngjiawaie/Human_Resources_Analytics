library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)
library(corrplot)
library(DT)
library(caret)
library(radarchart)

data <- read.csv(file = "HR_comma_sep.csv")

colnames(data)[names(data) == "sales"] <- "department"
colnames(data)[names(data) == "time_spend_company"] <- "years_spent_in_company"
colnames(data)[names(data) == "left"] <- "left_job"
colnames(data)[names(data) == "average_montly_hours"] <- "average_monthly_hours"
data <- data[!duplicated(data),]
preprocessParams <- preProcess(data[,1:8], method=c("range"))
normData <- predict(preprocessParams, data[,1:8])
default <- par("mar")

par(mar=c(10,11,3,1))
#------------------------------------------------------------------End of Data Preparation


shinyServer(function(input, output) {
  
  output$outliers<- renderPlot({
    
    if(input$TypeOfData == "Data normalized"){
      boxplot(data[1:5], names = names(data[1:5]), las = 2, col = c("#7E57C2","#42A5F5","#9CCC65","#EA80FC","#EC407A"), horizontal = T, main="Checking for outliers (Data without normalizing)")
    }
    #Plot using facet, foods that is bought in different quantity is visualized in different charts
    else if(input$TypeOfData == "Data without normalized"){
      boxplot(normData[1:5], names = names(normData[1:5]), las = 2, col = c("#7E57C2","#42A5F5","#9CCC65","#EA80FC","#EC407A"), horizontal = T, main="Checking for outliers (Data normalized)")
    }
    
  })
  
  output$correlation <- renderPlot({
    corrplot(cor(data[,1:8]), method="circle")
  })
  
  output$plot <- renderPlot({
    
    if(input$TypeOfGraph == "Sastification level of people who left"){
      hr_hist <- data %>% filter(left_job==1, last_evaluation > input$evaluation)
      hist(hr_hist$satisfaction_level,col="#3090C7", main = "Satisfaction level : left job + evaluation")
    }
    else if(input$TypeOfGraph == "Relationship between salary and people who left and evaluation >0.5"){
      hr_bar <- data %>% filter(last_evaluation > 0.5, left_job==1)
      barplot(table(hr_bar$salary), col="#3090C7", main = "Salary : left job + >0.5 performance")
    } 
    else if(input$TypeOfGraph == "Percentage of people leaving arranged by department"){
      if(input$position == FALSE){
        ggplot(data,aes(department,fill=as.factor(left_job)))+geom_bar()
      }
      else{
        ggplot(data,aes(department,fill=as.factor(left_job)))+geom_bar(position="fill")
      }
    } 
  })
  
  
})
