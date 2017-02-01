library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Human Resource (Exploratory Data Analysis)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Check Outliers"),br(),
      selectInput("TypeOfData",
                  label = "Data Choices:",
                  choices = c("Data normalized", "Data without normalized"),
                  selected = "Data normalized")
    ),
    mainPanel(
      plotOutput("outliers")
    )
  ),br(),
  sidebarLayout(
    sidebarPanel(
      helpText("Show correlations")
    ),
    mainPanel(
      plotOutput("correlation")
    )
  ),br(),
  sidebarLayout(
    sidebarPanel(
      helpText("What you want to find: "),br(),
      selectInput("TypeOfGraph",
                  label = "Type of graph: ",
                  choices = c("Sastification level of people who left", 
                              "Relationship between salary and people who left and evaluation >0.5",
                              "Percentage of people leaving arranged by department"),
                  selected = "Sastification level of people who left"),
      conditionalPanel(
        condition = 'input.TypeOfGraph == "Sastification level of people who left"',
        sliderInput(inputId="evaluation", 
                    label="Evaluation value:", 
                    min=0.00, max=0.95, value=0.50, step=0.05)
      ),
      conditionalPanel(
        condition = 'input.TypeOfGraph == "Percentage of people leaving arranged by department"',
        checkboxInput(inputId = "position", label = "Position", value=FALSE)
      )
    ),  
    mainPanel(
      plotOutput("plot")
    )
  )
)