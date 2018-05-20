#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("tidyverse")
# install.packages("magrittr")
# install.packages("shinycssloaders")
# install.packages("DataExplorer")
require(readr) # to read csv faster
library(DataExplorer) # to create powerful visualization  on the dataframe
library(ggplot2) # to create aesthetic visualizations
library(magrittr) # to make use of R 's pipe operator
library(shinycssloaders) #to create busy indicator when reactivity happens in the shiny app
library(lubridate) # to work on date data type
train <- readr::read_csv("train.csv",col_types=list(
  Store = col_integer(),
  DayOfWeek= col_integer(),
  Date = col_date(),
  Customers = col_integer(),
  Open = col_integer(),
  Promo = col_integer(),
  StateHoliday = col_character(),
  SchoolHoliday = col_integer()))
store <- readr::read_csv("store.csv",col_types=list(
  Store = col_integer(),
  StoreType= col_character(),
  Assortment = col_character(),
  CompetitionDistance = col_integer(),
  CompetitionOpenSinceMonth = col_integer(),
  CompetitionOpenSinceYear = col_integer(),
  Promo2 = col_integer(),
  Promo2SinceYear = col_character(),
  PromoInterval = col_character()
)
)

mergedTrain <- merge(train,store)

mergedTrain$CompetitionDistance <- NULL
mergedTrain$CompetitionOpenSinceMonth <- NULL
mergedTrain$Promo2SinceWeek <- NULL
mergedTrain$CompetitionOpenSinceYear <- NULL
mergedTrain$Promo2SinceYear <- NULL
mergedTrain$PromoInterval <- NULL

paste(sum(is.na(mergedTrain)))
# Define UI for application
ui <- fluidPage(
  titlePanel("RossMann Sales Data Analysis",windowTitle = "rossmannShinyApp_MuthuApril2018"),
  tabsetPanel(
    tabPanel("Distribution of Data",
             sidebarLayout(
               sidebarPanel(
                 hr(),
                 hr(),
                 selectInput("features", 
                             label = "Feature Type ",
                             choices = c("Discrete", "Continuous"),
                             selected = "Discrete"),
                 hr(),
                 hr(),
                 sliderInput("bins", 
                             label = "Number of Stores",min = 1, max = 1115, value = 100)
               ),
               mainPanel(
                 plotOutput('plot') %>% withSpinner(color="#0dc5c1"))
             )
    ),
    tabPanel("Outliers and Correlation",
             sidebarLayout(
               sidebarPanel( 
                 hr(),
                 hr(),
                 selectInput("features1", 
                             label = "Box Plot & ScatterPlot",
                             choices = c("Box Plot", "ScatterPlot"),
                             selected = 1),
                 hr(),
                 hr(),
                 sliderInput("bins1", 
                             label = "Number of Stores",min = 1, max = 1115, value = 100)
               ),
               mainPanel(
                 plotOutput('plot1') %>% withSpinner(color="#0dc5c1"))
             )),
    tabPanel("Sales Information",
             sidebarLayout(
               sidebarPanel(
                 hr(),
                 hr(),
                 numericInput("selectStore",label = "Pick a store between 1 and 30",value=2,min= 1,max=30),
                 hr(),
                 hr(),
                 selectInput("typePlt", 
                             label = "Type of TimeSeries Plot",
                             choices = c("General Sales Trend", "Per Customer Sales Trend"),
                             selected = "General Sales Trend")
               ),
               mainPanel(
                 plotOutput('plot2') %>% withSpinner(color="#0dc5c1"))
             )
    )
  )
)
# Define server
plot_function <- function(type,bins)
{
  #ifelse(type == "discreteFeatures",plot_bar(mergedTrain),plot_histogram(mergedTrain))
  #dat <-  dplyr::slice(visualData,1:(bins*1115))
  dat <- mergedTrain[1:(1115*bins),]
  if(type == 1){
    plot_bar(dat,title = "Frequency distribution of Discrete valued features")
  }
  else{
    #data <- visualData[,4]
    plot_histogram(dat,title = "Frequency distribution of Continuous valued features")
  }
}

plot_function1 <- function(type,bins1)
{
  #ifelse(type == "discreteFeatures",plot_bar(mergedTrain),plot_histogram(mergedTrain))
  #dat <-  dplyr::slice(visualData,1:(bins*1115))
  dat <- mergedTrain[1:(1115*bins1),]
  if(type == 1){
    plot_boxplot(dat,"Sales",title = "Outlier Detection using Box & Whiskers Plot")
  }
  else{
    #data <- visualData[,4]
    plot_correlation(dat,title = "Correlation between variables")
  }
}
plot_function2 <- function(store,type)
{
  salesData <- mergedTrain[1:(1115*30),]
  dat <- dplyr::filter(salesData,Store == store) 
  dat$perCustomer <- (dat$Sales / dat$Customers) 
  #dat <- dplyr::arrange(dat,Date)
  if(type==0){
    ggplot(dat,title="Overall Sales of selected Store", aes(x=dat$Date,y=dat$Sales))+geom_line()+xlab("Dates")+ylab("Sales")
  }
  else{
    ggplot(dat,title="per Customer sales of selected Store", aes(x=dat$Date,y=dat$perCustomer),colour="green")+geom_line()+xlab("Dates")+ylab("PerCustomer Sales")
  }
}

server <- function(input,output,session){
  output$plot <- renderPlot({
    selectedType <- switch(input$features,
                           "Discrete"= 1,
                           "Continuous"= 2)
    plot_function(type <- selectedType, bins <- input$bins)       
  })
  output$plot1 <- renderPlot({
    selectedType <- switch(input$features1,
                           "Box Plot"= 1,
                           "ScatterPlot"= 2)
    plot_function1(type <- selectedType, bins <- input$bins)       
  })
  output$plot2 <- renderPlot({
    selectedType <- switch(input$typePlt,
                           "General Sales Trend"= 0,
                           "Per Customer Sales Trend"= 1)
    plot_function2(store <- input$selectStore, type <-selectedType )       
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

