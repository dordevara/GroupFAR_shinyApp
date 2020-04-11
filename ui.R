library(shiny)
library(shinydashboard)
library(tidyverse)
library(maps)
library(mapproj)
library(ggplot2)
library(DT)
library(Cairo)# For ggplot2
library(MASS)
library(e1071) 
library("party") 
library("rpart") 
library("Metrics") 
library(nnet) 
library(caret)
library("PerformanceAnalytics")

ui <- fluidPage(titlePanel("CA1 - Group: Roxana, Adrian, Florica"), 
                
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file1", "Choose CSV File",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    ),
                    checkboxInput("header", "Header", TRUE),
                    selectInput(inputId = "depvar", label = "Dependent Variables", choices = ""), 
                    selectInput(inputId = "indepvar", label = "Independent Variables", multiple = TRUE, choices = ""),             
                    uiOutput("depvar"),
                    uiOutput("indepvar"),
                    sliderInput("ratio", "Ratio for trainset", min = 1, max = 100, value = 80 ), 
                    sliderInput("mc", "mc", min = 1, max = 1000, value = 1 ),
                    radioButtons("type_radio", label ="", choices = list("Regression", "Classification")), 
                    radioButtons("model_radio", label ="", choices = list("GLM", "SVM", "Decision Tree", "Naive Bayes")) 
                  ),
                  
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                tabPanel("Correlation Matrix", #tabName = "scatterplot", 
                                         h3("Correlation Matrix"),
                                         tryCatch({plotOutput("scatterplot")},
                                                  error = function(e){message("Waiting for input ")
                                                    print(e) }
                                         )
                                         
                                         ),
                                tabPanel("Model Summary", 
                                         box( background = "olive",
                                              collapsible = FALSE, title="Model", valueBoxOutput("rmse")),
                                         
                                         box( collapsible = FALSE, 
                                              title = "Average Accuracy",
                                              status="primary",
                                              DT::dataTableOutput("summaryTable")),
                                         
                                         box( background = "olive", 
                                              collapsible = FALSE,
                                              title = "Model Summary",
                                              #height = "300px", #need to add scroll here to avoid a long box
                                              tryCatch({verbatimTextOutput("modelling")},
                                                       error = function(e){
                                                         message("Waiting for modeling output ")
                                                         print(e)
                                                       })
                                         )
                                         ),
                                tabPanel("Actual vs Predicted", 
                                         tabBox(
                                           title = "Actual vs Predicted",
                                           height = "500px",
                                           selected = "Values",
                                           tabPanel("Values", DT::dataTableOutput('actualVpred')),
                                           tabPanel("Plot", plotOutput('PLOTactualVpred'))
                                         ),
                                         
                                         tabBox(
                                           title = "Train & Test Sets",
                                           height = "500px",
                                           selected = "Test Set",
                                           tabPanel( "Test Set", DT::dataTableOutput('testset')),
                                           tabPanel("Train Set", DT::dataTableOutput('trainset'))
                                         )
                                         )
                    )
                    
                  )
                )
)