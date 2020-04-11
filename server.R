library(shiny)
server <- function(input, output, session) {
  
  myDependentVar <- reactive({ 
    mytype = input$type_radio 
    df <- myData() 
    dataset=cbind(df[,input$depvar],df[,input$indepvar])
    ylevels = levels(dataset[,1]) 
    switch(mytype, 
           "Classification"={y <- as.factor(input$y, levels = ylevels)
           return(y)},                             #---------- added return(y)
           "Regression"={y <- as.numeric(input$y) #---------- as.numeric(input$y, c(input$y))
           return(y)} 
           )
  })
  
  myInputData <- reactive({ 
    y <- myDependentVar()
    #make dataframe 
    my_data <- data.frame(y) 
    vars <- myVars() 
    df = myData() 
    indepvars = (df[,input$indepvar])
    for(i in 1:length(vars)) { 
      if(class(indepvars[,i]) == "numeric"){ 
        my_data[1,i+1] <- as.numeric(input[[paste("var",i, 
                                                  sep = "")]]) 
      }  
      else 
      { 
        ylevels = levels(indepvars[,i]) 
        my_data[1,i+1] <- factor( input[[paste("var",i, sep = "")]], 
                                  levels = ylevels) 
      } 
    }  
    #change appropriate column names 
    colnames(my_data) = c('y', vars ) 
    return(my_data)
  } 
  )

  myTestset <- data.frame() 
  makeReactiveBinding("myTestset") 
  myTrainset <- data.frame() 
  makeReactiveBinding("myTrainset") 
  myactualVpred <- data.frame() 
  makeReactiveBinding("myactualVpred") 
  myRMSE <- 0 
  makeReactiveBinding("myRMSE") 
  myPrediction <- 0 
  makeReactiveBinding("myPrediction")
  
  output$modelling <- renderPrint({ 
    df <- myData() 
    dataset <- data.frame(cbind(df[,input$depvar], df[,input$indepvar]))
    dataset <- na.omit(dataset) 
    #splitting into training and test sets 
    n=nrow(dataset) 
    indexes = sample(n,n*((input$ratio)/100)) 
    trainset = dataset[indexes,] 
    testset = dataset[-indexes,] 
    myTrainset <<- trainset 
    myTestset <<- testset 
    actual <<- testset[,1] # changed from testset$X1
    actual 
    mytype = input$type_radio 
    mymodel = input$model_radio 
    switch(mytype, 
           "Regression" = {
             switch(mymodel,  
                    
                    "GLM"= { 
                      print("GLM - Regresion")
                      if(length(trainset[1,])==0){
                        print("Wait")
                      }else{
                        lr_model <<- glm(trainset[,1] ~ ., data=trainset, family = "gaussian") 
                        lr_pred <<- predict(lr_model, testset) 
                        myPrediction <<- lr_pred
                        myRMSE <<- rmse(as.numeric(lr_pred), as.numeric(actual))
                        myactualVpred <<- data.frame(actual, myPrediction) 
                        print(summary(lr_model)) 
                        plot(myactualVpred)
                      }
                    },
                    "SVM"= { 
                      print("SVM - Regresion")
                      if(length(trainset[1,])==0){
                        print("Wait")
                      }else{
                        svr_model <- svm(trainset[,1] ~ ., data=trainset) 
                        svr_pred <- predict(svr_model, testset) 
                        myRMSE <<- rmse(as.numeric(svr_pred), as.numeric(actual)) 
                        myPrediction <<- svr_pred 
                        myactualVpred <<- data.frame(actual, svr_pred) 
                        print(svr_model) 
                        plot(myactualVpred)
                      }                     
                    }, 
                    "Decision Tree"= { 
                      print("Decision Tree - Regresion")
                      if(length(trainset[1,])==0){
                        print("Wait")
                      }else{
                        library(rpart)
                        rpart_model <- rpart(trainset[,1] ~., data=trainset) 
                        rpart_pred <- predict(rpart_model, testset) 
                        myRMSE <<- rmse(as.numeric(rpart_pred), as.numeric(actual)) 
                        myPrediction <<- rpart_pred 
                        myactualVpred <<- data.frame(actual, rpart_pred) 
                        print(rpart_model)                        
                        plot(myactualVpred)
                      }                     
                    }, 
                    "Naive Bayes"= { 
                      print("Naive Bayes - Regresion")
                      if(length(trainset[1,])==0){
                        print("Wait")
                      }else{
                        nb_model <- naiveBayes(trainset[,1] ~ .,  data = trainset) 
                        nb_pred <- predict(nb_model, testset)  
                        myRMSE <- rmse(as.numeric(nb_pred), as.numeric(actual)) 
                        myPrediction <- nb_pred 
                        myactualVpred <- data.frame(actual, nb_pred) 
                        print(nb_model) 
                        plot(myactualVpred)
                      } 
                    })
           }
           , 
           "Classification" = {
             switch(mymodel,  
                    "GLM"= { 
                      print("Classification GLM")
                      if(length(trainset[1,])==0){
                        print("Wait")
                      }else{
                        library(MASS)
                        lr_model <- multinom(as.factor(trainset[,1]) ~ .,  data=trainset) 
                        lr_pred <- predict(lr_model, testset) 
                        myRMSE <<- mean(lr_pred == actual) 
                        myPrediction <<- lr_pred
                        myactualVpred <<- data.frame(actual, lr_pred) 
                        print(summary(lr_model)) 
                        
                        #confusion_matrix <- confusionMatrix(lr_pred, testset) 
                        #print(confusion_matrix)
                      }
                    },
                    "SVM"= { 
                      print("Classification - SVM")
                      if(length(trainset[1,])==0){
                        print("Wait")
                      }else{
                        library(e1071)
                        svr_model <- svm(as.factor(trainset[,1]) ~ ., data=trainset, method="C-classification") 
                        svr_pred <- predict(svr_model, testset) 
                        myRMSE <<- mean(svr_pred == actual) 
                        # peste tot am inlocuit predict(svr_model, myTrainset) ca sa faca prediction la testset si nu la trainset
                        myPrediction <<- svr_pred
                        myactualVpred <<- data.frame(actual, svr_pred) 
                        print(svr_model) 
                        #confusion_matrix <- confusionMatrix(svr_pred, actual) 
                        #print(confusion_matrix)
                      }
                    }, 
                    "Decision Tree"= { 
                      print("Classification Decision Tree")
                      if(length(trainset[1,])==0){
                        print("Wait")
                      }else{
                        library(rpart)
                        rpart_model <- rpart(as.factor(trainset[,1])~., data=trainset, method="class", control = rpart.control(cp = 0)) 
                        rpart_pred <- predict(rpart_model, testset, type='class') 
                        myRMSE <<- mean(rpart_pred == actual) 
                        myPrediction <<- rpart_pred 
                        myactualVpred <<- data.frame(actual, rpart_pred) 
                        print(rpart_model) 
                        #confusion_matrix <- confusionMatrix(rpart_pred, actual) 
                        #print(confusion_matrix)                           
                      }
                    }, 
                    "Naive Bayes"= { 
                      print("Classification Naive Bayes")
                      if(length(trainset[1,])==0){
                        print("Wait")
                      }else{
                        nb_model <- naiveBayes(as.factor(trainset[,1]) ~ ., data = trainset) 
                        nb_pred = predict(nb_model, testset)  
                        myRMSE <<- mean(nb_pred == actual) 
                        myPrediction <<- nb_pred 
                        myactualVpred <<-data.frame(actual, nb_pred) 
                        print(nb_model) 
                        #confusion_matrix <- confusionMatrix(nb_pred, actual) 
                        #print(confusion_matrix)
                      }
                    })
           })
  }) #----------------------------end of output modeling

  #-------------------------------------------------------
  output$summaryTable <- DT::renderDataTable({ 
    tryCatch({
      #---- add the details here
      df <- myData() 
      dataset <- data.frame(cbind(df[,input$depvar],df[,input$indepvar]))
      dataset <- na.omit(dataset) 
      mytype = input$type_radio 
      mc = input$mc 
      vectorNB = rep(0,mc) 
      vectorLM = rep(0,mc) 
      vectorRPART = rep(0,mc) 
      vectorSVM = rep(0,mc) 
      vectorPRINT = data.frame()
      
      switch(mytype, 
             "Regression" = { 
               if( ncol(dataset)>1){
                 for (i in 1:mc) 
                 { 
                   #----------splitting into training and test sets 
                   n=nrow(dataset) 
                   indexes = sample(n,n*(input$ratio/100)) 
                   trainset = dataset[indexes,] 
                   testset = dataset[-indexes,] 
                   actual <- testset[,1] 
                   #-----------LINEAR REGRESSION
                   lr_model <- glm(trainset[,1] ~., data=trainset, family = "gaussian") 
                   lr_pred <- predict(lr_model, testset) 
                   vectorLM[i] = rmse(as.numeric(lr_pred), as.numeric(actual))
                   vectorLM <- c(vectorLM, vectorLM[i])
                   #-----------SVM
                   svr_model <- svm(trainset[,1] ~., data=trainset) 
                   svr_pred <- predict(svr_model, testset) 
                   vectorSVM[i] = rmse(as.numeric(svr_pred), as.numeric(actual)) 
                   vectorSVM <- c(vectorSVM, vectorSVM[i])
                   #-----------DECISION TREE using rpart function
                   rpart_model <- rpart(trainset[,1] ~., data=trainset) 
                   rpart_pred <- predict(rpart_model, testset) 
                   vectorRPART[i] = rmse(as.numeric(rpart_pred), as.numeric(actual)) 
                   vectorRPART <- c(vectorRPART, vectorRPART[i])
                   #-----------Naive Bayes
                   nb_model <- naiveBayes(as.factor(trainset[,1]) ~ ., data = trainset) 
                   nb_pred = predict(nb_model, testset)  
                   vectorNB[i] = rmse(as.numeric(nb_pred), as.numeric(actual)) 
                   vectorNB <- c(vectorNB, vectorNB[i])
                 } 
                }
                 vectorPRINT = data.frame(mean(vectorLM), mean(vectorRPART), mean(vectorSVM), mean(vectorNB)) 
                 colnames(vectorPRINT) = c("REG_GLM","RPART", "SVM", "NB") 
                 vectorPRINT 
               
             }, 
             "Classification" = { 
               if( ncol(dataset)>1){
                 for (i in 1:mc) 
                 { 
                   #splitting into training and test sets 
                   n=nrow(dataset) 
                   indexes = sample(n,n*(input$ratio/100)) 
                   trainset = dataset[indexes,] 
                   testset = dataset[-indexes,] 
                   actual <- testset[,1] 
                   #-----------LINEAR REGRESSION
                   library(MASS)
                   lr_model<- multinom(as.factor(trainset[,1]) ~ ., data=trainset, MaxNWts = 10000) 
                   lr_pred = predict(lr_model,testset) 
                   vectorLM[i] <- mean(lr_pred == actual)
                   vectorLM <- c(vectorLM, vectorLM[i])
                   #------------SVM
                   svr_model <- svm(as.factor(trainset[,1]) ~ ., data=trainset, method="C-classification") 
                   svr_pred = predict(svr_model, testset) 
                   vectorSVM[i] <- mean(svr_pred == actual) 
                   vectorSVM <- c(vectorSVM, vectorSVM[i])
                   #-------------DECISION TREE using rpart function
                   rpart_model <- rpart(trainset[,1]~., data=trainset, method="class") 
                   rpart_pred <- predict(rpart_model, testset, type='class') 
                   vectorRPART[i] <- mean(rpart_pred == actual) 
                   vectorRPART <- c(vectorRPART, vectorRPART[i])
                   #-------------Naive Bayes
                   nb_model <- naiveBayes(as.factor(trainset[,1]) ~ ., data = trainset) 
                   nb_pred = predict(nb_model, testset[,-1], type='class')  
                   vectorNB[i] <- mean(nb_pred == actual) 
                   vectorNB <- c(vectorNB, vectorNB[i]) 
                 } 
               }
               vectorPRINT = data.frame(mean(vectorLM), mean(vectorRPART), mean(vectorSVM), mean(vectorNB)) 
               colnames(vectorPRINT) = c("REGGLM","RPART", "SVM", "NB") 
               vectorPRINT 
             })
      
    },
    error = function(e){
      message("Waiting for input File!!! ")
      print(e)
    }
    ) 
  }) 
  
  #-----------------------------
  # Data output 
  output$datatable = DT::renderDataTable({ 
    #---- get input  
    # input$file1 will be NULL initially. After the user selects and uploads a file, head of that data file by default, or all rows if selected, will be shown. 
    req(input$datafile) 
    # when reading semicolon separated files, having a comma separator causes `read.csv` to error 
    tryCatch( 
      { 
        df <- read.csv(input$datafile$datapath) 
      }, 
      error = function(e) { 
        # return a safeError if a parsing error occurs 
        stop(safeError(e)) 
      } 
    ) 
    return(df) 
  }) 
  
  output$testset = DT::renderDataTable({ 
    tryCatch({
      #---- get myTestset 
      myTestset 
    },
    error = function(e){
      message("Waiting for input File!!! ")
      print(e)
    })
    
  }) 
  
  output$trainset = DT::renderDataTable({ 
    tryCatch({
      #---- get  myTrainset 
      myTrainset 
    },
    error = function(e){message("Waiting for input File!!! ")
              print(e) 
              }
    )
  }) 
  
  output$inputData = DT::renderDataTable({ 
    tryCatch({
      #---- get data
      dataset = data.frame(myInputData()) 
      dataset 
    },
    error = function(e){
      message("Waiting for input File!!! ")
      print(e)
    }
    )
  }) 
  
  #-------------------------table display actual vs predicted values
  output$actualVpred = DT::renderDataTable({ 
    colnames(myactualVpred) = c("actual", "prediction") 
    myactualVpred 
  }) 
  
  #-------------------------plot actual vs predicted values
  output$PLOTactualVpred <- renderPlot({ 
    input$newplot
    colnames(myactualVpred) = c("actual", "prediction") 
    plot(myactualVpred)
  }) 
  
  #-------------------------define prediction function     
  output$prediction <- renderValueBox({ 
    valueBox( myPrediction, "Prediction", icon = icon("lightbulb"), color = "yellow" ) 
  }) 
  
  #-------------------------define ACC/RMSE function   
  output$rmse <- renderValueBox({ 
    valueBox( 
      myRMSE, "Accuracy / RMSE", icon = icon("check"), color = "purple" ) 
  })  
  
  #-------------------------correlation matrix
    output$scatterplot <- renderPlot({ 
    tryCatch({
      df <- myData() 
      data <- cbind(df[,input$depvar],df[,input$indepvar]) # here we need to make it work for more than one indep var
      vars = myVars() 
      colnames(data) = c(input$depvar, vars) 
      library(psych)
      pairs.panels(data)
    },
    error = function(e){
      message("Waiting for input ")
      print(e)
    }
    )
  }, height = 600)     
  
  #--------------------------Read the data from the files 
  myData <- reactive({ 
    tryCatch({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath, header = input$header)
    },
    error = function(e){
      message("Waiting for input File!!! ")
      print(e)
    }
    )
  })
  
  myVars <- reactive({ 
    tryCatch({
      #---- add independent variable 
      vars_df <- input$indepvar 
      vars_df 
    },
    error = function(e){
      message("Waiting for independent variablle!!! ")
      print(e)
    }
    )
  }) 
  observe({ 
    tryCatch({
      #---- chose the columns to work with
      updateSelectInput(session, "depvar",    choices = colnames(myData())) 
      updateSelectInput(session, "indepvar",  choices = colnames(myData())) 
    },
    
    error = function(e){
      message("Waiting column selection!!! ")
      print(e)
    }
    )
  }) 
  #-------------------
  output$depvar <- renderUI({ 
    #---- chose the columns to work with
    df <- myData() 
    dataset <- cbind(df[,input$depvar],df[,input$indepvar])
    ylevels = levels(dataset[,1]) 
    w = "" 
    mytype = input$type_radio 
    switch(mytype, 
           "Classification"={w = paste(w, selectInput(paste("y"), 
                                                      paste(input$depvar), 
                                                      choices = ylevels))}, 
           "Regression"={w = paste(w, textInput(paste("y"),
                                                paste(input$depvar),
                                                1))}) 
    HTML(w) 
    
  }) 
  
  output$vars <- renderUI({ 
    tryCatch({
      #--------------- output independent variables
      w = "" 
      vars <- myVars() 
      df = myData() 
      indepvars = (df[,input$indepvar]) 
      for(i in 1:length(vars)) { 
        if(class(indepvars[,i]) == "numeric"){ 
          w = paste(w, textInput(paste("var", i, sep = ""), 
                                 paste(vars[i], sep = ""),
                                 "1")) 
        } 
        else 
        { 
          ylevels = levels(indepvars[,i]) 
          w = paste(w, selectInput(paste("var", i, sep = ""), 
                                   paste(vars[i], sep = ""), 
                                   choices = ylevels)) 
        } 
      } 
      HTML(w) 
      
    },
    error = function(e){
      message("Waiting for ouput variable!!! ")
      print(e)
    }
    )
  }) 

}
