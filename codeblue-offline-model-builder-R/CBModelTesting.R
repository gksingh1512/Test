InitEnv <- function() {
  # To init the environment
  #
  #
  #
  # Returns: Nothing
  # Error handling
  
  # closeAllConnections()
  # rm(list = ls())
  # install.packages("futile.logger")
  # install.packages("randomForest")
  # install.packages("pROC")
  # install.packages("xda")
  # install.packages("mice")
  # install.packages("VIM")
  # install.packages("xts")
  # install.packages("broom")
  # install.packages("ROSE")
  # install.packages("DMwR")
  # install.packages("Boruta")
  # 
  library(futile.logger)
  library(randomForest)
  library(pROC)
  library(caret)
  library(dplyr)
  library(car)
  library(scales)
  library(ggplot2)
  library(hexbin)
  library(fpc)
  library(pastecs)
  library(factoextra)
  library(cluster)
  library(NbClust)
  library(clustertend)
  library(seriation)
  #library(xda)
  library(lubridate)
  library(mice)
  library(VIM)
  library(xts)
  library(broom)
  library(tidyr)
  library(ROSE)
  library(DMwR)
  library(doParallel)
  library(Boruta)
  setwd("C:\\Gaurav\\Project\\TestCBModel\\nSlice\\8\\op\\")
  
  # these are the options for scientific notation and digits precision. SuVN
  options(scipen = 999)
  options(digits = 3)
  
  flog.appender(appender.file("CBLog.log"), name = 'logger.b')
  
  #(TODO)Profile your programs to see how much time is being spent in each function.
  # You can accomplish this with the Rprof() and summaryRprof() functions. The
  # system.time() function can also help. The profr and prooftools packages
  # provide functions that can help in analyzing profiling output
  
}

#Load Model
LoadModel <- function(){

   load("8_down_trainingDf_dfComplete_rf.tune.model", envir = parent.frame(), verbose = TRUE)
}

# Load Test R data
LoadTestData <- function(){
   load('8_dfComplete_testingDF.Rda', envir = parent.frame(), verbose = TRUE)

}


ValidateModelsOnSamples <-
  function(model, nSlice, nameOfDev, val) {
    
    flog.info(
      "Started to validate the model, we have testing set of ",
      dim(val),
      name = 'logger.b',
      capture = TRUE
    )
    pdf(file = paste(nSlice, nameOfDev,"ModelPerformance.pdf",sep = "_"))
    
  
    # registerDoParallel(4,cores=4)
    # getDoParWorkers()
    #val1 <- subset( val, select = -Patient.ID )
    
    model.pred <- predict(model, val)
    model.pred.cm <- confusionMatrix(model.pred, val$IsCb)
    tocsv.model.pred.cm <-
      data.frame(cbind(t(model.pred.cm$overall), t(model.pred.cm$byClass)))
    df  <- data.frame(val$Patient.ID ,val$IsCb, model.pred)
    
    
    write.csv(df, "CM.model.or.pred.csv")

    # Variable Importance
    vaImp1 <- varImp(model)
    flog.info(
      "Variable Importance from model: %s  ",
      vaImp1,
      name = 'logger.b',
      capture = TRUE
    )
    
    
    write.csv(tocsv.model.pred.cm,
              paste(nSlice, nameOfDev, "CM.model.pred.csv", sep = "_"))
    flog.info(
      "Confusion Matrix for GLM model: %s  ",
      tocsv.model.pred.cm,
      name = 'logger.b',
      capture = TRUE
    )
  }



ExecMain <- function(){
  InitEnv()
 LoadModel()
  LoadTestData()
  summary(rf.tune)
  summary(testingDF)
  str(testingDF)
  
  ValidateModelsOnSamples(rf.tune, 8, "abc", testingDF) 
}

ExecMain()


cm.model.pred <-read.csv("CM.model.or.pred.csv")
data <- cm.model.pred
attach(data)
table(model.pred,val.IsCb)
table(val.IsCb,model.pred) 

levels(val.IsCb)[2]
levels(model.pred)
confusionMatrix(model.pred, val.IsCb)

# wikipedia
#59/(59+31)
# = TP/(P) = 0.656 = sensitivity
# 212/(212+68)

# = TN/(N) = 0.757 = specificity

# Going By CM library
# Sensivity = 0.757
# specificity = 0.656



