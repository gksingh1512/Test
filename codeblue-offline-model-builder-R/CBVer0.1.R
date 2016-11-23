

# There are three basic criterion for a series to be classified as stationary series :
# The mean of the series should not be a function of time rather should be a constant
# homoscedasticity.: The variance of the series should not a be a function of time. It should be a constant
# The covariance of the i th term and the (i + m) th term should not be a function of time. It should be a constant


InitEnv <- function() {
  # To init the environment
  #
  #
  #
  # Returns: Nothing
  # Error handling
  
  closeAllConnections()
  rm(list = ls())
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
  
  setwd("C:\\Gaurav\\Project\\CB\\op\\")
  # these are the options for scientific notation and digits precision. SuVN
  options(scipen = 999)
  options(digits = 3)
  
  flog.appender(appender.file("CBLog.log"), name = 'logger.b')
  
  #(TODO)Profile your programs to see how much time is being spent in each function.
  # You can accomplish this with the Rprof() and summaryRprof() functions. The
  # system.time() function can also help. The profr and prooftools packages
  # provide functions that can help in analyzing profiling output
  
}


LoadDF <- function() {
  # Loads the Dataframe into the environment
  #
  #
  #
  # Returns: DF
  # Error handling
  # if (k <=1 ) {
  #   stop("There is no meaning tryin to understand this : ",
  #        x, ".")
  # }
  # Patient.ID -> PatientId
  # Visit_ID -> Id given during each visit to the patient.
  # CB_Time	-> Code blue time. (I am not sure, how are they getting it.) //  Vijay sir, can give his comments here.
  # Obs_Time ->	Observation time (Time at which vitals are generated)
  # HR	-> Heart rate
  # RR -> Respiratory rate
  # TEMP.C -> Temp in celsius
  # ART.DBP ->  Artery Diastolic blood pressure
  # ART.SBP -> Artery Diastolic blood pressure
  # ART.MBP -> Artery mean blood pressure
  # NBP-D -> (Non-invasive Blood Pressure-Diastolic)
  # NBP-S ->(Non-invasive Blood Pressure-Systolic)
  # NBP-M -> (Non-invasive Blood Pressure-Mean)
  path <-
  "C:\\Gaurav\\Project\\CB\\CBdata\\vitalData_For_CB\\"

  files <- list.files(path = path, pattern = "*.csv")
  listofDataFrames <- list()
  i <- 1
  for (file in files)
  {
    perpos <- which(strsplit(file, "")[[1]] == ".")
    listofDataFrames[[i]] <-
      assign(gsub(" ", "", substr(file, 1, perpos -
                                    1)),
             read.csv(paste(path, file, sep = "")))
    listofDataFrames[[i]]$fileName <- file
    
    i <- i + 1
  }
  # The cb-set5 has additional spaces and - marks in it. this is read as additional columns.
  # system.time({ df.Base <- do.call("rbind", listofDataFrames) })
  df <- do.call("rbind", listofDataFrames)
  names(df) <- sub(" ", ".", names(df))
  names(df) <- sub("_", ".", names(df))
  setwd("C:\\Gaurav\\Project\\CB\\op\\")
  return(df)
  
}
LoadDF_ncb <- function() {
  # Loads the Dataframe into the environment
  #
  #
  #
  # Returns: DF
  # Error handling
  # if (k <=1 ) {
  #   stop("There is no meaning tryin to understand this : ",
  #        x, ".")
  # }
  # Patient.ID -> PatientId
  # Visit_ID -> Id given during each visit to the patient.
  # CB_Time	-> Code blue time. (I am not sure, how are they getting it.) //  Vijay sir, can give his comments here.
  # Obs_Time ->	Observation time (Time at which vitals are generated)
  # HR	-> Heart rate
  # RR -> Respiratory rate
  # TEMP.C -> Temp in celsius
  # ART.DBP ->  Artery Diastolic blood pressure
  # ART.SBP -> Artery Diastolic blood pressure
  # ART.MBP -> Artery mean blood pressure
  # NBP-D -> (Non-invasive Blood Pressure-Diastolic)
  # NBP-S ->(Non-invasive Blood Pressure-Systolic)
  # NBP-M -> (Non-invasive Blood Pressure-Mean)
  
  
  path <-
    "C:\\Gaurav\\Project\\CB\\NonCBdata\\nonCBDataSet\\"
  files <- list.files(path = path, pattern = "*.csv")
  listofDataFrames0 <- list()
  i <- 1
  for (file in files)
  {
    perpos <- which(strsplit(file, "")[[1]] == ".")
    listofDataFrames0[[i]] <-
      assign(gsub(" ", "", substr(file, 1, perpos -
                                    1)),
             read.csv(paste(path, file, sep = "")))
    listofDataFrames0[[i]]$fileName <- file
    i <- i + 1
  }
  # The cb-set5 has additional spaces and - marks in it. this is read as additional columns.
  # system.time({ df.Base <- do.call("rbind", listofDataFrames) })
  df.ncb0 <- do.call("rbind", listofDataFrames0)
  names(df.ncb0) <- sub(" ", "_", names(df.ncb0))
  save(df.ncb0, file = "NCBData100.Rda")
  
  
  path <-
  "C:\\Gaurav\\Project\\CB\\NonCBdata\\7thNovNonCB\\"
  files <- list.files(path = path, pattern = "*.csv")
  listofDataFrames <- list()
  i <- 1
  for (file in files)
  {
    perpos <- which(strsplit(file, "")[[1]] == ".")
    listofDataFrames[[i]] <-
      assign(gsub(" ", "", substr(file, 1, perpos -
                                    1)),
             read.csv(paste(path, file, sep = "")))
    listofDataFrames[[i]]$fileName <- file
    i <- i + 1
  }
  # The cb-set5 has additional spaces and - marks in it. this is read as additional columns.
  # system.time({ df.Base <- do.call("rbind", listofDataFrames) })
  df.ncb <- do.call("rbind", listofDataFrames)
  names(df.ncb) <- sub(" ", "_", names(df.ncb))
  save(df.ncb, file = "NCBData7Nov.Rda")
  
  #Read the
  path <-
  "C:\\Gaurav\\Project\\CB\\NonCBdata\\8thNovNonCB\\"
  files <- list.files(path = path, pattern = "*.csv")
  listofDataFrames1 <- list()
  i <- 1
  for (file in files)
  {
    perpos <- which(strsplit(file, "")[[1]] == ".")
    listofDataFrames1[[i]] <-
      assign(gsub(" ", "", substr(file, 1, perpos -
                                    1)),
             read.csv(paste(path, file, sep = "")))
    listofDataFrames1[[i]]$fileName <- file
    i <- i + 1
  }
  # The cb-set5 has additional spaces and - marks in it. this is read as additional columns.
  # system.time({ df.Base <- do.call("rbind", listofDataFrames) })
  df.ncb1 <- do.call("rbind", listofDataFrames1)
  names(df.ncb1) <- sub(" ", "_", names(df.ncb1))
  
  setwd("C:\\Gaurav\\Project\\CB\\op\\")
  save(df.ncb1, file = "NCBData8Nov.Rda")
  
  df <- rbind(df.ncb1, df.ncb)
  flog.info("Started binding nonCbdata with 7thNov and 8thNov", name = 'logger.b')
  df <- rbind(df.ncb0, df)
  flog.info("Binding of nonCbdata with 7thNov and 8thNov finished", name = 'logger.b')
  return(df)
  
}


ExploreDataHighLevel <- function(RawDataFrame) {
  # Explores the data and prints out the high level stats of a DF
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   Nothing
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   # stop(" Arguments dataframe  has missing values.")
  #
  # }
  dim(RawDataFrame)
  # summary(RawDataFrame)
  # stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
  # where x is a data frame or time series. If basic=TRUE (the default), the number of values,
  # null values, missing values, minimum, maximum, range, and sum are provided. If
  # desc=TRUE (also the default), the median, mean, standard error of the mean, 95 percent
  # confidence interval for the mean, variance, standard deviation, and coefficient of
  # variation are also provided. Finally, if norm=TRUE (not the default), normal distribution
  # statistics are returned, including skewness and kurtosis (and their statistical significance),
  # and the ShapiroâWilk test of normality. A p-value option is used to calculate
  # the confidence interval for the mean (.95 by default). Listing 7.4 gives an example.
  s1 <- stat.desc(RawDataFrame)
  print(s1)
  write.csv(s1, paste0(Append.Date.Time("InitialSummary"), ".csv"))
  print(str(RawDataFrame))
  print(names(RawDataFrame))
  
}
ExploreIntervalData <- function() {
  # To Explore the final df forExploration No modelling as yet
  #
  # Args: dfInterval
  #
  # Returns: None as of now
  # Error handling
  # n <- length(dfInterval)
  # # Error handling
  # if (n <= 1) {
  #   stop("The dataframe doesnt have any values lengths: ",
  #        length(dfInterval),
  #        ".")
  # }
  dfMeans <- read.csv("ImputedIntrvlMeans.csv")
  str(dfMeans)
}

CreateDummyDF <- function() {
  lsNormVals <- list()
  names <-
    c(
      "HR",
      "RR",
      "TEMP.C",
      "ART.DBP",
      "NBP.D",
      "ART.SBP",
      "NBP.S",
      "ART.MBP",
      "NBP.M",
      "CVP",
      "SPO2"
    )
  values <-
    c(80, 20, 36, 80, 80, 120, 120, 105, 105, 8, 99)
  lsNormVals <- setNames(as.list(values), names)
  dummyDf <- as.data.frame(lsNormVals)
  return(dummyDf)
}

ImputeColsWithNorms <- function(RawDataFrame) {
  # To Impute  each of the cols with normal values
  #
  # Args: RawDf
  #
  # Approximate Age Range	Heart Rate
  # Newborn	100-160
  # 0-5 months	90-150
  # 6-12 months	80-140
  # 1-3 years	80-130
  # 3-5 years	80-120
  # 6-10 years	70-110
  # 11-14 years	60-105
  # 15-20 years	60-100
  # Adults	50-80
  
  # Approximate Age Range	Respiratory Rate
  # Newborn	30-50
  # 0-5 months	25-40
  # 6-12 months	20-30
  # 1-3 years	20-30
  # 3-5 years	20-30
  # 6-10 years	15-30
  # 11-14 years	12-20
  # 15-20 years	12-30
  # Adults	16-20
  
  # Approximate Age Range	Systolic Range	Diastolic Range
  # 1-12 months	75-100	50-70
  # 1-4 years	80-110	50-80
  # 3-5 years	80-110	50-80
  # 6-13 years	85-120	55-80
  # 13-18 years	95-140	60-90
  
  
  # Parameter	                      Equation	      Normal Range
  # Arterial Blood Pressure (BP)	Systolic (SBP)	90 - 140 mmHg
  # Diastolic (DBP)	                              60 - 90 mmHg
  # Mean Arterial Pressure (MAP)	(SBP + 2 x DBP)/3	70 - 105 mmHg
  # Systolic Pressure Variation (SPV)	(SPmax-SPmin)	<5 mmHg unlikely to be preload responsive
  # >5mmHg likely to be preload responsive
  
  
  # Normal range for CVP is 2-8 cm H20 or 2-6 mmHg (TODO)We need to check our units in data
  # Normal SpO2 values vary between 95 and 99%.
  
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   #stop(" Arguments dataframe  has missing values.")
  # }
  
  # create a key value pair
  lsNormVals <- list()
  names <-
    c(
      "HR",
      "RR",
      "TEMP.C",
      "ART.DBP",
      "NBP.D",
      "ART.SBP",
      "NBP.S",
      "ART.MBP",
      "NBP.M",
      "CVP",
      "SPO2"
    )
  values <-
    c(80, 20, 36, 80, 80, 120, 120, 105, 105, 8, 99) # these have to be vetted by smes and these have to be grpd by age
  lsNormVals <- setNames(as.list(values), names)
  
  RawDataFrame$HR[is.na(RawDataFrame$HR)] <- lsNormVals$HR
  RawDataFrame$RR[is.na(RawDataFrame$RR)] <- lsNormVals$RR
  RawDataFrame$TEMP.C[is.na(RawDataFrame$TEMP.C)] <-
    lsNormVals$TEMP.C
  RawDataFrame$ART.DBP[is.na(RawDataFrame$ART.DBP)] <-
    lsNormVals$ART.DBP
  RawDataFrame$NBP.D[is.na(RawDataFrame$NBP.D)] <- lsNormVals$NBP.D
  RawDataFrame$ART.SBP[is.na(RawDataFrame$ART.SBP)] <-
    lsNormVals$ART.SBP
  RawDataFrame$NBP.S[is.na(RawDataFrame$NBP.S)] <- lsNormVals$NBP.S
  RawDataFrame$ART.MBP[is.na(RawDataFrame$ART.MBP)] <-
    lsNormVals$ART.MBP
  RawDataFrame$NBP.M[is.na(RawDataFrame$NBP.M)] <- lsNormVals$NBP.M
  RawDataFrame$CVP[is.na(RawDataFrame$CVP)] <- lsNormVals$CVP
  RawDataFrame$SPO2[is.na(RawDataFrame$SPO2)] <- lsNormVals$SPO2
  
  # s<-numSummary(RawDataFrame)
  # print(s)
  return(RawDataFrame)
}

# (TODO)
ImputeOutliersWithX <- function(RawDataFrame) {
  
}
#(TODO)
ImputeZerosWithX <- function(RawDataFrame) {
  
}
#(TODO)
ImputeNANsWithX <- function(RawDataFrame) {
  
}
#(TODO)
ImputeInfWithX <- function(RawDataFrame) {
  
}

DoFeatEng <- function(RawDataFrame) {
  # To Create additional columns that explin the latent features in a better way
  #
  # Args: RawDataFrame
  #
  # Returns: None as of now
  # Error handling
  n <-  nrow(RawDataFrame)
  # # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  attach(RawDataFrame)
  # HR	RR	TEMP.C	ART.DBP	NBP.D	ART.SBP	NBP.S	ART.MBP	NBP.M	CVP	SPO2
  
  RawDataFrame$LV.ShkIdx <- HR * NBP.S
  # Oxygen delivery is estimated by multiplying heart rate, pulse pressure (the
  # difference between systolic and diastolic blood pressures), oxygen
  # saturation, and hemoglobin
  
  RawDataFrame$LV.O2Del <- HR * (NBP.S - NBP.D) * SPO2
  
  #oxygen delivery  is dependent on the pressure gradient across the tissue bed, which is actually
  #the gradient between the mean arterial pressure and the central venous pressure is important.
  RawDataFrame$LV.GrdMapCvp <-
    NBP.M - CVP # (TODO)check with the SME
  
  
  detach(RawDataFrame)
  
  
}


ImputeIntervalsWithX <- function(dfInterval, ImputeType = "Mean") {
  # To create the final df forExploration No modelling as yet
  #
  # Args: RawDf
  #
  # Returns: Final DF The final step required to standardize the formatting of
  # candidate features is to transform the native measurement resolution to the
  # resolution specified by the windowing parameters. When the number of native
  # measurements in a given time period preceding or following the reference
  # point exceeds the desired number of measurements specified, a reduction
  # strategy is needed. Several options include selecting the mean, median or
  # mode, maximum, or minimum of the native measurements. When the number of
  # native measurements is less than the desired number of measurements
  # specified, an imputation strategy is needed. Several options include
  # imputing a normal value, a mean, median, or mode value from the data, or
  # carrying forward previous data points.
  
  # Less observations:Needs a imputation strategy
  # â¢	0- No observations at all for all the intervals:	Impute by normal values per Age
  # â¢	6 full-6 empty in between the intervals: At least one observation is there per interval i, but its immediate neighbor has 0 observations in the next interval  i+1 or i-1 th cell, carry fwd the i th observation to the  i+1 or i-1 th cell. (this works only if at least 6 intervals are filled and 6 are empty). If there is no prev obsvn, then use normal values as per step1
  # â¢	8 full-4 empty in between: use the same carry fwd strategy as outlined above
  # â¢	11 full - 1 empty: Use rand forests or regression to predict the missing value or simply carry forward.
  
  
  
  # More observations- Needs a reduction strategy
  # â¢	30-60 values per patient are collected:  observe the slope see if it is increasing OR decreasing and explore if Min or Max is the best way to impute. its like a population.
  # â¢	10-30 values per patient are collected: get the slope, examine the skew and kurtosis. then make decision to impute by mean median or mode. Its like a sample
  # â¢	<10 values per patient are collected: Make it using mean. Safer thus.
  #
  
  # For age based imputation, dont create a fn to take age, hr value and get
  # imputed value. create a col based on age and fill it initially itself with
  # age based normal values. Use these normal values later to perform imputation
  
  
  n <- length(dfInterval)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(dfInterval),
         ".")
  }
  names <-
    c(
      "M_HR",
      "M_RR",
      "M_TEMP.C",
      "M_ART.DBP",
      "M_NBP.D",
      "M_ART.SBP",
      "M_NBP.S",
      "M_ART.MBP",
      "M_NBP.M",
      "M_CVP",
      "M_SPO2"
    )
  dfm <- list()
  i <- 1
  if (ImputeType == "Mean") {
    for (name in names) {
      dfm[[i]] <-
        as.data.frame(t(apply(select(dfInterval, starts_with(name)), 1, function(xv) {
          xv[is.na(xv)] <- mean(xv, na.rm = TRUE)
          return(xv)
        })))
      i <- i + 1
    }
    df <- do.call("cbind", dfm)
    df <- cbind(df, dfInterval$Patient.ID)
    # select(df,dfInterval$Patient.ID,everything())
    # rename(df,Patient.ID=dfInterval$Patient.ID )
    # there will still be NAs as a result of Mean imputation. fill it with a BIG negative number
    df[is.na(df)] <- -99999
    write.csv(df, paste0(Append.Date.Time("ImputedIntrvlMeans"), ".csv"))
  }
  
  if (ImputeType == "Median") {
    for (name in names) {
      dfm[[i]] <-
        as.data.frame(t(apply(select(dfInterval, starts_with(name)), 1, function(xv) {
          xv[is.na(xv)] <- Median(xv, na.rm = TRUE)
          return(xv)
        })))
      i <- i + 1
    }
    df <- do.call("cbind", dfm)
    df <- cbind(df, dfInterval$Patient.ID)
    select(df, Patient.ID, everything())
    # there will still be NAs as a result of Mean imputation. fill it with a BIG negative number
    df[is.na(df)] <- -99999
    write.csv(df, paste0(Append.Date.Time("ImputedIntrvlMedians"), ".csv"))
    
    
  }
  
  if (ImputeType == "Mode") {
    for (name in names) {
      dfm[[i]] <-
        as.data.frame(t(apply(select(dfInterval, starts_with(name)), 1, function(xv) {
          xv[is.na(xv)] <- Mode(xv, na.rm = TRUE)
          return(xv)
        })))
      i <- i + 1
    }
    df <- do.call("cbind", dfm)
    df <- cbind(df, dfInterval$Patient.ID)
    select(df, Patient.ID, everything())
    # there will still be NAs as a result of Mean imputation. fill it with a BIG negative number
    df[is.na(df)] <- -99999
    write.csv(df, paste0(Append.Date.Time("ImputedIntrvlMode"), ".csv"))
  }
  return(df)
}

CreateIntervalsDf <- function(RawDataFrame, nSlice) {
  # To create the final df forExploration No modelling as yet
  #
  # Args: RawDf
  #
  # Returns: Final DF
  # Error handling
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # This creates the individual col means for each pat obsvsn in each interval
  finaldfListMean <- list()
  finaldfListCount <- list()
  i <- 1
  for (patient in  unique(RawDataFrame$Patient.ID)) {
    sampl1 <- filter(RawDataFrame, Patient.ID == patient)
    finaldfListMean[[i]] <-
      GetARowPerPatWtMeans(patient, sampl1, nSlice)
    finaldfListCount[[i]] <-
      GetARowPerPatWtCounts(patient, sampl1, nSlice)
    i <- i + 1
  }
  
  dfMean <- do.call("rbind", finaldfListMean)
  dfMean <-
    select(
      dfMean,
      Patient.ID,
      # GapBtwLastOb_Cb,
      GapBtwLastOb_1stObs,
      starts_with("HR"),
      starts_with("RR"),
      starts_with("TEMP.C"),
      starts_with("ART.DBP"),
      starts_with("NBP.D"),
      starts_with("ART.SBP"),
      starts_with("NBP.S"),
      starts_with("ART.MBP"),
      starts_with("NBP.M"),
      starts_with("SPO2"),
      starts_with("CVP"),
      everything()
    )
  
  dfCount <- do.call("rbind", finaldfListCount)
  dfCount <-
    select(
      dfCount,
      Patient.ID,
      # GapBtwLastOb_Cb,
      GapBtwLastOb_1stObs,
      starts_with("HR"),
      starts_with("RR"),
      starts_with("TEMP.C"),
      starts_with("ART.DBP"),
      starts_with("NBP.D"),
      starts_with("ART.SBP"),
      starts_with("NBP.S"),
      starts_with("ART.MBP"),
      starts_with("NBP.M"),
      starts_with("SPO2"),
      starts_with("CVP"),
      everything()
    )
  
  write.csv(dfMean, paste0(Append.Date.Time("IntrvlMeans"), ".csv"))
  write.csv(dfCount, paste0(Append.Date.Time("IntrvlCount"), ".csv"))
  return(dfMean)
  
}
ageImputation <- function(dataFrame) {
  #has zero
  if (is.element('0', dataFrame$Age)) {
    dataFrame$Age <- 1
    pid.age <- c(unique(dataFrame$PID), unique(dataFrame$Age))
    
  }
  
  #has 115 or 116
  else if (is.element('115', dataFrame$Age) |
           is.element('116', dataFrame$Age))
  {
    dataFrame$Age <- min(dataFrame$Age)
    pid.age <- c(unique(dataFrame$PID), unique(dataFrame$Age))
    
  }
  
  #more than one value or General
  else{
    dataFrame$Age <- max(dataFrame$Age)
    pid.age <- c(unique(dataFrame$PID), unique(dataFrame$Age))
    
  }
  
  paste('pid.age ', pid.age)
  return (pid.age)
}
CreateNormalsDFFromAge <- function(Df) {
  lsNormVals <- list()
  names <-
    c(
      "HR",
      "RR",
      "TEMP.C",
      "ART.DBP",
      "NBP.D",
      "ART.SBP",
      "NBP.S",
      "ART.MBP",
      "NBP.M",
      "CVP",
      "SPO2"
    )
  # this is just an example to show how to do it....in the values list please fill from look-up table or some assumptions from google/
  for (i in 1:length(Df$Age))
  {
    if (Df$Age[i] < 1) {
      values <-
        c(120, 32.5, 36.8, 60, 50, 95, 55, 71.6667, 51.6667, 5, 97) # Value of CVP(10), SPO2(11) are randomly taken as no specific look-up available as yet
      
      # Values of ART.DBP,ART.SBP taken as the range given in look up table
      # Values of NBP.D,NBP.S(which means NBP.M too ) taken randomly as no specific table is avaliable is fouond for them
    }
    
    if (Df$Age[i] >= 1 && Df$Age[i] <= 3) {
      values <-
        c(105, 25, 36.8, 65, 50, 95, 55, 75, 51.6667, 5, 97)  # Value of CVP(10), SPO2(11) are randomly taken as no specific look-up available as yet
      # Values of ART.DBP,ART.SBP taken as the range given in look up table
      # Values of NBP.D,NBP.S(which means NBP.M too ) taken randomly as no specific table is avaliable is fouond for them
      
    }
    
    if (Df$Age[i] > 3 && Df$Age[i] <= 5) {
      values <-
        c(100, 25, 36.8, 65, 50, 95, 55, 75, 51.6667, 5, 97)  # Value of CVP(10), SPO2(11) are randomly taken as no specific look-up available as yet
      
      # Values of ART.DBP,ART.SBP taken as the range given in look up table
      # Values of NBP.D,NBP.S(which means NBP.M too ) taken randomly as no specific table is avaliable is fouond for them
    }
    
    if (Df$Age[i] > 5 && Df$Age[i] <= 10) {
      values <-
        c(90, 22.5, 36.8, 67.5, 50, 102.5, 55, 79.16667, 51.6667, 5, 97)  # Value of CVP(10), SPO2(11) are randomly taken as no specific look-up available as yet
      
      # Values of ART.DBP,ART.SBP taken as the range given in look up table
      # Values of NBP.D,NBP.S(which means NBP.M too ) taken randomly as no specific table is avaliable is fouond for them
    }
    
    if (Df$Age[i] > 10 && Df$Age[i] <= 13) {
      values <-
        c(82.5, 16, 36.8, 67.5, 50, 102.5, 55, 79.16667, 51.6667, 5, 97)  # Value of CVP(10), SPO2(11) are randomly taken as no specific look-up available as yet
      
      # Values of ART.DBP,ART.SBP taken as the range given in look up table
      # Values of NBP.D,NBP.S(which means NBP.M too ) taken randomly as no specific table is avaliable is fouond for them
    }
    
    if (Df$Age[i] > 13 && Df$Age[i] <= 14) {
      values <-
        c(82.5, 16, 36.8, 75, 50, 117.5, 55, 89.16667, 51.6667, 5, 97)  # Value of CVP(10), SPO2(11) are randomly taken as no specific look-up available as yet
      
      # Values of ART.DBP,ART.SBP taken as the range given in look up table
      # Values of NBP.D,NBP.S(which means NBP.M too ) taken randomly as no specific table is avaliable is fouond for them
    }
    
    if (Df$Age[i] > 14 && Df$Age[i] <= 18) {
      values <-
        c(80, 21, 36.8, 75, 50, 117.5, 55, 89.16667, 51.6667, 5, 97)  # Value of CVP(10), SPO2(11) are randomly taken as no specific look-up available as yet
      
      # Values of ART.DBP,ART.SBP taken as the range given in look up table
      # Values of NBP.D,NBP.S(which means NBP.M too ) taken randomly as no specific table is avaliable is fouond for them
    }
    
    if (Df$Age[i] > 18 && Df$Age[i] <= 20) {
      values <-
        c(80, 21, 36.8, 80, 50, 120, 55, 93.3333, 51.6667, 5, 97)  # Value of CVP(10), SPO2(11) are randomly taken as no specific look-up available as yet
      
      # Values of ART.DBP,ART.SBP taken as the range given in look up table
      # Values of NBP.D,NBP.S(which means NBP.M too ) taken randomly as no specific table is avaliable is fouond for them
    }
    
    if (Df$Age[i] > 20) {
      values <-
        c(65, 18, 36.8, 80, 50, 120, 55, 93.3333, 51.6667, 5, 97)  # Value of CVP(10), SPO2(11) are randomly taken as no specific look-up available as yet
      
      # Values of ART.DBP,ART.SBP taken as the range given in look up table
      # Values of NBP.D,NBP.S(which means NBP.M too ) taken randomly as no specific table is avaliable is fouond for them
    }
    lsNormVals <- setNames(as.list(values), names)
    dummyDf <- as.data.frame(lsNormVals)
    ## multiple if statements to capture ages from 30 to 100 and above
    #followed by multiple values hardcoded based on google finding.
    
  } # eof for
  
  return(dummyDf)
}
norm_age_vit <- function(d) {
  lsNormVals <- list()
  names <-
    c(
      "HR",
      "RR",
      "TEMP.C",
      "ART.DBP",
      "NBP.D",
      "ART.SBP",
      "NBP.S",
      "ART.MBP",
      "NBP.M",
      "CVP",
      "SPO2"
    )
  
  hr <- sapply(d$Age , function(x)
    ifelse(
      x < 1,
      mean_hr_rr_age_less_than_zero(100, 160, 90, 150, 80, 140),
      ifelse(x >= 1 &
               x <= 3, mean(c(80, 130)) ,
             ifelse(
               x > 3 & x <= 5, mean(c(80, 120)), ifelse(x >= 6 & x <= 10,
                                                        mean(c(70, 110)), ifelse(
                                                          x >= 11 & x <= 14, mean(c(60, 105)),
                                                          ifelse(x >= 15 &
                                                                   x <= 20, mean(c(60, 100)), ifelse(x >= 21, mean(c(
                                                                     50, 80
                                                                   )), NA))
                                                        ))
             ))
    ))
  #  print(hr)
  
  
  rr <- sapply(d$Age , function(x)
    ifelse(
      x < 1,
      mean_hr_rr_age_less_than_zero(30, 50, 25, 40, 20, 30),
      ifelse(x >= 1 &
               x <= 3, mean(c(20, 30)) ,
             ifelse(
               x > 3 & x <= 5, mean(c(20, 30)), ifelse(x >= 6 & x <= 10,
                                                       mean(c(15, 30)), ifelse(
                                                         x >= 11 & x <= 14, mean(c(12, 20)),
                                                         ifelse(x >= 15 &
                                                                  x <= 20, mean(c(12, 30)),
                                                                ifelse(x >= 21, mean(c(
                                                                  16, 20
                                                                )), NA))
                                                       ))
             ))
    ))
  #print(rr)
  
  temp.c <- mean(c(36.6, 37))
  #print(temp.c)
  #art.dbp normal values(here values are used from the diastolic pressure's table)
  
  art.dbp <- sapply(d$Age , function(x)
    ifelse(x < 1, mean(c(50, 70)),
           ifelse(
             x >= 1 & x <= 5, mean(c(50, 80)) ,
             ifelse(x >= 6 &
                      x <= 13, mean(c(55, 80)),
                    ifelse(
                      x >= 14 & x <= 18, mean(c(60, 90)),
                      ifelse(x >= 19, 80, NA)
                    ))
           )))
  #print(art.dbp)
  
  nbp.d <- mean(c(40, 60))
  #print(nbp.d)
  
  #art.sbp nomral values
  
  art.sbp <-
    sapply(d$Age , function(x)
      ifelse(x < 1, mean(c(80, 110)) , ifelse(
        x >= 1 & x <= 5, mean(c(80, 110)) ,
        ifelse(x >= 6 &
                 x <= 13, mean(c(85, 120)),
               ifelse(
                 x >= 14 & x <= 18, mean(c(95, 140)),
                 ifelse(x >= 19, 120, NA)
               ))
      )))
  #print(art.sbp)
  
  nbp.s <- mean(c(30, 80))
  #print(nbp.s)
  #art.mbp normal values
  art.mbp <- mean(c(art.dbp, art.sbp))
  #print(art.mbp)
  
  #nbp.m normal values(random values taken)
  nbp.m <- mean(c(nbp.d, nbp.s))
  #print(nbp.m)
  #cvp normal values
  cvp <- mean(c(50, 80))  # values randomly taken
  #print(cvp)
  #spo2 normal values
  spo2 <- mean(c(80, 100))   # value randomly taken
  #print(spo2)
  values <-
    c(hr,
      rr,
      temp.c,
      art.dbp,
      nbp.d,
      art.sbp,
      nbp.s,
      art.mbp,
      nbp.m,
      cvp,
      spo2)
  
  #lsNormVals <- as.data.frame(setNames(as.list(values), names))
  lsNormVals <- CreateDummyDF()
  return (lsNormVals)
}


mean_hr_rr_age_less_than_zero <-
  function(low1, high1, low2, high2, low3, high3)
  {
    return((mean(c(low1, high1)) + mean(c(low2, high2)) + mean(c(low3, high3))) /
             3)
  }

createDFOneAgePerPatient <-
  function(CompleteRawDataFrame, forwhom = "NCB") {
    # Tryng to solve the problem of same ps having multiple problems- better write a diff fun altogether since cb is totally different and ncb is differnet
    
    
    dataFrame <- ReadAgeForCB()   # Load the data
    
    # Create a data frame for each patient and call the imputation method
    listOfDataFrames <- vector(mode = "list", length = 100)
    for (i in 1:length(unique(dataFrame$PID))) {
      patientID = unique(dataFrame$PID)[i]
      #print(patientID)
      dataPerPatient <- subset(dataFrame, dataFrame$PID == patientID)
      #print(dataPerPatient)
      listOfDataFrames[[i]] <- ageImputation(dataPerPatient)
      
    }
    
    # Create dataFrame with all the vectors and write the dataFrame in csv file.
    df <- do.call("rbind", listOfDataFrames)
    df <- as.data.frame(df)
    df <- setNames(df, c("Patient.ID", "Age"))
    
    #this is for making a merge of the extra patients in the new ages
    unP <- as.data.frame(unique(CompleteRawDataFrame$Patient.ID))
    unP <- setNames(unP, "Patient.ID")
    df <- merge(unP, df, by = "Patient.ID", all.x = TRUE)
    
    write.csv(df, paste0(Append.Date.Time("AgePerPatientAfterImputation"), ".csv"))
    return(df)
  }

GetCBAgeFromPatientID <- function(PatientID) {
  gAgeDf <- get('gAgeDf', .GlobalEnv)
  #agedf<-createDFOneAgePerPatient()
  age <- gAgeDf$Age[gAgeDf$Patient.ID == PatientID]
  return(age)
}

# 11 ----------------------------------------------------------------------

GetDFForCB <-function() {
  
  flog.info("Started Loading CB data", name = 'logger.b')
  RawDataFrame <- LoadDF()
  flog.info("Started Transforming Numeric CB data for %s",  name = 'logger.b')
  #RawDataFrame<-TransformAllNumericData(RawDataFrame)
  #(TODO) since age is not part of the CB data, this fn fails. hence handle it sep
  flog.info("Started Transforming Categ CB data for %s",  name = 'logger.b')
  RawDataFrame <- TransformAllCategData(RawDataFrame)
  flog.info("Started Transforming TimeSeries CB data for %s",  name = 'logger.b')
  RawDataFrame <- TransformDateTimeData(RawDataFrame)
  
  return(RawDataFrame)
}
GetDFForNCB <-function() {
    
    flog.info("Started Loading NCB data", name = 'logger.b')
    ncbdf <- LoadDF_ncb()
    flog.info("Started Transforming Numeric NCB data for ", name = 'logger.b')
    ncbdf <- TransformAllNumericData(ncbdf)
    flog.info("Started Transforming Categ NCB data for ",  name = 'logger.b')
    ncbdf <- TransformAllCategData(ncbdf)
    flog.info("Started Transforming TimeSeries NCB data for", name = 'logger.b')
    ncbdf <- TransformDateTimeData(ncbdf)
    
    return(ncbdf)
  }
    
RunSliceBasedClassifications_wrapper <-
  function(RawDataFrame, ncbdf) {
    
    # nSlices <- c(8, 12, 16, 20, 24, 36)
    # nSlices <- c(12)
    # nSlices <- c(8, 12)
    nSlices <- c(8)
    
    for (nSlice in nSlices) {
      flog.info("Started building model for %s", nSlice, name = 'logger.b')
      RunSliceBasedClassifications_SampledDev (RawDataFrame, ncbdf, nSlice)
      print(nSlice)
    }
  }

RunSliceBasedClassifications <-
  function(RawDataFrame, ncbdf, nSlice) {
    
    gAgeDf <- data.frame()
    assign('gAgeDf', gAgeDf, .GlobalEnv)
    gAgeDf <- createDFOneAgePerPatient(RawDataFrame)
    
    cb12 <-
      GenerateConseqHourlyDistribution_Wrapper(RawDataFrame, nSlice, "CB")
    flog.info("Done with CB data generation  for %s  slice", nSlice, name = 'logger.b')
    ncbdf12 <-
      GenerateConseqHourlyDistribution_Wrapper(ncbdf, nSlice, "NCB")
    
    flog.info("Done with NCB data generation  for %s  slice", nSlice, name = 'logger.b')
    
    ModellingDataFrame <-
      PrepareFinalDataForModel(ncbdf12, cb12, "No", nSlice)
    
    flog.info("Done PrepareFinalDataForModel  for %s  slice", nSlice, name = 'logger.b')
    
    modelObject <-
      PerformClassification(ModellingDataFrame, nSlice)
    
    flog.info("Done PerformClassification  for %s  slice", nSlice, name = 'logger.b')
    
    ValidateModels(modelObject, nSlice)
    
    flog.info("Done ValidateModels  for %s  slice", nSlice, name = 'logger.b')
    
  }
RunSliceBasedClassifications_SampledDev <-
  function(RawDataFrame, ncbdf, nSlice) {
    
    gAgeDf <- data.frame()
    # assign('gAgeDf', gAgeDf, .GlobalEnv)
    # gAgeDf <- createDFOneAgePerPatient(RawDataFrame)
    cb12 <-
      GenerateConseqHourlyDistribution_Wrapper(RawDataFrame, nSlice, "CB")
    flog.info("Done with CB data generation  for %s  slice", nSlice, name = 'logger.b')
    ncbdf12 <-
      GenerateConseqHourlyDistribution_Wrapper(ncbdf, nSlice, "NCB")

    flog.info("Done with NCB data generation  for %s  slice", nSlice, name = 'logger.b')

    ModellingDataFrame <-
      PrepareFinalDataForModel(ncbdf12, cb12, "No", nSlice)

    flog.info("Done PrepareFinalDataForModel  for %s  slice", nSlice, name = 'logger.b')
#-------------------------------------- Changes --------------------------------------------------#
    load("ModellingDataFrame.Rda")
    str(ModellingDataFrame$IsCb)
    table(ModellingDataFrame$IsCb)
    View(ModellingDataFrame$IsCb)
    
    abc <- ModellingDataFrame
    abc$IsCb <- as.numeric(abc$IsCb)
    CA <- ifelse(abc$IsCb=="Yes", 1, 0)
    table(CA)
    ModellingDataFrame$IsCb <- CA
    as.factor(ModellingDataFrame$IsCb)
#---------------------------------------------------------------------------------------------------------#    
    str(ModellingDataFrame)
    lstFeatSelectedDfs <-
      GetListOfFinalizedDfs(ModellingDataFrame, nSlice)
    
    View(lstFeatSelectedDfs$dfFeatSelBoruta$IsCb)
    
#---------------------------------------------------------------------------------------------------------#    
    
    save(lstFeatSelectedDfs, file = "lstFeatSelectedDfs.Rda")
    load("lstFeatSelectedDfs.Rda")
    str(lstFeatSelectedDfs)
    write.csv(lstFeatSelectedDfs$dfFeatSelBoruta, "FeatSelBoruta.csv")
   
    
     lstFeatSelectedDfs$dfFeatSelBoruta
    
    lstFeatSelectedDfs$dfComplete$Patient.ID
    
    
    names(lstFeatSelectedDfs)
    nSlice <- 8
#---------------------------------------------------------------------------------------------------------#    
    
    i <- 1
    for (FinalizedDfs in lstFeatSelectedDfs)  {
      nameOfFinalDf <- names(lstFeatSelectedDfs)[i]
      flog.info(
        "Started to execute the Generation of Test and Training data for these Feat Selected dfs:",
        nameOfFinalDf,
        name = 'logger.b',
        capture = TRUE
      )
      
      #str(lstFeatSelectedDfs)

      testingDF <- GenSampledDataForTesting(FinalizedDfs, nSlice)
      #str(testingDF)
      #save(testingDF, file = i+"testingDF.Rda")
      #save(testingDF, file = paste(nSlice,names(lstFeatSelectedDfs)[i] , "testingDF.Rda", sep = "_"))
    
      lstDevData <-
         GenSampledDataForTraining(FinalizedDfs, nSlice)
      #save(lstDevData, file = i+"lstDevData.Rda")
      #save(lstDevData, file = paste(nSlice,names(lstFeatSelectedDfs)[i] , "trainingDF.Rda", sep = "_"))
      
      
      flog.info(
        "Generated the following number of sampled data frames for training %s",
        length(lstDevData),
        name = 'logger.b',
        capture = TRUE
      )
# 
#       #list.models <-lapply(lstDevData, PerformClassificationOnSampledDev,nSlice)
#       j <- 1
#       for (dev in lstDevData)  {
#         #str(dev)
#         nameOfDev <- names(lstDevData)[j]
#         flog.info(
#           "Begin Classifying the  data Sample*******************************************************************************",
#           nameOfDev,
#           name = 'logger.b',
#           capture = TRUE
#         )
#         nameOfDev <- paste(nameOfDev,nameOfFinalDf,sep = "_")
#         print(nameOfDev)
#         flog.info("Hi..........",nameOfDev)
#         # list.models <-
#         #   PerformClassificationOnSampledDev(dev, nameOfDev, nSlice)
#         # flog.info("Done PerformClassification  for %s  slice", nSlice, name = 'logger.b')
#         # #list.models will contain 4 models. we have to validate all the four
#         # ValidateModelsOnSamples(list.models, nSlice, nameOfDev, testingDF)
#         # flog.info("Done ValidateModels  for %s  slice", nSlice, name = 'logger.b')
#         j <- j + 1
#       } # EO f2

      i <- i+1
    } # EO f1

  }
# function for determining sparseness of variables
sparseness <- function(a) {
  n <- length(a)
  na.count <- sum(is.na(a))
  return((n - na.count)/n)
}

GetDFFromSparseness <- function(ModellingDataFrame){
  # Gets all the  dim reduced data based on analysis of spase data- 
  # This gives poor resuly as most of the cols r removed because of the sparse data. Not to be used in our specific case
  # Args:
  #   ModellingDataFrame: DF which has to be split and sampled
  #
  # Returns:
  
# sparness of input variables based on training subset
variable.sparseness <- apply(ModellingDataFrame, 2, sparseness)
# sparness of input variables based on training subset
variable.sparseness <- apply(ModellingDataFrame, 2, sparseness)

# trim down the subs by removing sparse variables
trimTrainSub <- trainingSub[, variable.sparseness > 0.9]

return(trimTrainSub)
}
GetFeatSelBoruta <- function(ModellingDataFrame){
  # Gets all the possible dim reduced data 
  #
  # Args:
  #   ModellingDataFrame: DF which has to be split and sampled
  #
  # Returns:Reduceddf
  
  flog.info(
    "Started performing FeatSelection on the following dimensions :",
    dim(ModellingDataFrame),
    name = 'logger.b',
    capture = TRUE
  )
  
  set.seed(123)
  registerDoParallel(4,cores=4)
  getDoParWorkers()
  
  boruta.train <- Boruta(IsCb ~ ., data = ModellingDataFrame, doTrace = 2)
 
  
  flog.info(
    "Borutas Output :",
    boruta.train,
    name = 'logger.b',
    capture = TRUE
  )
  
  pdf(file= "BorutaOutput.pdf")
  plot(boruta.train, xlab = "", xaxt = "n")
  lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
    boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
  names(lz) <- colnames(boruta.train$ImpHistory)
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),
         at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
  
  dev.off()
  
  # Now is the time to take decision on tentative attributes.  The tentative 
  # attributes will be classified as confirmed or rejected by comparing the 
  # median Z score of the attributes with the median Z score of the best shadow 
  # attribute.
  final.boruta <- TentativeRoughFix(boruta.train)
  print(final.boruta)
  
  lstSelectedAttributes <- getSelectedAttributes(final.boruta, withTentative = F)
  
  boruta.df <- attStats(final.boruta)
  dim(boruta.df)
  
  dfSelectedAttrib <- ModellingDataFrame[, lstSelectedAttributes]
  flog.info(
    "Boruta Selected the following attributes for modelling :",
    lstSelectedAttributes,
    name = 'logger.b',
    capture = TRUE
  )
  dfSelectedAttrib$IsCb <- ModellingDataFrame$IsCb
  
  return(dfSelectedAttrib)
  
}

GetFeatSelCaret <- function(ModellingDataFrame){
  # Gets all the possible dim reduced data 
  #
  # Args:
  #   ModellingDataFrame: DF which has to be split and sampled
  #
  # Returns:Reduceddf
  
  flog.info(
    "RF Started performing FeatSelection on the following dimensions :",
    dim(ModellingDataFrame),
    name = 'logger.b',
    capture = TRUE
  )
  
  set.seed(123)
  registerDoParallel(4,cores=4)
  getDoParWorkers()
  
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  rfe.train <- rfe(select(ModellingDataFrame,-IsCb), ModellingDataFrame[,c("IsCb")], sizes=1:12, rfeControl=control)
  
  flog.info(
    "RFE Output :",
    rfe.train,
    name = 'logger.b',
    capture = TRUE
  )
  
  pdf(file= "RFE_Output.pdf")
  plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:11)
  
  dev.off()
  
  
  
  lstSelectedAttributes <- predictors(rfe.train)
  
  dfSelectedAttribRFE <- ModellingDataFrame[, lstSelectedAttributes]
  flog.info(
    "RFE Selected the following attributes for modelling :",
    lstSelectedAttributes,
    name = 'logger.b',
    capture = TRUE
  )
  dfSelectedAttribRFE$IsCb <- ModellingDataFrame$IsCb
  
  return(dfSelectedAttribRFE)
}
GetFeatSelLvQ <- function(ModellingDataFrame){
  # Gets all the possible dim reduced data
  # 
  # Args: ModellingDataFrame: DF which has to be split and sampled
  # 
  # Returns:Reduceddf
  # LVQ can be understood as a special case of an artificial
  # neural network, more precisely, it applies a winner-take-all Hebbian
  # learning-based approach. It is a precursor to self-organizing maps (SOM) and
  # related to neural gas, and to the k-Nearest Neighbor algorithm (k-NN).
  flog.info(
    "LVQ Started performing FeatSelection on the following dimensions :",
    dim(ModellingDataFrame),
    name = 'logger.b',
    capture = TRUE
  )
  
  set.seed(123)
  registerDoParallel(4,cores=4)
  getDoParWorkers()
  
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  model <- train(IsCb ~ ., data=ModellingDataFrame, method="lvq", preProcess="scale", trControl=control)
  # estimate variable importance
  importance <- varImp(model, scale=FALSE)
  
  flog.info(
    "LVQ Output :",
    importance,
    name = 'logger.b',
    capture = TRUE
  )
  
  pdf(file= "LVQ_Output.pdf")
  plot(importance)
  
  dev.off()
  
  lstSelectedAttributes <- predictors(importance)
  
  dfSelectedAttribRFE <- ModellingDataFrame[, lstSelectedAttributes]
  flog.info(
    "Boruta Selected the following attributes for modelling :",
    lstSelectedAttributes,
    name = 'logger.b',
    capture = TRUE
  )
  dfSelectedAttribRFE$IsCB <- ModellingDataFrame$IsCb
  
  return(dfSelectedAttribRFE)
}

GetListOfFinalizedDfs <- function(ModellingDataFrame,nSlice){
  
  # Gets all the possible dim reduced data 
  #
  # Args:
  #   ModellingDataFrame: DF which has to be split and sampled
  ## (TODO) Testing whether the results make sense
  # 
  # Letâs assume, Age was the variable which came out to be most important from the above analysis. Here is a simple chi-square test which you can do to see whether the variable is actually important or not.
  # 
  # test <- chisq.test(train$Age, output_vector)
  # print(test)
  
  # Returns:
  flog.info("Inside GetListOfFinalizedDfs", name = 'logger.b')
  dfComplete <- ModellingDataFrame
  dfWithoutH <- select(ModellingDataFrame,-(starts_with("H")))
  dfWithoutS <- select(ModellingDataFrame,-(starts_with("Slp")))
  dfWithoutSnH <- select(dfWithoutH,-(starts_with("Slp")))
  dfFeatSelBoruta <- GetFeatSelBoruta(ModellingDataFrame) 
  dfFeatSelCaret <- GetFeatSelCaret(ModellingDataFrame)
  dfFeatSelCorrel <- GetFeatsSelCorrel(ModellingDataFrame)
  # dfFeatSelSparse <- GetDFFromSparseness(ModellingDataFrame) 
  lstFeatSelectedDfs <- list()
  
  lstFeatSelectedDfs <-
    list(
      dfComplete = dfComplete,
       dfWithoutH = dfWithoutH,
       dfWithoutS = dfWithoutS,
       dfWithoutSnH = dfWithoutSnH,
       dfFeatSelBoruta = dfFeatSelBoruta,
       dfFeatSelCaret = dfFeatSelCaret,
       dfFeatSelCorrel = dfFeatSelCorrel
      # dfFeatSelSparse = dfFeatSelSparse
    )
  
  flog.info("Done with GetListOfFinalizedDfs", length(lstFeatSelectedDfs), name = 'logger.b',capture = TRUE)
  
  return(lstFeatSelectedDfs)
  
}





GenFeat_Wrapper <- function(ModellingDataFrame, nSlice) {
  a1 <- list()
  splitdf <-
    split(ModellingDataFrame, f = ModellingDataFrame$Patient.ID)
  a1 <- lapply(splitdf, GenFeatures, nSlice)
  a1df <- do.call("rbind", a1)
  ModellingDataFrame <- cbind(ModellingDataFrame, a1df)
  return(ModellingDataFrame)
}

GenSlopes_Wrapper <- function(ModellingDataFrame, nSlice) {
  a1 <- list()
  splitdf <-
    split(ModellingDataFrame, f = ModellingDataFrame$Patient.ID)
  a1 <- lapply(splitdf, GenSlopes, nSlice)
  a1df <- do.call("rbind", a1)
  ModellingDataFrame <- cbind(ModellingDataFrame, a1df)
  return(ModellingDataFrame)
}

GenFeatures <- function(ModellingDataFrame, nSlice) {
  ModellingDataFrame <-
    select(
      ModellingDataFrame,
      starts_with("M_HR"),
      starts_with("M_RR"),
      starts_with("M_TEMP.C"),
      starts_with("M_ART.DBP"),
      starts_with("M_NBP.D"),
      starts_with("M_ART.SBP"),
      starts_with("M_NBP.S"),
      starts_with("M_ART.MBP"),
      starts_with("M_NBP.M"),
      starts_with("M_SPO2"),
      starts_with("M_CVP")
    )
  
  xi <- list()
  i <- 1
  for (name in 1:nSlice) {
    n1 <- paste("F_LV.ShkIdx", name, sep = "_")
    n2 <- paste("F_LV.O2Del", name, sep = "_")
    n3 <- paste("F_LV.GrdMapCvp", name, sep = "_")
    
    x <- data.frame(matrix(1:3, 1, 3))
    
    M_HR_i <-
      as.numeric(select(ModellingDataFrame, num_range("M_HR_", name:name)))
    M_NBP.S_i <-
      as.numeric(select(ModellingDataFrame, num_range("M_NBP.S_", name:name)))
    M_NBP.D_i <-
      as.numeric(select(ModellingDataFrame, num_range("M_NBP.D_", name:name)))
    M_SPO2_i <-
      as.numeric(select(ModellingDataFrame, num_range("M_SPO2_", name:name)))
    M_NBP.M_i <-
      as.numeric(select(ModellingDataFrame, num_range("M_NBP.M_", name:name)))
    M_CVP_i <-
      as.numeric(select(ModellingDataFrame, num_range("M_CVP_", name:name)))
    
    
    #x$X1 <- ySamp$M_HR_1 * ySamp$M_NBP.S_1
    # x$X1 <- ySamp[,1] * ySamp[,7]
    x$X1 <- M_HR_i * M_NBP.S_i
    # Oxygen delivery is estimated by multiplying heart rate, pulse pressure (the
    # difference between systolic and diastolic blood pressures), oxygen
    # saturation, and hemoglobin
    
    #x$X2 <- ySamp$M_HR_1 * (ySamp$M_NBP.S_1 - ySamp$M_NBP.D_1) * ySamp$M_SPO2_1
    #x$X2 <- ySamp[,1] * (ySamp[,7] - ySamp[,5]) * ySamp[,11]
    x$X2 <- M_HR_i * (M_NBP.S_i - M_NBP.D_i) * M_SPO2_i
    #oxygen delivery  is dependent on the pressure gradient across the tissue bed, which is actually
    #the gradient between the mean arterial pressure and the central venous pressure is important.
    
    #x$X3 <-ySamp$M_NBP.M_1 - ySamp$M_CVP_1
    #x$X3 <-ySamp[,9] - ySamp[,10]
    x$X3 <- M_NBP.M_i * M_CVP_i
    # (TODO)Systolic Pressure Variation (SPV)	(SPmax-SPmin)	<5 mmHg unlikely to be preload responsive
    # >5mmHg likely to be preload responsive
    
    names(x) <- c(n1, n2, n3)
    xi[[i]] <- x
    i <- i + 1
    #ModellingDataFrame<-cbind(ModellingDataFrame,x)
  }
  y <- do.call("cbind", xi)
  return(y)
}



GenSlopes <- function(ModellingDataFrame, nSlice) {
  ModellingDataFrame <-
    select(
      ModellingDataFrame,
      starts_with("M_HR"),
      starts_with("M_RR"),
      starts_with("M_TEMP.C"),
      starts_with("M_ART.DBP"),
      starts_with("M_NBP.D"),
      starts_with("M_ART.SBP"),
      starts_with("M_NBP.S"),
      starts_with("M_ART.MBP"),
      starts_with("M_NBP.M"),
      starts_with("M_SPO2"),
      starts_with("M_CVP")
    )
  
  names <-
    c(
      "M_HR",
      "M_RR",
      "M_TEMP.C",
      "M_ART.DBP",
      "M_NBP.D",
      "M_ART.SBP",
      "M_NBP.S",
      "M_ART.MBP",
      "M_NBP.M",
      "M_CVP",
      "M_SPO2"
    )
  slopeList <- list()
  i <- 1
  for (name in names) {
    ySamp <- select(ModellingDataFrame, starts_with(name))
    ls <-
      as.data.frame(t(apply(
        ySamp, 1, GetSlopePerColType, name, nSlice
      )))
    #ModellingDataFrame<-cbind(ModellingDataFrame,ls)
    slopeList[[i]] <- ls
    i <- i + 1
  }
  y <- do.call("cbind", slopeList)
  return(y)
  #return(ModellingDataFrame)
}


GetSlopePerColType <-
  function(ySamp, name,
           nSlice = 12) {
    x <- (1:nSlice)
    nm <- (1:(nSlice - 1))
    slopes <- diff(ySamp) / diff(x)
    n1 <- paste("Slp", name, nm, sep = "_")
    n2 <- paste("Slp_Fin", name, sep = "_")
    s2 <- setNames(slopes, n1)
    # s2$slp<-lm(ySamp ~ x)$coeff[[2]]
    # names(s2)[nSlice]<-n2
    return(s2)
  }


FillNAValuesBasedonNormAge <- function(dfAgeTemp, OrigDf) {
  s1 <- CreateNormalsDFFromAge(dfAgeTemp)
  
  OrigDf$HR[is.na(OrigDf$HR)] <- s1$HR
  OrigDf$RR[is.na(OrigDf$RR)] <- s1$RR
  OrigDf$TEMP.C[is.na(OrigDf$TEMP.C)] <- s1$TEMP.C
  OrigDf$ART.DBP[is.na(OrigDf$ART.DBP)] <- s1$ART.DBP
  OrigDf$NBP.D[is.na(OrigDf$NBP.D)] <- s1$NBP.D
  OrigDf$ART.SBP[is.na(OrigDf$ART.SBP)] <- s1$ART.SBP
  OrigDf$NBP.S[is.na(OrigDf$NBP.S)] <- s1$NBP.S
  OrigDf$ART.MBP[is.na(OrigDf$ART.MBP)] <- s1$ART.MBP
  OrigDf$NBP.M[is.na(OrigDf$NBP.M)] <- s1$NBP.M
  OrigDf$CVP[is.na(OrigDf$CVP)] <- s1$CVP
  OrigDf$SPO2[is.na(OrigDf$SPO2)] <- s1$SPO2
  
  return(OrigDf)
}
GenerateConseqHourlyDistribution <-
  function(sampl1,
           nSlice = 12,
           forwhom = "NCB") {
    #Create single row per patient
    #
    # rowid
    #
    # Returns:
    # Error handling
    ConsecWithin12HrsRange <- list()
    nRowsInHr <- list()
    ReadingInaHr <- list()
    MeanInaHr <- list()
    nCnt <- 0
    lsNormVals <- list()
    lsnRowsInHr <- list()
    
    PatientID <- unique(sampl1$Patient.ID)
    
    if (forwhom == "NCB")
    {
      Age <- max(sampl1$Age, na.rm = TRUE)
    }
    
    if (forwhom == "CB")
    {
      Age <- GetCBAgeFromPatientID(PatientID)
    }
    
    
    for (i in 1:nSlice) {
      if (i == 1) {
        intervallistvar <-
          as.interval(max(sampl1$Obs_Time.Date) - i * 3600,
                      max(sampl1$Obs_Time.Date))
      } else{
        intervallistvar <-
          as.interval(max(sampl1$Obs_Time.Date) - i * 3600,
                      max(sampl1$Obs_Time.Date) - (i - 1) * 3600)
      }
      sampl2 <-
        filter(sampl1, Obs_Time.Date %within% intervallistvar)
      nR <- nrow(sampl2)
      nRowsInHr <- c(nRowsInHr, nR)
      
      if (nR > 0)
      {
        ConsecWithin12HrsRange <- c(ConsecWithin12HrsRange, 1)
        ReadingInaHr <-
          c(ReadingInaHr, GetLenEachReadingInaHr(sampl2, i))
        dfAgeTemp <- as.data.frame(Age)
        sampl4 <- FillNAValuesBasedonNormAge(dfAgeTemp, sampl2)
        MeanInaHr <-
          c(MeanInaHr, GetMeanofEachReadingInaHr(sampl4, i))
      }
      else
      {
        ConsecWithin12HrsRange <- c(ConsecWithin12HrsRange, 0)
        nCnt <- nCnt + 1
        dfAgeTemp <- as.data.frame(Age)
        sampl2 <- CreateNormalsDFFromAge(dfAgeTemp)
        ReadingInaHr <-
          c(ReadingInaHr, GetLenEachReadingInaHr(sampl2, i))
        MeanInaHr <-
          c(MeanInaHr, GetMeanofEachReadingInaHr(sampl2, i))
      }
      
    }# endof for loop.
    
    # fill other values
    namesH <- paste("H", 1:nSlice, sep = "")
    lsNormVals <- setNames(ConsecWithin12HrsRange, namesH)
    m2 <- as.data.frame(lsNormVals)
    m2$HrlySeq <-
      paste(as.character(c(unlist(
        ConsecWithin12HrsRange
      ))), collapse = ",")
    
    namesRc <- paste("Rc", 1:nSlice, sep = "")
    lsnRowsInHr <- setNames(nRowsInHr, namesRc)
    m2 <- cbind(m2, as.data.frame(lsnRowsInHr))
    
    dfReadingInaHr <- as.data.frame(do.call("cbind", ReadingInaHr))
    m2 <- cbind(m2, dfReadingInaHr)
    
    dfMeanInaHr <- as.data.frame(do.call("cbind", MeanInaHr))
    m2 <- cbind(m2, dfMeanInaHr)
    
    m2$Patient.ID <- PatientID
    m2$CntNonSeq <- nCnt
    m2$CntSeq <- nSlice - nCnt
    m2$GapBtwLastOb_1stObs <-
      as.numeric(as.duration(max(sampl1$Obs_Time.Date) - min(sampl1$Obs_Time.Date))) /
      3600
    
    
    
    m2$Age <- Age
    
    #"2015-10-23 12:42:58 UTC"  Latest observation time
    #"2015-10-23 12:50:00 UTC" CB time
    #"422s (~7.03 minutes)" what is the time gap between latest Observation and the CB
    # Be careful to flag off negative values since mean that the last observation is taken after CB.
    # m2$GapBtwLastOb_Cb <-
    #   as.duration(max(sampl1$Obs_Time.Date) - unique(sampl1$CB_Time.Date))
    
    return(m2)
  }

GenerateConseqHourlyDistribution_Wrapper <-
  function(df1, nSlice, forwhom) {
    a1 <- list()
    splitdf <- split(df1, f = df1$Patient.ID)
    a1 <-
      lapply(splitdf, GenerateConseqHourlyDistribution, nSlice, forwhom)
    a1df <- do.call("rbind", a1)
    
    
    strPath1 <- paste0(forwhom,
                       "_",
                       nSlice,
                       "_",
                       Append.Date.Time("ConsecHourlyDistribution"),
                       ".csv")
    strPath2 <- paste0(forwhom,
                       "_",
                       nSlice,
                       "_",
                       Append.Date.Time("CHD"),
                       ".Rda")
    
    write.csv(a1df, strPath1)
    save(a1df, file = strPath2)
    return(a1df)
    
  }


GenerateConseqHourlyDistribution_Wrapper1 <-
  function(df1, nSlice, forwhom) {
    chdfList <- list()
    featList <- list()
    slopeList <- list()
    n <- nrow(df)
    
    splitdf <- split(df1, f = df1$Patient.ID)
    
    chdfList <-
      lapply(splitdf, GenerateConseqHourlyDistribution, nSlice, forwhom)
    chdf <- do.call("rbind", chdfList)
    
    splitdf <- split(chdf, f = df1$Patient.ID)
    featList <- lapply(splitdf, GenFeatures, nSlice)
    featdf <- do.call("rbind", featList)
    
    splitdf <- split(featdf, f = df1$Patient.ID)
    slopeList <- lapply(splitdf, GenSlopes, nSlice)
    slopedf <- do.call("rbind", slopeList)
    
    
    strPath1 <- paste0(forwhom,
                       "_",
                       nSlice,
                       "_",
                       Append.Date.Time("ConsecHourlyDistribution"),
                       ".csv")
    strPath2 <- paste0(forwhom,
                       "_",
                       nSlice,
                       "_",
                       Append.Date.Time("CHD"),
                       ".Rda")
    write.csv(a1df, strPath1)
    save(a1df, file = strPath2)
    return(a1df)
  }

GetLenEachReadingInaHr <- function(df, i) {
  names <-
    c(
      "HR",
      "RR",
      "TEMP.C",
      "ART.DBP",
      "NBP.D",
      "ART.SBP",
      "NBP.S",
      "ART.MBP",
      "NBP.M",
      "CVP",
      "SPO2"
    )
  s1 <- df[, names]
  n1 <- paste("L", names, i, sep = "_")
  s2 <-
    as.data.frame(t(apply(s1, 2, function(x) {
      length(which(!is.na(x)))
    })))
  s2 <- setNames(s2, n1)
  #s2<-t(apply(s1,2,function(x){length(which(!is.na(x)))}))
  #s2<-lapply(s1,function(x){length(which(!is.na(x)))})
  return(s2)
}

GetMeanofEachReadingInaHr <- function(df, i) {
  names <-
    c(
      "HR",
      "RR",
      "TEMP.C",
      "ART.DBP",
      "NBP.D",
      "ART.SBP",
      "NBP.S",
      "ART.MBP",
      "NBP.M",
      "CVP",
      "SPO2"
    )
  s1 <- df[, names]
  n1 <- paste("M", names, i, sep = "_")
  s2 <-
    as.data.frame(t(apply(s1, 2, function(x) {
      mean(x, na.rm = TRUE)
    })))
  s2 <- setNames(s2, n1)
  return(s2)
}


GetARowPerPatWtMeans <-  function(rowid, sampl1, nSlice = 12) {
  #Create single row per patient
  #
  # rowid
  #
  # Returns:
  # Error handling
  
  
  # Now lets take a sample in which we will create hour boundaries and allocate all observations into these boundaries
  # sampling 1992193 2015 7 29 16 917
  #[1] "2015-07-29 08:06:00 UTC" "2015-07-29 23:22:00 UTC"
  #  CB time 2015-07-30 02:30:00
  # July 30th at 2 30 cb was sounded. There are readings available from July 29 8:06 to 29th 23:22
  # Create cols HR@CB.t-1, HR@CB.t-2, HR@CB.t-12
  # ist variable is the CB itself
  # 2nd variable is the CB-1 time cb-1<-CB-3600
  # interval variable
  
  All.Obs.in.int.list <- list()
  for (i in 1:nSlice) {
    if (i == 1) {
      intervallistvar <-
        as.interval(max(sampl1$Obs_Time.Date) - i * 3600,
                    max(sampl1$Obs_Time.Date))
    } else{
      intervallistvar <-
        as.interval(max(sampl1$Obs_Time.Date) - i * 3600,
                    max(sampl1$Obs_Time.Date) - (i - 1) * 3600)
    }
    sampl2 <- filter(sampl1, Obs_Time.Date %within% intervallistvar)
    sampl3 <- select(sampl2,
                     HR,
                     RR,
                     TEMP.C,
                     ART.DBP,
                     NBP.D,
                     ART.SBP,
                     NBP.S,
                     ART.MBP,
                     NBP.M,
                     CVP,
                     SPO2)
    All.Obs.in.int.list <-
      c(All.Obs.in.int.list, lapply(sampl3, function(x)
        mean(x, na.rm = TRUE)))
  }
  m2 <- as.data.frame(All.Obs.in.int.list)
  m2$Patient.ID <- rowid
  
  #"2015-10-23 12:42:58 UTC"  Latest observation time
  #"2015-10-23 12:50:00 UTC" CB time
  #"422s (~7.03 minutes)" what is the time gap between latest Observation and the CB
  # Be careful to flag off negative values since mean that the last observation is taken after CB.
  # m2$GapBtwLastOb_Cb <-
  #   as.duration(max(sampl1$Obs_Time.Date) - unique(sampl1$CB_Time.Date))
  m2$GapBtwLastOb_1stObs <-
    as.numeric(as.duration(max(sampl1$Obs_Time.Date) - min(sampl1$Obs_Time.Date))) /
    3600
  return(m2)
}
GetARowPerPatWtCounts <-  function(rowid, sampl1, nSlice = 12) {
  #Create single row per patient
  #
  # rowid
  #
  # Returns:
  # Error handling
  
  
  # Now lets take a sample in which we will create hour boundaries and allocate all observations into these boundaries
  # sampling 1992193 2015 7 29 16 917
  #[1] "2015-07-29 08:06:00 UTC" "2015-07-29 23:22:00 UTC"
  #  CB time 2015-07-30 02:30:00
  # July 30th at 2 30 cb was sounded. There are readings available from July 29 8:06 to 29th 23:22
  # Create cols HR@CB.t-1, HR@CB.t-2, HR@CB.t-12
  # ist variable is the CB itself
  # 2nd variable is the CB-1 time cb-1<-CB-3600
  # interval variable
  
  All.Obs.in.int.list <- list()
  for (i in 1:nSlice) {
    if (i == 1) {
      intervallistvar <-
        as.interval(max(sampl1$Obs_Time.Date) - i * 3600,
                    max(sampl1$Obs_Time.Date))
    } else{
      intervallistvar <-
        as.interval(max(sampl1$Obs_Time.Date) - i * 3600,
                    max(sampl1$Obs_Time.Date) - (i - 1) * 3600)
    }
    sampl2 <- filter(sampl1, Obs_Time.Date %within% intervallistvar)
    sampl3 <- select(sampl2,
                     HR,
                     RR,
                     TEMP.C,
                     ART.DBP,
                     NBP.D,
                     ART.SBP,
                     NBP.S,
                     ART.MBP,
                     NBP.M,
                     CVP,
                     SPO2)
    All.Obs.in.int.list <-
      c(All.Obs.in.int.list, lapply(sampl3, function(x)
        length(which(x > 0))))
  }
  m2 <- as.data.frame(All.Obs.in.int.list)
  m2$Patient.ID <- rowid
  #"2015-10-23 12:42:58 UTC"  Latest observation time
  #"2015-10-23 12:50:00 UTC" CB time
  #"422s (~7.03 minutes)" what is the time gap between latest Observation and the CB
  # Be careful to flag off negative values since mean that the last observation is taken after CB.
  
  # m2$GapBtwLastOb_Cb <-
  #   as.duration(max(sampl1$Obs_Time.Date) - unique(sampl1$CB_Time.Date))
  m2$GapBtwLastOb_1stObs <-
    as.numeric(as.duration(max(sampl1$Obs_Time.Date) - min(sampl1$Obs_Time.Date))) /
    3600
  return(m2)
}
ExploreDataLowLevel <- function(RawDataFrame) {
  # Pass a df and you get various ways of understanding it from indivisual col perspective
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   Nothing
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   #stop(" Arguments dataframe  has missing values.")
  # }
  s2 <- numSummary(RawDataFrame)
  
  print(s2)
  write.csv(s2, paste0(Append.Date.Time("LowLevelSummary"), ".csv"))
  print(charSummary(RawDataFrame))
  
  # Understand pereach patient no of readingsofeach type
  
  Rawdata.grp.p <- group_by(RawDataFrame, Patient.ID)
  
  # The following are the number of total unique patients who have non Zero observations.
  # For patient 2379251	 we have 125 HR and 	116 RR	5	11	11	11	23	23	23 readings that are not zero
  
  summ <-
    summarise(
      Rawdata.grp.p,
      hr = length(which(HR >= 0)),
      rr = length(which(RR >= 0)),
      tmp = length(which(TEMP.C >= 0)),
      ns = length(which(NBP.S >= 0)),
      nM = length(which(NBP.M >= 0)),
      nd = length(which(NBP.D >= 0)),
      ad = length(which(ART.DBP >= 0)),
      as = length(which(ART.SBP >= 0)),
      am = length(which(ART.MBP >= 0)),
      ob = length(Obs_Time.Date)
    )
  # scatterplot(summ$Patient.ID, summ$hr)
  # hist(summ$hr)
  # plot(summ$hr ~ summ$Patient.ID , type = "p")
  
  # The following are the number of total unique patients who have at least 10 observations.
  
  Per.Patient.gr.10 <-
    summarise(
      Rawdata.grp.p,
      hr = length(which(HR >= 10)),
      rr = length(which(RR >= 10)),
      tmp = length(which(TEMP.C >= 10)),
      ns = length(which(NBP.S >= 10)),
      nM = length(which(NBP.M >= 10)),
      nd = length(which(NBP.D >= 10)),
      ad = length(which(ART.DBP >= 10)),
      as = length(which(ART.SBP >= 10)),
      am = length(which(ART.MBP >= 10))
    )
  
  # the above is per patient below is sum total . in 312 patients how many are obs... kind of question
  #in total how many number of readings are collected more than 10 times. If they are collected less than 10 times, for a # single patient how are we going to use them in TIME series? we have to divide these 10 observations in to hour of collection.
  
  df.10 <-
    data.frame(
      hr = length(which(summ$hr >= 10)),
      rr = length(which(summ$rr >= 10)),
      tmp = length(which(summ$tmp >= 10)),
      ns = length(which(summ$ns >= 10)),
      nM = length(which(summ$nM >= 10)),
      nd = length(which(summ$nd >= 10)),
      ad = length(which(summ$ad >= 10)),
      as = length(which(summ$as >= 10)),
      am = length(which(summ$am >= 10)),
      ob = length(which(summ$ob >= 10))
    )
  
  #   hr rr tmp  ns  nM  nd ad as am
  #  137 87  34 106 106 106 27 27 28
  # this means that there are a total of 137 HR observations which are collected  more than 10 times
  
  which(summ$nd > 10)
  summ[which(summ$as > 10), ]
  nParients <- length(unique(RawDataFrame$Patient.ID))
  # what is the percent of readings that exceed 10
  df.perc <- as.data.frame(lapply(df.10, function(x)
    percent(x / nParients)))
  dffin <- rbind(df.10, df.perc)
  dffin <-
    rbind(df.10, df.perc, as.data.frame(lapply(df.10, function(x)
      percent(1 - x / nParients))))
  
  
  write.csv(dffin, paste0(Append.Date.Time("PercentageObservations"), ".csv"))
  #    hr    rr   tmp  ns  nM  nd    ad    as    am
  # 43.9% 27.9% 10.9% 34% 34% 34% 8.65% 8.65% 8.97%
  
  # what is the percent of readings that are below 10 readings
  # df.perc <- as.data.frame(lapply(df.10, function(x)
  #   percent(1 - x / nParients)))
  #
  #     hr    rr   tmp  ns  nM  nd    ad    as  am
  #   56.1% 72.1% 89.1% 66% 66% 66% 91.3% 91.3% 91%
  
  
  # here we get the number of patients who have more than 10 readings in each of their observations
  df.com <-
    data.frame(len = length(which(summ$ad >= 10) &
                              length(which(summ$as >= 10))))
  
  df.com <-
    data.frame(len = length(which(summ$hr >= 10) &
                              length(which(summ$rr >= 10))) &
                 length(which(summ$tmp >= 10))) &
    length(which(summ$ns >= 10)) &
    length(which(summ$nM >= 10)) &
    length(which(summ$nd >= 10)) &
    length(which(summ$ad >= 10)) &
    length(which(summ$as >= 10)) & length(which(summ$am >= 10))
  
  #  Now that we know that eg patient 2379251	 has 125 non Zero observations. How do we find out the frequency of these non zero observations
  # can we know if these 125 observations are spanning across 2 days or 3 hours. Ideally we want them to span across t(e) minus 12 hrs.
  # Lets see the distribution here
  
  Rawdata.grp.dt <-
    group_by(
      RawDataFrame,
      Patient.ID,
      Obs_Time.Yr,
      Obs_Time.Mo,
      Obs_Time.Dy,
      Obs_Time.Hr,
      Obs_Time.Mi,
      Obs_Time.Sc
    )
  
  
  #Arrange  reorders . It takes a data frame, and a set of column names (or more complicated expressions) to order by.
  #here it arranges patiet x data based on time. 2015-09-02 16:07:0 2015-09-02 16:08:0 2015-09-02 16:09:0 2015-09-02 16:10:0
  Rawdata.grp.dt.Arr <-
    arrange(
      Rawdata.grp.dt,
      Patient.ID,
      Obs_Time.Yr,
      Obs_Time.Mo,
      Obs_Time.Dy,
      Obs_Time.Hr,
      Obs_Time.Mi,
      Obs_Time.Sc
    )
  
  #This is an excellent rollup of observations from seconds to years. This will help us find the spans
  
  persec <- summarise(Rawdata.grp.dt.Arr, secob = n())
  permin <- summarise(persec, minob = n())
  perhr <- summarise(permin, hrob = n())
  perday <- summarise(perhr, dailyob = n())
  permon <- summarise(perday, monob = n())
  peryear <- summarise(permon, yrob = n())
  
  
  peryear.range <- summarise(peryear, numYRS.span = length(yrob))
  # outlier 2355245 has readings in 2000 and 2015
  
  permon.range <- summarise(permon, numMon.span = length(monob))
  #2407714    2425364 2436991 2442751 2444525 have readings in 2 months (Intrestingly, they are from month boundaries. So simple #arithmatic of month substraction wont work. Over to Lubridate for getting this right)
  
  perday.range <- summarise(perday, numdays.span = length(dailyob))
  #2389336 obsvns has a span of 6 days , rest of all patients either have 2 days or 1. So it implies that we have sufficient #data if we look at day span ONLY. Lets see if this hypothesis is correct or wrong based on the next perhr.range observations
  
  perhour.range <-
    summarise(perhr,
              numHrs.span = length(hrob),
              SumofObs = sum(hrob))
  # 9076288 and 907712 have a hour.range of 20 and 19 . It means that their samples are taken 20/19 times in a particular day. We generally need a 12 hr min range to chk if the patient is eligible for modelling-Importantly, decimal hrs in perhr.range are not the right way to understand if 12 obsvns happened in a single day or not since (decimal) it doesnt take care of day boundaries. 6 observations in day1 midnight and 6 more in d2 post midnight is a good candidate, but the way we have aggregated teh data, it doesntgive this clarity. hence lets redo this with Lubridate to see if we can accurately surpass this limitations.
  
  # We had an hypothesis  in the perday.range section above. This hypothesis is proven wron by examining the SumofObs in perday.range. SumofObs when it is minimal (below 10), it doesnt make sense to use this patient for modeling even though he has got 1 day worth of observations. Hence hypothesis false.
  persec.range <- summarise(persec, numSec.span = length(secob))
  
  # Now lets take a sample in which we will create hour boundaries and allocate all observations into these boundaries
  # sampling 1992193 2015 7 29 16 917
  # sampl1<-filter(Rawdata.grp.dt.Arr, Patient.ID == 1992193 )
  #
  # range(sampl1$Obs_Time.Date)
  # #[1] "2015-07-29 08:06:00 UTC" "2015-07-29 23:22:00 UTC"
  # #  CB time 2015-07-30 02:30:00
  # # July 30th at 2 30 cb was sounded. There are readings available from July 29 8:06 to 29th 23:22
  # # Create cols HR@CB.t-1, HR@CB.t-2, HR@CB.t-12
  # # ist variable is the CB itself
  # # 2nd variable is the CB-1 time cb-1<-CB-3600
  # # interval variable
  # intcb.1<- as.interval(max(sampl1$Obs_Time.Date)-1*3600, max(sampl1$Obs_Time.Date))
  # intcb.2<- as.interval(max(sampl1$Obs_Time.Date)-2*3600, max(sampl1$Obs_Time.Date)-1*3600)
  # intcb.3<- as.interval(max(sampl1$Obs_Time.Date)-3*3600, max(sampl1$Obs_Time.Date)-2*3600)
  # intcb.4<- as.interval(max(sampl1$Obs_Time.Date)-4*3600, max(sampl1$Obs_Time.Date)-3*3600)
  # intcb.5<- as.interval(max(sampl1$Obs_Time.Date)-5*3600, max(sampl1$Obs_Time.Date)-4*3600)
  # intcb.6<- as.interval(max(sampl1$Obs_Time.Date)-6*3600, max(sampl1$Obs_Time.Date)-5*3600)
  # intcb.7<- as.interval(max(sampl1$Obs_Time.Date)-7*3600, max(sampl1$Obs_Time.Date)-6*3600)
  # intcb.8<- as.interval(max(sampl1$Obs_Time.Date)-8*3600, max(sampl1$Obs_Time.Date)-7*3600)
  # intcb.9<- as.interval(max(sampl1$Obs_Time.Date)-9*3600, max(sampl1$Obs_Time.Date)-8*3600)
  # intcb.10<- as.interval(max(sampl1$Obs_Time.Date)-10*3600, max(sampl1$Obs_Time.Date)-9*3600)
  # intcb.11<- as.interval(max(sampl1$Obs_Time.Date)-11*3600, max(sampl1$Obs_Time.Date)-10*3600)
  # intcb.12<- as.interval(max(sampl1$Obs_Time.Date)-12*3600, max(sampl1$Obs_Time.Date)-11*3600)
  # intcb.12<- as.interval(max(sampl1$Obs_Time.Date)-13*3600, max(sampl1$Obs_Time.Date))
  #
  #
  # Obs.within.int1<-filter(sampl1, Obs_Time.Date %within% intcb.1)
  # Obs.within.int2<-filter(sampl1, Obs_Time.Date %within% intcb.2)
  # Obs.within.int3<-filter(sampl1, Obs_Time.Date %within% intcb.3)
  # Obs.within.int4<-filter(sampl1, Obs_Time.Date %within% intcb.4)
  # Obs.within.int5<-filter(sampl1, Obs_Time.Date %within% intcb.5)
  # Obs.within.int6<-filter(sampl1, Obs_Time.Date %within% intcb.6)
  # Obs.within.int7<-filter(sampl1, Obs_Time.Date %within% intcb.7)
  # Obs.within.int8<-filter(sampl1, Obs_Time.Date %within% intcb.8)
  # Obs.within.int9<-filter(sampl1, Obs_Time.Date %within% intcb.9)
  # Obs.within.int10<-filter(sampl1, Obs_Time.Date %within% intcb.10)
  # Obs.within.int11<-filter(sampl1, Obs_Time.Date %within% intcb.11)
  # Obs.within.int12<-filter(sampl1, Obs_Time.Date %within% intcb.12)
  #
  # which(sampl1$Obs_Time.Date%within%intcb.1)
  # which(sampl1$Obs_Time.Date%within%intcb.3)
  
  # find if the observation time Obs_Time is  %within% intcb-1
  #sampl1$HR@CB.t-1<-Get all the HR readings in the range 1:30 to 2:30 (CB -1 hr)
  
  
  
}

PlotData <- function(RawDataFrame) {
  # Visualizes the data, finds corelations, nulls, nas, and other interesting facts about the data.
  #
  # Args:
  #   RawDataFrame: DF which has to be Visualized
  #
  # Returns:
  #   Nothing
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   #stop(" Arguments dataframe  has missing values.")
  # }
  
  par(mfrow = c(2, 2))
  
  newdata <-
    RawDataFrame[which(RawDataFrame$Patient.ID == 2379251),]
  
  hist(newdata$Obs_Time.Date, breaks = "hours", freq = TRUE)
  
  plot.ts(newdata$Obs_Time.Date)
  plot(newdata$HR ~ newdata$Obs_Time.Date, type = "p")
  plot(newdata$RR ~ newdata$Obs_Time.Date, type = "p")
  plot(newdata$TEMP.C ~ newdata$Obs_Time.Date, type = "p")
  plot(newdata$NBP.D ~ newdata$Obs_Time.Date, type = "p")
  
  
  
  # ggplot(newdata, aes(x = newdata$Obs_Time.Date, y = newdata$CB_Time.Date)) +
  #   geom_point() +
  #   scale_y_datetime(breaks=date_breaks("hour"), labels=date_format("%H:%M")) +
  #   theme(axis.text.x=element_text(angle=90))
  #
  # ggplot(newdata, aes(x = newdata$Obs_Time.Date, y = newdata$HR)) + geom_line() +
  #   scale_x_date(format = "%h-%m") + xlab("") + ylab("HR")
  
  # mice_plot <- aggr(d1, col=c('pink','yellow'),
  #                   numbers=TRUE, sortVars=TRUE,
  #                   labels=names(d1), cex.axis=.7,
  #                   gap=3, ylab=c("Missing data","Pattern"))
  #
  # # for only TEMP.C
  # mice_plot <- aggr(select(d1,starts_with("TEMP.C")), col=c('pink','green'),
  #                   numbers=TRUE, sortVars=TRUE,
  #                   labels=names(select(d1,starts_with("TEMP.C"))), cex.axis=.7,
  #                   gap=3, ylab=c("Missing data","Pattern"))
  
  
  par(mfrow = c(1, 1))
  
}

TransformDateTimeData <- function(RawDataFrame) {
  # To handle the Transfrormation of  numerical data intoPOSIXlt date time classes.
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  #(TODO) Original 1271, final 1220. Because NCB data has 51 patients rows that were removed since they were invalid. Need the data in corect form, Already mailed to get this corrected.
  # CB data Origina 363 final 317. Because NCB data has 46 patients rows that were removed since they were invalid. Need the data in corect form, Already mailed to get this corrected.
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   #stop(" Arguments dataframe  has missing values.")
  # }
  
  #(TODO-Important) this is encountered in non cb data. Where some of the obs
  #dates are are character "null" very interesting to find the source of this problem
  if (length(which(RawDataFrame$Obs.Time == "null")) >= 1) {
    row_to_keep1 = which(RawDataFrame$Obs.Time == "null")
    RawDataFrame = RawDataFrame[-row_to_keep1, ]
    
    # str<-paste0("NullDates",n,sep="_")
    # write.csv( RawDataFrame[row_to_keep1,],paste0(Append.Date.Time(str),".csv"))
  }
  
  RawDataFrame$Obs_Time.Date <- ymd_hms(RawDataFrame$Obs.Time)
  # RawDataFrame$CB_Time.Date <-
  #   dmy_hm(as.character(RawDataFrame$CB_Time))
  RawDataFrame$Obs_Time.Yr <- year(RawDataFrame$Obs_Time.Date)
  RawDataFrame$Obs_Time.Mo <- month(RawDataFrame$Obs_Time.Date)
  RawDataFrame$Obs_Time.Dy <- day(RawDataFrame$Obs_Time.Date)
  RawDataFrame$Obs_Time.Hr <- hour(RawDataFrame$Obs_Time.Date)
  RawDataFrame$Obs_Time.Mi <- minute(RawDataFrame$Obs_Time.Date)
  RawDataFrame$Obs_Time.Sc <- second(RawDataFrame$Obs_Time.Date)
  
  
  #(TODO-Important) this is encountered in non cb data. Where some of the obs
  #dates are invaid for eg in set10 2349488.  hence removing the obsvns that
  #correspond to these dates.
  Invaliddates <- RawDataFrame[is.na(RawDataFrame$Obs_Time.Yr),]
  str<-paste0("InvalidDates",n,sep="_")
  write.csv( Invaliddates,paste0(Append.Date.Time(str),".csv"))
  
  RawDataFrame <- RawDataFrame[!is.na(RawDataFrame$Obs_Time.Yr),]
  
  return(RawDataFrame)
  
}

TransformAllCategData <- function(RawDataFrame) {
  # To handle the encoding of all categorical data variables
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   #stop(" Arguments dataframe  has missing values.")
  # }
  
  # (IMPORTANT). Nulls in factors are recoded with a number. Because they are
  # seen as one more level. So we need to ignore nulls. **** Solution for that
  # problem In particular, as.numeric applied to a factor is meaningless, and
  # may happen by implicit coercion. To transform a factor f to approximately
  # its original numeric values, as.numeric(levels(f))[f] is recommended and
  # slightly more efficient than  as.numeric(as.character(f)).
  
  
  
  RawDataFrame$RR <- as.numeric(RawDataFrame$RR)
  return(RawDataFrame)
}

TransformAllNumericData <- function(RawDataFrame) {
  # To handle the encoding of all Numerical data variables
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   #stop(" Arguments dataframe  has missing values.")
  # }
  RawDataFrame$Age <- as.numeric(RawDataFrame$Age)
  RawDataFrame$Age[RawDataFrame$Age == 0] <-  1
  RawDataFrame$Age[RawDataFrame$Age == 115 |
                     RawDataFrame$Age == 116] <- 58
  
  
  return(RawDataFrame)
}


ConvertCatFactorToNumericData <- function(RawDataFrame) {
  # To handle the encoding of all Numerical data variables
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   #stop(" Arguments dataframe  has missing values.")
  # }
  
  
  return(RawDataFrame)
}

RemoveUnwantedColumns <- function(RawDataFrame) {
  # To remove all unwanted columns for our final data on which the model runs
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  RawDataFrame <- dplyr::select(RawDataFrame, -starts_with("L_"))
  RawDataFrame <- dplyr::select(RawDataFrame, -HrlySeq)
  RawDataFrame <- dplyr::select(RawDataFrame, -starts_with("Rc"))
  RawDataFrame <- dplyr::select(RawDataFrame, -CntNonSeq)
  RawDataFrame <- dplyr::select(RawDataFrame, -CntSeq)
  RawDataFrame <- dplyr::select(RawDataFrame, -GapBtwLastOb_1stObs)
  
  return(RawDataFrame)
}

RemoveUnwantedRows <- function(RawDataFrame) {
  # To remove all unwanted rows for our final data on which the model runs
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   #stop(" Arguments dataframe  has missing values.")
  # }
  # waiting for further analysis to understand if we should remove any of the rows.
  return(RawDataFrame)
}

HandleNaNullNan <- function(RawDataFrame) {
  # To remove all unwanted cells and replace for our final data on which the model runs
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  # if (TRUE %in% is.na(RawDataFrame)) {
  #   # stop(" Arguments dataframe  has missing values.")
  # }
  #Looking at the data this is the fn where we ALL are going to spend a lot of time
  # Lets assume that all NAs have been addressed. We will use complete cases for the timebwing
  # dim(RawDataFrame)
  # RawDataFrame = RawDataFrame[complete.cases(RawDataFrame), ]
  
  return(RawDataFrame)
}


Append.Date.Time <-  function(str,
                              sep = '_',
                              date.format = "%Y_%m_%d_%H_%M_%S") {
  # Append date time to a string so that we can use the file name
  #
  # Args: k num of clusters and i Labels
  #
  # Returns: Nothing
  # Error handling
  stopifnot(is.character(str))
  return(paste(str, format(Sys.time(), date.format), sep = sep))
}



GetTopCorrelCols <- function(RawDataFrame, numtoreport) {
  # To Print number of variables numtoreport which are highly correlated
  #
  # Args: k num of clusters and i Labels
  #
  # Returns: Nothing
  # Error handling
  n <- nrow(RawDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(RawDataFrame),
         ".")
  }
  
  # find the correlations
  cormatrix <- cor(RawDataFrame)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable", "Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation), decreasing = T), ], n = numtoreport)
}




ValidateClusters <- function() {
  # This is similar to profiling the cluster, but we will perform individual validattion here
  # To technically profile the clusters and see if they are good enough
  # To profile the clusters from commonsense perspective to see if they really make sense
  # Technically speaking:
  # 1.  Lookout num of cluster and size for very big clusters or very small clusters. Analyze why they are so.
  # 2. Look for measuring cohesion of clusters. Within cluster distances
  #   2.1 SD of each clustering field in a cluster i. This should be small.
  #   2.1.1 Pooled SD is  the weighted avg (acc to clus sz) of individual SDs for all clusters
  #
  # Returns:
  #
  
  
}
DoClustering <- function() {
  # Perform clustering to undertand hidden patterns in the data
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  
  
  
}



GetFeatsSelCorrel <- function(ModellingDataFrame) {
  # Get Correraltions for feat sel
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  
  set.seed(143)
  # load the library
  
  # load the data
  flog.info("Inside GetFeatsSelCorrel", name = 'logger.b')
  dfWithoutH <- dplyr::select(ModellingDataFrame,-(starts_with("H")))
  dfWithoutH <- dplyr::select(dfWithoutH,-Patient.ID)
  dfWithoutH <- dplyr::select(dfWithoutH,-IsCb)
  
  
  # calculate correlation matrix
  correlationMatrix <- cor(dfWithoutH,use ="complete.obs")
  
  # remove rows that have any nas
  row.has.na <- apply(correlationMatrix, 1, function(x){any(is.na(x))})
  sum(row.has.na)
  correlationMatrix <- correlationMatrix[!row.has.na,]
  
  # summarize the correlation matrix
  print(correlationMatrix)
  # find attributes that are highly corrected (ideally >0.75)
  
  #highlyCorrelated <- findCorrelation(correlationMatrix1, cutoff=0.9)
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.9)
  # print indexes of highly correlated attributes
  #print(highlyCorrelated)
  
  diag(correlationMatrix) <- 0
  correlationMatrix[lower.tri(correlationMatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(correlationMatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable", "Correlation")
  # sort and print the top n correlations
  numtoreport <- 350
  head(fm[order(abs(fm$Correlation), decreasing = T), ], n = numtoreport)
  
  dfx1 <- dfWithoutH[,highlyCorrelated]
  #these are the cols that areidentified as more than 90% correlated
  # removeallthe cols which are more than 90% correleated. If you removemore you loose more info
  dfx <- dfWithoutH[,-highlyCorrelated]
  
  flog.info("The following feats are more than 90% ccorrelated ", dim(dfx1),name = 'logger.b',capture = TRUE)
  flog.info("Data Frame after removing 90% coorelated cols: ", names(dfx),name = 'logger.b',capture = TRUE)
  
  return(dfx)
  
  
}

GetScaledMat <- function(ModellingDataFrame) {
  # Scaling performs mean/sd calculation to make sd 1 and mean 0 of every variable.
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  n <- length(ModellingDataFrame)
  # Error handling
  if (n <= 1) {
    stop("The dataframe doesnt have any values lengths: ",
         length(ModellingDataFrame),
         ".")
  }
  
  
  # Actual clustering data set has to be sent to the scaling and dist functions.
  Scaled.matrix <- scale(select(ModellingDataFrame,-c(IsCb,Patient.ID)))
  pcenter <- attr(Scaled.matrix, "scaled:center")
  pscale <- attr(Scaled.matrix, "scaled:scale")
  dim(Scaled.matrix)
  return(Scaled.matrix)
  
}

DoPCA <- function(ModellingDataFrame) {
  # Perform Principal component analysis to understand Eigen vecs
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The transformed original DataFrame object
  Scaled.matrix <- GetScaledMat(ModellingDataFrame)
  dim(Scaled.matrix)
  prin.comp <- prcomp(select(ModellingDataFrame,-c(IsCb,Patient.ID)))
  summary(prin.comp) # print variance accounted for
  loadings(prin.comp) # pc loadings
  screeplot(prin.comp, type = "lines")
  screeplot(prin.comp, type = "barplot")
  sum((prin.comp$sdev) ^ 2)
  prin.comp$sdev
  
  # We can try to visualize the clustering by projecting
  # the data onto the first two principal components of the data.2 If N is the number of
  # variables that describe the data, then the principal components describe the hyperellipsoid
  # in N-space that bounds the data.
  
  # The predict() function will rotate the data into the space described by the principal components. We only want the projection on
  # the first two axes.
  
  # nComp <- 2
  # project <- predict(prin.comp, newdata=Scaled.matrix)[,1:nComp]
  #
  # # (Important )This rawdataframe here expects unremoved age range.
  # project.plus <- cbind(as.data.frame(project),
  #                       cluster=as.factor(groups),
  #                       agerange=RawDataFrame$CustCOBinYrs.range)
  # ggplot(project.plus, aes(x=PC1, y=PC2)) + geom_point(aes(shape=cluster)) +
  #                     geom_text(aes(label=agerange), hjust=0, vjust=1)
  #
  ggplot(project, aes(x=PC1, y=PC2)) + geom_point(aes(shape=cluster)) +
                         geom_text(aes(label=agerange), hjust=0, vjust=1)
    
    
  df.standard <- as.data.frame(Scaled.matrix)
  sapply(df.standard, mean)
  sapply(df.standard, sd)
  
  # rotations
  library(psych)
  # All numerical final RawDataFrame here.
  psychfit <-
    principal(select(ModellingDataFrame,-c(IsCb,Patient.ID)),
              nfactors = 5,
              
              
              
              
              rotate = "varimax")
  psychfit # print results
  psychfit$loadings
  psychfit$rotation
  psychfit$communality
  typeof(psychfit)
  
}

ProfileClusters <-
  function(PostClustering = FALSE,
           PrintToFile = TRUE) {
    #After clustering Profile them to findout Business specific meaning. not just technical meaning.
    # Args: k num of clusters and i Labels, Cluster type
    #
    # Returns: Nothing
    
    
  }

EvalClusterability <- function() {
  # Evaluate whether the data is clusterable or not determines whether a given dataset contains meaningful clusters (i.e., non-random structure).
  #
  # Args: matrix and the clean raw dataframe that is about to be clustered
  #
  
}


GetBestk <- function(Scaled.matrix, RawDataFrame.cluster) {
  # Answers If the data is clusterable, then how to choose the right number of expected clusters
  # Unfortunately, there is no definitive answer to this question. The optimal clustering is somehow subjective and depend on the method used for measuring similarities and the parameters used for partitioning.
  # Args: matrix and the clean raw dataframe that is about to be clustered
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  
}
ReadAgeForCB <- function() {
  # To Read Age for CB patients from an external file and then use it for analysis
  #
  # Args:
  #   Filename
  #
  # Returns:
  #   Df of Patient Id and Age
  age.df <-
    read.csv("C:\\Gaurav\\Project\\CB\\CBdata\\16thNovCBAgeConsolidated.csv")
  
    return(age.df)
}

GenSampledDataForTesting <- function(ModellingDataFrame, nSlice) {
  # Get only the Testing split
  #
  # Args: Full DF
  #
  #
  # Returns: Testing spllit
  flog.info("Inside GenSampledDataForTesting", name = 'logger.b')
  set.seed(143)
  split <-
    createDataPartition(y = ModellingDataFrame$IsCb, p = 0.75, list = FALSE)
  
  
  val <- ModellingDataFrame[-split,]
  val <- dplyr::select(val, -Patient.ID)
  
  #************************************** Changes *******************************************#
  #val <- dplyr::select(val)
  #**************************************************************************************************#
  #save(val, file = paste(nSlice,"val.Rda",sep = "_"))
  flog.info("Done with the splits %s",
            nrow(val),
            name = 'logger.b',
            capture = TRUE)
  return(val)
}


GenSampledDataForTraining <- function(ModellingDataFrame, nSlice) {
  # Gets all the possible sampled data - ROSE create troble with parallelism exceptions which are hard to debug. Hence commenting ROSE.
  #
  # Args:
  #   RawDataFrame: DF which has to be split and sampled
  #
  # Returns:
  flog.info("Inside GenSampledDataForTraining", name = 'logger.b')
  set.seed(143)
  split <-
    createDataPartition(y = ModellingDataFrame$IsCb, p = 0.75, list = FALSE)
  trainingDf <- ModellingDataFrame[split,]
  #trainingDf <- dplyr::select(trainingDf, -Patient.ID)
  trainingDf  <- subset(trainingDf, select = -c(Patient.ID))
  save(trainingDf, file = paste(nSlice,"trainingDf.Rda",sep = "_"))
  
  flog.info(
    "Done with the splitting the trainingDf frame %s",
    table(trainingDf$IsCb),
    name = 'logger.b',
    capture = TRUE
  )
  
  dim(trainingDf)
  #[1] 1153  303
  table(trainingDf$IsCb)
  # No Yes
  # 915 238
  #using rose
  
  
  # down_trainingDf_rose <- ovun.sample(IsCb ~ ., data = trainingDf, method = "under",  seed = 124)$data
  # table(down_trainingDf_rose$IsCb)
  #
  # # No Yes
  # # 230 238
  # dim(down_trainingDf_rose)
  # # [1] 468 303
  # flog.info("Done with the splitting the Down trainingDf frame %s", table(down_trainingDf_rose$IsCb), name = 'logger.b', capture = TRUE)
  #using caret
  down_trainingDf  <-
    downSample(x = trainingDf, y = as.factor(trainingDf$IsCb))
  table(down_trainingDf$IsCb)
  # No Yes
  # 238 238
  down_trainingDf <- select(down_trainingDf, -Class)
  dim(down_trainingDf)
  # [1] 476 304
  
  # over_trainingDf_rose <- ovun.sample(IsCb ~ ., data = trainingDf, method = "over",  seed = 124)$data
  # dim(over_trainingDf_rose)
  # #[1] 1810  303
  #
  # table(over_trainingDf_rose$IsCb)
  # flog.info("Done with the splitting the Over trainingDf frame %s", table(over_trainingDf_rose$IsCb), name = 'logger.b', capture = TRUE)
  # # No Yes
  # 915 895
  
  # using caret
  
  over_trainingDf <-
    upSample(x = trainingDf, y = as.factor(trainingDf$IsCb))
  over_trainingDf <- select(over_trainingDf, -Class)
  dim(over_trainingDf)
  #[1] 1830  304
  table(over_trainingDf$IsCb)
  
  # No Yes
  # 915 915
  
  # both_trainingDf_rose <- ovun.sample(IsCb ~ ., data = trainingDf, method = "both", p=0.5,  seed = 124)$data
  # dim(both_trainingDf_rose)
  # #[1] 1153  303
  # table(both_trainingDf_rose$IsCb)
  # # No Yes
  # # 602 551
  # flog.info("Done with the splitting the Both trainingDf frame %s", table(both_trainingDf_rose$IsCb), name = 'logger.b', capture = TRUE)
  #
  pure_trainingDf_rose  <-
    ROSE(IsCb ~ ., data = trainingDf, seed = 124)$data
  
  table(pure_trainingDf_rose$IsCb)
  
  # No Yes
  # 602 551
  dim(pure_trainingDf_rose)
  # [1] 1153  303
  flog.info(
    "Done with the splitting the Pure Rose trainingDf frame %s",
    table(pure_trainingDf_rose$IsCb),
    name = 'logger.b',
    capture = TRUE
  )
  
  trainingDf$IsCb <- as.factor(trainingDf$IsCb)
  trainingDf_SMOTE  <- SMOTE(IsCb ~ ., data = trainingDf)
  
  dim(trainingDf_SMOTE)
  #[1] 1666  303
  table(trainingDf_SMOTE$IsCb)
  # No Yes
  # 952 714
  flog.info(
    "Done with the splitting the  SMOTE trainingDf frame %s",
    table(trainingDf_SMOTE$IsCb),
    name = 'logger.b',
    capture = TRUE
  )
  
  lsttrainingDfData <- list()
  
  lsttrainingDfData <-
    list(
      orig_trainingDf = trainingDf,
      down_trainingDf = down_trainingDf,
      over_trainingDf = over_trainingDf,
      pure_trainingDf = pure_trainingDf_rose,
      smote_trainingDf = trainingDf_SMOTE
    )
  
  flog.info("Done with GenSampledDataForTraining", name = 'logger.b')
  
  return(lsttrainingDfData)
}

PrepareFinalDataForModel <-
  function(dfImputed.ncb,
           dfImputed,
           loadFrmFile = "No",
           nSlice = 12) {
    # To prepare final data on which the model runs
    #
    # Args:
    #   RawDataFrame: DF which has to be explored
    #
    # Returns:
    #   Data frame for the model
    # flog.appender(appender.file("other.log"), name = 'logger.b')
    # flog.info("This writes to a %s", "file", name='logger.b')
    # flog.info("My first log statement with futile.logger", name='logger.b')
    # flog.warn("This statement has higher severity", name='logger.b')
    # flog.fatal("This one is really scary", name='logger.b')
    #
    if (loadFrmFile == "Yes") {
      dfImputed <-
        read.csv("ImputedIntrvlMeans_2016_09_09_18_00_02.csv")
      dfImputed.ncb <-
        read.csv("ImputedIntrvlMeans_2016_09_09_17_59_44.csv")
      dfImputed.ncb$IsCb <- "No"
      dfImputed$IsCb <- "Yes"
      
      ModellingDataFrame <- rbind(dfImputed.ncb, dfImputed)
      ModellingDataFrame <-
        dplyr::rename(ModellingDataFrame, Patient.ID = dfInterval.Patient.ID)
      ModellingDataFrame <-
        dplyr::select(ModellingDataFrame, c(Patient.ID, IsCb), everything())
      ModellingDataFrame <- dplyr::select(ModellingDataFrame, -X)
      
    }
    else
    {
      dfImputed.ncb <- RemoveUnwantedColumns(dfImputed.ncb)
      dfImputed <- RemoveUnwantedColumns(dfImputed)
      dfImputed.ncb$IsCb <- "No"
      dfImputed$IsCb <- "Yes"
      ModellingDataFrame <- rbind(dfImputed.ncb, dfImputed)
    }
    
    ModellingDataFrame$Age <- as.numeric(ModellingDataFrame$Age)
    ModellingDataFrame$IsCb <- as.factor(ModellingDataFrame$IsCb)
    ModellingDataFrame <- GenFeat_Wrapper(ModellingDataFrame, nSlice)
    
    ModellingDataFrame <- GenSlopes_Wrapper(ModellingDataFrame, nSlice)
    
    write.csv(ModellingDataFrame, paste0(Append.Date.Time("ModellingDataFrame"), ".csv"))
    save(ModellingDataFrame,
         file = paste(nSlice, "ModellingDataFrame.Rda", sep = "_"))
    
    flog.info(
      "Written ModellingDataFrame  in CSV  with Dimensions ",
      dim(ModellingDataFrame),
      name = 'logger.b',
      capture = TRUE
    )
    return(ModellingDataFrame)
  }


PerformClassificationOnSampledDev <-
  function(dev, nameOfDev, nSlice) {
    dev$IsCb
    table(dev$IsCb)
    # To perform the actual classification on our final data
    #
    # Args:
    #   RawDataFrame: DF which has to be explored
    #
    # Returns:
    #   The model object.
    # ModellingDataFrame<-read.csv("ModellingDataFrame_2016_09_02_14_42_58.csv")
    #
    # ModellingDataFrame<-select(ModellingDataFrame,-c(X))
    
    # You can specify a summaryFunction in trainControl. The summary function
    # calculates metrics. The defaultSummary function only calculates accuracy and
    # kappa, so you need to specify another summary function that calculates area
    # under ROC. There is the twoClassSummary function, that computes sensitivity,
    # specificity and the area under the ROC curve. So adding
    # summaryFunction=twoClassSummary, classProbs=TRUE to the trainControl
    # function should allow you to specify metric="ROC" in the train function.
    # (But only for 2-class problems!)
    
    ## Define control function to handle optional arguments for train function
    ## Models to be assessed based on largest absolute area under ROC curve
    cv.ctrl <- trainControl(
      method = "repeatedcv",
      repeats = 3,
      summaryFunction = twoClassSummary,
      classProbs = TRUE
    )
     registerDoParallel(4)		# Registrer a parallel backend for train
     getDoParWorkers()
    
     set.seed(35)
     glm.tune.1 <- train(
       IsCb ~ . ,
       data = dev,
       method = "glm",
       metric = "ROC",
       trControl = cv.ctrl
     )
    
     s <- summary(glm.tune.1)
     list.models <- list(glm = glm.tune.1)
     flog.info(
       "Done with the GLM and here is the summary: ",
       glm.tune.1,
       name = 'logger.b',
       capture = TRUE
     )

    # save the model to disk

     save(glm.tune.1, file = paste(nSlice,nameOfDev,"glm.tune.1.model",sep = "_"))
    
    
    
    # First up is boosting. I can instruct train to fit a stochastic boosting model
    # for the binary response CB using the adapackage and a range of values for
    # each of three tuning parameters. Concretely, when fitting a model using train
    # with method=âadaâ, one has three levers to tweak: iter (number of boosting
    # iterations, default=50), maxdepth (depth of trees), and nu (shrinkage
    # parameter, default=1).
    
    # note the dot preceding each variable
     ada.grid <- expand.grid(.iter = c(50, 100),
                             .maxdepth = c(4, 8),
                             .nu = c(0.1, 1))
     registerDoParallel(4,cores=4)
     getDoParWorkers()
     
     set.seed(35)
     ada.tune <- train(IsCb ~ . ,
                       data = dev,
                       method = "ada",
                       metric = "ROC",
                       tuneGrid = ada.grid,
                       trControl = cv.ctrl)
     #
    # # # ada.tune
    # # # plot(ada.tune)
     flog.info("Done with the ADA Boost and here is the summary %s",ada.tune, name='logger.b',capture = TRUE)
    # # # save(ada.tune, file = "ada.tune.model")
     save(ada.tune, file = paste(nSlice,nameOfDev,"ada.tune.model",sep = "_"))
    #
    # # Time to give the popular Random Forest (RF) model a shot at the CB Pred
    # # challenge. The number of randomly pre-selected predictor variables for each
    # # node, designated mtry, is the sole parameter available for tuning an RF with
    # # train. Since the number of features is so high,
    #
    # # Strobl et al suggested setting mtry at the square root of the number of
    # # variables. In this case, that would be mtry = sqrt ncol-1 , which did produce the better
    # # RF model.
    n <- round(sqrt(ncol(ModellingDataFrame) - 1))
    registerDoParallel(4,cores=4)
    getDoParWorkers()

    rf.grid <- data.frame(.mtry = c(n, 3))
    set.seed(35)
    rf.tune <- train(
      IsCb ~ . ,
      data = dev,
      method = "rf",
      metric = "ROC",
      tuneGrid = rf.grid,
      trControl = cv.ctrl
    )
    #
    # rf.tune
    #
    flog.info("Done with Random forest and here is the summary",
              rf.tune,
              name = 'logger.b',capture = TRUE)

    save(rf.tune, file = paste(nSlice,nameOfDev,"rf.tune.model",sep = "_"))
    #
    #
    # # And finally, we'll fit a support vector machine (SVM) model to the CB pred
    # # data. There are two functions which can be tuned for SVM using train. The
    # # default value for one of them -â sigest â- produces good results on most
    # # occasions. The default grid of cost parameter C is 0.25, 0.5, and 1. If we set
    # # train argument tuneLength = 9, the grid expands to c(0.25, 0.5, 1, 2, 4, 8,
    # # 16, 32, 64). As SVM is considered sensitive to the scale and magnitude of the
    # # presented features, I'll use the preProcess argument to instruct train to make
    # # arrangements for normalizing the data within resampling loops.
    #
    set.seed(35)
    registerDoParallel(4,cores=4)
    getDoParWorkers()
    svm.tune <- train(
      IsCb ~ . ,
      data = dev,
      method = "svmRadial",
      tuneLength = 9,
      preProcess = c("center", "scale"),
      metric = "ROC",
      trControl = cv.ctrl
    )
    # svm.tune
    flog.info("Done with SVM and here is the summary: ", svm.tune, name =
                'logger.b',capture = TRUE)

    save(svm.tune, file = paste(nSlice,nameOfDev,"svm.tune.model",sep = "_"))
    
    list.models <- list(glm = glm.tune.1, ada = ada.tune, rf = rf.tune,  svm = svm.tune)
    
    flog.info("Added all the models into the list. : ", length(list.models), name =
                'logger.b',capture = TRUE)
    
    
    return(list.models)
  }

ValidateModelsOnSamples <-
  function(list.models, nSlice, nameOfDev, val) {
    # To understand the technical details of the model object and derive insights
    #
    # Args:
    #   list.models
    #
    # Returns:
    #   exploratary analysis
    
    # Model Evaluation
    ## Logistic regression model
    # load("val.Rda")
    # load("dev.Rda")
    # load(paste(nSlice,"val.Rda",sep = "_"))
    # load(paste(nSlice,"dev.Rda",sep = "_"))
    # val <- dplyr::select(val, -Patient.ID)
    # load("glm.tune.1.model")
    #load(paste(nSlice,"glm.tune.1.model",sep = "_"))
    
    flog.info(
      "Started to validate the model, we have testing set of ",
      dim(val),
      name = 'logger.b',
      capture = TRUE
    )
    pdf(file = paste(nSlice, nameOfDev,"ModelPerformance.pdf",sep = "_"))
    
    glm.tune.1 <- list.models$glm
    if (is.null(glm.tune.1)) {
      flog.error("Unfortunately glm model tune returned null ", name = 'logger.b')
    }
    # registerDoParallel(4,cores=4)
    # getDoParWorkers()
    glm.pred <- predict(glm.tune.1, val)
    glm.pred.cm <- confusionMatrix(glm.pred, val$IsCb)
    tocsv.glm.pred.cm <-
      data.frame(cbind(t(glm.pred.cm$overall), t(glm.pred.cm$byClass)))
    
    vaImp1 <- varImp(glm.tune.1)
    flog.info(
      "Variable Importance from GLM model: %s  ",
      vaImp1,
      name = 'logger.b',
      capture = TRUE
    )
    
    
    write.csv(tocsv.glm.pred.cm,
              paste(nSlice, nameOfDev, "CM.glm.pred.csv", sep = "_"))
    flog.info(
      "Confusion Matrix for GLM model: %s  ",
      tocsv.glm.pred.cm,
      name = 'logger.b',
      capture = TRUE
    )
    
    # ## Boosted model
    # #load(paste(nSlice,"ada.tune.model",sep = "_"))
     ada.tune <- list.models$ada
     # registerDoParallel(4,cores=4)
     # getDoParWorkers()
     
    ada.pred <- predict(ada.tune, val)
    ada.pred.cm<-confusionMatrix(ada.pred, val$IsCb)

    tocsv.ada.pred.cm <- data.frame(cbind(t(ada.pred.cm$overall),t(ada.pred.cm$byClass)))
    write.csv(tocsv.ada.pred.cm,paste(nSlice,nameOfDev,"CM.ADA.pred.csv",sep = "_"))
    flog.info("Confusion Matrix for ADA model: %s  ", tocsv.ada.pred.cm, name = 'logger.b',capture = TRUE)
    #
    #
    # ## Random Forest model
    # # load("rf.tune.model")
    # #load(paste(nSlice,"rf.tune.model",sep = "_"))
    rf.tune <- list.models$rf
    # registerDoParallel(4,cores=4)
    # getDoParWorkers()
    rf.pred <- predict(rf.tune, val)
    rf.pred.cm<-confusionMatrix(rf.pred, val$IsCb)
    tocsv.rf.pred.cm <- data.frame(cbind(t(rf.pred.cm$overall),t(rf.pred.cm$byClass)))
    vaImp<-varImp(rf.tune)
    flog.info("Variable Importance from RF model: %s  ", vaImp, name = 'logger.b',capture = TRUE)
    write.csv(tocsv.rf.pred.cm,paste(nSlice,vnameOfDev,"CM.rf.pred.csv",sep = "_"))
    flog.info("Confusion Matrix for RF model: %s  ", tocsv.rf.pred.cm, name = 'logger.b',capture = TRUE)
    #
    # #SVM
    #
    # #load(paste(nSlice,"svm.tune.model",sep = "_"))
    svm.tune <- list.models$svm
    # registerDoParallel(4,cores=4)
    # getDoParWorkers()
    svm.pred <- predict(svm.tune, val)
    svm.pred.cm<-confusionMatrix(svm.pred, val$IsCb)
    tocsv.svm.pred.cm <- data.frame(cbind(t(svm.pred.cm$overall),t(svm.pred.cm$byClass)))
    write.csv(tocsv.svm.pred.cm,paste(nSlice,nameOfDev,"CM.svm.pred.csv",sep = "_"))
    flog.info("Confusion Matrix for SVM model: %s  ", tocsv.svm.pred.cm, name = 'logger.b',capture = TRUE)

    
    
    require(pROC)
    ## Logistic regression model (BLACK curve)
    # registerDoParallel(4,cores=4)
    # getDoParWorkers()
    glm.probs <- predict(glm.tune.1, val, type = "prob")
    glm.ROC <- roc(
      response = val$IsCb,
      predictor = glm.probs$No,
      levels = levels(as.factor(val$IsCb))
    )
    
    tocsv.glm.pred.cm$AUC <- as.numeric(glm.ROC$auc)
    flog.info("AUC for GLM model is: ",
              tocsv.glm.pred.cm$AUC,
              name = 'logger.b',
              capture = TRUE)
    plot(glm.ROC,print.thres = "best",main="GLM with Best threshold")
    plot(glm.ROC, type = "S", main="ROC Comparision of various models")
    histogram(~glm.probs$No|val$IsCb, xlab = "GLM Prob of poor classification")
    # ## Area under the curve: 0.8609
    # ## Boosted model (GREEN curve)
    # registerDoParallel(4,cores=4)
    # getDoParWorkers()
    ada.probs <- predict(ada.tune, val, type = "prob")
    ada.ROC <- roc(response = val$IsCb,
                   predictor = ada.probs$No,
                   levels = levels(as.factor(val$IsCb)))
    plot(ada.ROC, add=TRUE, col="green")
    tocsv.ada.pred.cm$AUC <- as.numeric(ada.ROC$auc)
    histogram(~ada.probs$No|val$IsCb, xlab = "Prob of poor classification")
    #
    # ## Area under the curve: 0.8759
    # ## Random Forest model (RED curve)
    # registerDoParallel(4,cores=4)
    # getDoParWorkers()
    rf.probs <- predict(rf.tune, val, type = "prob")
    rf.ROC <- roc(
      response = val$IsCb,
      predictor = rf.probs$No,
      levels = levels(as.factor(val$IsCb))
    )
    plot(rf.ROC, add = TRUE, col = "red")
    tocsv.rf.pred.cm$AUC <- as.numeric(rf.ROC$auc)
    histogram(~rf.probs$No|val$IsCb, xlab = "Prob of poor classification")
    # ## Area under the curve: 0.8713
    # ## SVM model (BLUE curve)
    # registerDoParallel(4,cores=4)
    # getDoParWorkers()
    svm.probs <- predict(svm.tune, val, type = "prob")
    svm.ROC <- roc(
      response = val$IsCb,
      predictor = svm.probs$No,
      levels = levels(as.factor(val$IsCb))
    )
    plot(svm.ROC, add = TRUE, col = "blue")
    tocsv.svm.pred.cm$AUC <- as.numeric(svm.ROC$auc)
    histogram(~svm.probs$No|val$IsCb, xlab = "Prob of poor classification")
    ## Area under the curve: 0.8077
    
    # The following R script uses caret function resamples to collect the resampling
    # results, then calls the dotplot function to create a visualization of the
    # resampling distributions. I'm typically not one for leaning on a single metric
    # for important decisions, but if you have been looking for that one graph which
    # sums up the performance of the four models, this is it.
    
    library(caret)
    #
    # # cv.values <- resamples(list(Logit = glm.tune.1, Ada = ada.tune, RF = rf.tune, SVM = svm.tune))
    cv.values <-
      resamples(list(Logit = glm.tune.1, ada = ada.tune, RF = rf.tune, SVM = svm.tune))
    dotplot(cv.values, metric = "ROC", main = "Comparison of all Models")
    bwplot(cv.values, metric = "ROC", main = "Comparison of all Models")
    flog.info("Done with visualization of theresampling distribution %s",
              cv.values,
              name = 'logger.b',capture = TRUE)

    final.pred.cm<- rbind(tocsv.glm.pred.cm, tocsv.ada.pred.cm,tocsv.rf.pred.cm,tocsv.svm.pred.cm)
    write.csv(final.pred.cm,paste(nSlice,nameOfDev,"final.pred.cm.csv",sep = "_"))
  dev.off()
  }

PerformClassification <- function(ModellingDataFrame, nSlice) {
  # To perform the actual classification on our final data
  #
  # Args:
  #   RawDataFrame: DF which has to be explored
  #
  # Returns:
  #   The model object.
  # ModellingDataFrame<-read.csv("ModellingDataFrame_2016_09_02_14_42_58.csv")
  #
  # ModellingDataFrame<-select(ModellingDataFrame,-c(X))
  split <-
    createDataPartition(y = ModellingDataFrame$IsCb, p = 0.75, list = FALSE)
  
  dev <- ModellingDataFrame[split,]
  val <- ModellingDataFrame[-split,]
  
  dev <- dplyr::select(dev, -Patient.ID)
  val <- dplyr::select(val, -Patient.ID)
  
  save(val, file = paste(nSlice, "val.Rda", sep = "_"))
  save(dev, file = paste(nSlice, "dev.Rda", sep = "_"))
  
  flog.info("Done with the splits %s", nrow(val), name = 'logger.b')
  
  # You can specify a summaryFunction in trainControl. The summary function
  # calculates metrics. The defaultSummary function only calculates accuracy and
  # kappa, so you need to specify another summary function that calculates area
  # under ROC. There is the twoClassSummary function, that computes sensitivity,
  # specificity and the area under the ROC curve. So adding
  # summaryFunction=twoClassSummary, classProbs=TRUE to the trainControl
  # function should allow you to specify metric="ROC" in the train function.
  # (But only for 2-class problems!)
  
  ## Define control function to handle optional arguments for train function
  ## Models to be assessed based on largest absolute area under ROC curve
  cv.ctrl <- trainControl(
    method = "repeatedcv",
    repeats = 3,
    summaryFunction = twoClassSummary,
    classProbs = TRUE
  )
  registerDoParallel(4)		# Registrer a parallel backend for train
  getDoParWorkers()
  
  set.seed(35)
  glm.tune.1 <- train(
    IsCb ~ . ,
    data = dev,
    method = "glm",
    metric = "ROC",
    trControl = cv.ctrl
  )
  
  s <- summary(glm.tune.1)
  flog.info(
    "Done with the GLM and here is the summary",
    glm.tune.1,
    name = 'logger.b',
    capture = TRUE
  )
  
  # save the model to disk
  
  save(glm.tune.1, file = paste(nSlice, "glm.tune.1.model", sep = "_"))
  
  # First up is boosting. I can instruct train to fit a stochastic boosting model
  # for the binary response CB using the adapackage and a range of values for
  # each of three tuning parameters. Concretely, when fitting a model using train
  # with method=âadaâ, one has three levers to tweak: iter (number of boosting
  # iterations, default=50), maxdepth (depth of trees), and nu (shrinkage
  # parameter, default=1).
  
  # note the dot preceding each variable
  ada.grid <- expand.grid(
    .iter = c(50, 100),
    .maxdepth = c(4, 8),
    .nu = c(0.1, 1)
  )
  registerDoParallel(4, cores = 4)
  getDoParWorkers()
  
  set.seed(35)
  ada.tune <- train(
    IsCb ~ . ,
    data = dev,
    method = "ada",
    metric = "ROC",
    tuneGrid = ada.grid,
    trControl = cv.ctrl
  )
  
  # ada.tune
  # plot(ada.tune)
  flog.info(
    "Done with the ADA Boost and here is the summary %s",
    ada.tune,
    name = 'logger.b',
    capture = TRUE
  )
  # save(ada.tune, file = "ada.tune.model")
  save(ada.tune, file = paste(nSlice, "ada.tune.model", sep = "_"))
  
  # Time to give the popular Random Forest (RF) model a shot at the CB Pred
  # challenge. The number of randomly pre-selected predictor variables for each
  # node, designated mtry, is the sole parameter available for tuning an RF with
  # train. Since the number of features is so high,
  
  # Strobl et al suggested setting mtry at the square root of the number of
  # variables. In this case, that would be mtry = sqrt ncol-1 , which did produce the better
  # RF model.
  n <- round(sqrt(ncol(ModellingDataFrame) - 1))
  registerDoParallel(4, cores = 4)
  getDoParWorkers()
  
  rf.grid <- data.frame(.mtry = c(n, 3))
  set.seed(35)
  rf.tune <- train(
    IsCb ~ . ,
    data = dev,
    method = "rf",
    metric = "ROC",
    tuneGrid = rf.grid,
    trControl = cv.ctrl
  )
  
  rf.tune
  
  flog.info(
    "Done with Random forest and here is the summary",
    rf.tune,
    name = 'logger.b',
    capture = TRUE
  )
  
  save(rf.tune, file = paste(nSlice, "rf.tune.model", sep = "_"))
  
  
  # And finally, we'll fit a support vector machine (SVM) model to the CB pred
  # data. There are two functions which can be tuned for SVM using train. The
  # default value for one of them -â sigest â- produces good results on most
  # occasions. The default grid of cost parameter C is 0.25, 0.5, and 1. If we set
  # train argument tuneLength = 9, the grid expands to c(0.25, 0.5, 1, 2, 4, 8,
  # 16, 32, 64). As SVM is considered sensitive to the scale and magnitude of the
  # presented features, I'll use the preProcess argument to instruct train to make
  # arrangements for normalizing the data within resampling loops.
  
  set.seed(35)
  registerDoParallel(4, cores = 4)
  getDoParWorkers()
  svm.tune <- train(
    IsCb ~ . ,
    data = dev,
    method = "svmRadial",
    tuneLength = 9,
    preProcess = c("center", "scale"),
    metric = "ROC",
    trControl = cv.ctrl
  )
  svm.tune
  flog.info(
    "Done with SVM and here is the summary: ",
    svm.tune,
    name =
      'logger.b',
    capture = TRUE
  )
  
  save(svm.tune, file = paste(nSlice, "svm.tune.model", sep = "_"))
}



ValidateModels <- function(modelObject, nSlice) {
  # To understand the technical details of the model object and derive insights
  #
  # Args:
  #   modelObject
  #
  # Returns:
  #   exploratary analysis
  
  # Model Evaluation
  ## Logistic regression model
  # load("val.Rda")
  # load("dev.Rda")
  load(paste(nSlice, "val.Rda", sep = "_"))
  load(paste(nSlice, "dev.Rda", sep = "_"))
  # val <- dplyr::select(val, -Patient.ID)
  # load("glm.tune.1.model")
  load(paste(nSlice, "glm.tune.1.model", sep = "_"))
  
  
  
  glm.pred <- predict(glm.tune.1, val)
  glm.pred.cm <- confusionMatrix(glm.pred, val$IsCb)
  tocsv.glm.pred.cm <-
    data.frame(cbind(t(glm.pred.cm$overall), t(glm.pred.cm$byClass)))
  
  vaImp1 <- varImp(glm.tune.1)
  flog.info(
    "Variable Importance from GLM model: %s  ",
    vaImp1,
    name = 'logger.b',
    capture = TRUE
  )
  
  
  write.csv(tocsv.glm.pred.cm,
            paste(nSlice, "CM.glm.pred.csv", sep = "_"))
  flog.info(
    "Confusion Matrix for GLM model: %s  ",
    tocsv.glm.pred.cm,
    name = 'logger.b',
    capture = TRUE
  )
  
  ## Boosted model
  load(paste(nSlice, "ada.tune.model", sep = "_"))
  
  ada.pred <- predict(ada.tune, val)
  ada.pred.cm <- confusionMatrix(ada.pred, val$IsCb)
  
  tocsv.ada.pred.cm <-
    data.frame(cbind(t(ada.pred.cm$overall), t(ada.pred.cm$byClass)))
  write.csv(tocsv.ada.pred.cm,
            paste(nSlice, "CM.ADA.pred.csv", sep = "_"))
  flog.info(
    "Confusion Matrix for ADA model: %s  ",
    tocsv.ada.pred.cm,
    name = 'logger.b',
    capture = TRUE
  )
  
  
  ## Random Forest model
  # load("rf.tune.model")
  load(paste(nSlice, "rf.tune.model", sep = "_"))
  rf.pred <- predict(rf.tune, val)
  rf.pred.cm <- confusionMatrix(rf.pred, val$IsCb)
  tocsv.rf.pred.cm <-
    data.frame(cbind(t(rf.pred.cm$overall), t(rf.pred.cm$byClass)))
  vaImp <- varImp(rf.tune)
  flog.info("Variable Importance from RF model: %s  ",
            vaImp,
            name = 'logger.b',
            capture = TRUE)
  
  write.csv(tocsv.rf.pred.cm, paste(nSlice, "CM.rf.pred.csv", sep = "_"))
  flog.info(
    "Confusion Matrix for RF model: %s  ",
    tocsv.rf.pred.cm,
    name = 'logger.b',
    capture = TRUE
  )
  
  #SVM
  # load("svm.tune.model")
  load(paste(nSlice, "svm.tune.model", sep = "_"))
  svm.pred <- predict(svm.tune, val)
  svm.pred.cm <- confusionMatrix(svm.pred, val$IsCb)
  tocsv.svm.pred.cm <-
    data.frame(cbind(t(svm.pred.cm$overall), t(svm.pred.cm$byClass)))
  write.csv(tocsv.svm.pred.cm,
            paste(nSlice, "CM.svm.pred.csv", sep = "_"))
  flog.info(
    "Confusion Matrix for SVM model: %s  ",
    tocsv.svm.pred.cm,
    name = 'logger.b',
    capture = TRUE
  )
  
  
  
  require(pROC)
  ## Logistic regression model (BLACK curve)
  glm.probs <- predict(glm.tune.1, val, type = "prob")
  glm.ROC <- roc(
    response = val$IsCb,
    predictor = glm.probs$No,
    levels = levels(as.factor(val$IsCb))
  )
  tocsv.glm.pred.cm$AUC <- as.numeric(glm.ROC$auc)
  plot(glm.ROC, type = "S")
  histogram( ~ glm.probs$No |
               val$IsCb, xlab = "Prob of poor classification")
  ## Area under the curve: 0.8609
  ## Boosted model (GREEN curve)
  ada.probs <- predict(ada.tune, val, type = "prob")
  ada.ROC <- roc(
    response = val$IsCb,
    predictor = ada.probs$No,
    levels = levels(as.factor(val$IsCb))
  )
  plot(ada.ROC, add = TRUE, col = "green")
  tocsv.ada.pred.cm$AUC <- as.numeric(ada.ROC$auc)
  histogram( ~ ada.probs$No |
               val$IsCb, xlab = "Prob of poor classification")
  
  ## Area under the curve: 0.8759
  ## Random Forest model (RED curve)
  rf.probs <- predict(rf.tune, val, type = "prob")
  rf.ROC <- roc(
    response = val$IsCb,
    predictor = rf.probs$No,
    levels = levels(as.factor(val$IsCb))
  )
  plot(rf.ROC, add = TRUE, col = "red")
  tocsv.rf.pred.cm$AUC <- as.numeric(rf.ROC$auc)
  histogram( ~ rf.probs$No |
               val$IsCb, xlab = "Prob of poor classification")
  ## Area under the curve: 0.8713
  ## SVM model (BLUE curve)
  svm.probs <- predict(svm.tune, val, type = "prob")
  svm.ROC <- roc(
    response = val$IsCb,
    predictor = svm.probs$No,
    levels = levels(as.factor(val$IsCb))
  )
  plot(svm.ROC, add = TRUE, col = "blue")
  tocsv.svm.pred.cm$AUC <- as.numeric(svm.ROC$auc)
  histogram( ~ svm.probs$No |
               val$IsCb, xlab = "Prob of poor classification")
  ## Area under the curve: 0.8077
  
  # The following R script uses caret function resamples to collect the resampling
  # results, then calls the dotplot function to create a visualization of the
  # resampling distributions. I'm typically not one for leaning on a single metric
  # for important decisions, but if you have been looking for that one graph which
  # sums up the performance of the four models, this is it.
  
  library(caret)
  
  # cv.values <- resamples(list(Logit = glm.tune.1, Ada = ada.tune, RF = rf.tune, SVM = svm.tune))
  cv.values <-
    resamples(list(
      Logit = glm.tune.1,
      ada = ada.tune,
      RF = rf.tune,
      SVM = svm.tune
    ))
  dotplot(cv.values, metric = "ROC", main = "Comparison of all Models")
  bwplot(cv.values, metric = "ROC", main = "Comparison of all Models")
  flog.info(
    "Done with visualization of theresampling distribution %s",
    cv.values,
    name = 'logger.b',
    capture = TRUE
  )
  
  final.pred.cm <-
    rbind(tocsv.glm.pred.cm,
          tocsv.ada.pred.cm,
          tocsv.rf.pred.cm,
          tocsv.svm.pred.cm)
  write.csv(final.pred.cm, paste(nSlice, "final.pred.cm.csv", sep = "_"))
  
}

GenerateReport <- function(modelObject) {
  # To remove all unwanted columns for our final data on which the model runs
  #
  # Args:
  #   ModelObject
  #
  # Returns:
  #   graphs in pdf and writeups
  
}
# FnTestHours(df1, 3009408)

FnTestHours <- function(df, id) {
  r1 <- filter(df, Patient.ID == id)
  #r1 <- TransformDateTimeData(r1)
  #FnCreateSplitEfficiently(r1)
  debug(GenerateConseqHourlyDistribution)
  GenerateConseqHourlyDistribution(r1)
  
}
FnTestHoursOnSplit <- function(df) {
  splitdf <- split(df, f = df$Patient.ID)
  r1 <- lapply(splitdf, TransformDateTimeData)
  lapply(r1, FnCreateSplitEfficiently)
}
FnCreateSplitEfficiently <- function(RawDataFrame, nSlice = 12) {
  sampl1 <- select(
    RawDataFrame,
    Age,
    HR,
    Obs_Time.Date,
    RR,
    TEMP.C,
    ART.DBP,
    NBP.D,
    ART.SBP,
    NBP.S,
    ART.MBP,
    NBP.M,
    CVP,
    SPO2
  )
  xts.obj <- xts(sampl1, order.by = sampl1$Obs_Time.Date)
  try(lst <- last(xts.obj, '12 hours'))
  # print(difftime(last(lst),first(lst)))
  # try(lst1<-data.frame(date=index(lst), coredata(lst))
  xts.split.byHrs1 <- split.xts(lst, f = "hours")
  All.Obs.in.int.list <- list()
  nLen <- length(xts.split.byHrs1)
  niter <- 0
  if (nLen >= nSlice)  {
    niter <- nSlice
  }
  else if (nLen < nSlice) {
    niter <- nLen
  }
  
  for (i in 1:niter)
  {
    q1 <-
      data.frame(date = index(xts.split.byHrs1[i]), coredata(xts.split.byHrs1[i]))
    q2 <- select(q1,
                 Age,
                 HR,
                 RR,
                 TEMP.C,
                 ART.DBP,
                 NBP.D,
                 ART.SBP,
                 NBP.S,
                 ART.MBP,
                 NBP.M,
                 CVP,
                 SPO2)
    q2 <-
      as.data.frame(lapply(q2, function(f)
        as.numeric(levels(f))[f]))
    q3 <- select(q2, -Age)
    q4 <- lapply(q3, function(x)
      mean(x, na.rm = TRUE))
    All.Obs.in.int.list <- c(All.Obs.in.int.list, q4)
  }
  if (nLen < nSlice)
  {
    niter1 <- niter + 1
    for (i in niter1:nSlice)
    {
      q5 <- CreateDummyDF()
      All.Obs.in.int.list <- c(All.Obs.in.int.list, q5)
    }
  }
  m2 <- as.data.frame(All.Obs.in.int.list)
  m2$Age <- unique(q2$Age)
  m2$Patient.ID <- unique(RawDataFrame$Patient.ID)
  return(m2)
}

GenHourlyDistribution <- function(RawDataFrame) {
  # To Generate  hourly observation frequency.  Such as Patient 935548 has 15
  # uniq observations (freq). These observations are taken across 6 numHours and
  # the distribution is l2- 2 3 4 1 4 1 observations per each hour
  #
  # Args: Rawdataframe
  #
  # Returns: graphs in pdf and writeups
  tbl.Hourdistro <- as.data.frame(table(RawDataFrame$Patient.ID))
  names(tbl.Hourdistro) <- c("Patient.ID", "frequency")
  
  for (patient in  unique(RawDataFrame$Patient.ID)) {
    difTim <- NULL
    sampl1 <- filter(RawDataFrame, Patient.ID == patient)
    xts.obj <- xts(sampl1[, -1], order.by = sampl1$Obs_Time.Date)
    try(lst <- last(xts.obj, '12 hours'))
    try(difTim <- difftime(last(lst), first(lst), units = 'hours'))
    numHours <- nhours(xts.obj)
    xts.split.byHrs <- split.xts(xts.obj, f = "hours")
    xts.split.Distribution <- list()
    for (i in 1:length(xts.split.byHrs))
    {
      xts.split.Distribution[[i]] <- nrow(xts.split.byHrs[[i]])
    }
    tbl.Hourdistro$numHours[tbl.Hourdistro$Patient.ID == patient] <-
      numHours
    tbl.Hourdistro$DiffTime[tbl.Hourdistro$Patient.ID == patient] <-
      format(difTim)
    tbl.Hourdistro$hourlyDistribution[tbl.Hourdistro$Patient.ID == patient] <-
      paste(as.character(c(unlist(
        xts.split.Distribution
      ))), collapse = ",")
    
  }
  write.csv(tbl.Hourdistro, "CBHourlyDistribution.csv")
}


SetDebug <- function() {
  # To enable debugging
  #
  # Args:
  #   none
  #
  # Returns:
  #   none
  debug(ExecMain)
  debug(ValidateModels)
  
  debug(ImputeColsWithNorms)
  
  debug(PerformClassification)
  
  debug(PrepareFinalDataForModel)
  
  debug(LoadDF_ncb)
  
  debug(ExploreIntervalData)
  
  debug(CreateIntervalsDf)
  
  debug(LoadDF)
  
  debug(ExploreDataHighLevel)
  
  debug(PlotData)
  
  debug(TransformAllCategData)
  
  debug(TransformDateTimeData)
  
  debug(TransformAllNumericData)
  
  debug(ConvertCatFactorToNumericData)
  
  debug(ExploreDataLowLevel)
  
  debug(RemoveUnwantedRows)
  
  debug(HandleNaNullNan)
  
  debug(RemoveUnwantedColumns)
  
  # debug(PrintClusters)
  #
  # debug(GetScaledMat)
  #
  # debug(GetDisMat)
  #
  debug(DoPCA)
  
  debug(EvalClusterability)
  
  debug(GetBestk)
  
  # debug(DoHClust)
  
  debug(DoClustering)
  
  debug(ValidateClusters)
  
  # debug(ReadClusToDF)
  
  debug(ProfileClusters)
  
  debug(ImputeIntervalsWithX)
  
  debug(GenHourlyDistribution)
  
  # debug(GetARowPerPatWtMeans)
  #
  # debug(GetARowPerPatWtCounts)
  
  
}

SetUndebug <- function() {
  # To disable debugging
  #
  # Args:
  #   None
  #
  # Returns:
  #   None
  undebug(ImputeColsWithNorms)
  
  undebug(PrepareFinalDataForModel)
  
  undebug(LoadDF_ncb)
  
  undebug(ImputeIntervalsWithX)
  
  undebug(ExploreIntervalData)
  
  undebug(CreateIntervalsDf)
  
  undebug(GetARowPerPatWtMeans)
  
  undebug(ExploreDataHighLevel)
  
  undebug(PlotData)
  
  undebug(TransformAllCategData)
  
  undebug(TransformDateTimeData)
  
  undebug(TransformAllNumericData)
  
  undebug(ConvertCatFactorToNumericData)
  
  undebug(ExploreDataLowLevel)
  
  undebug(RemoveUnwantedRows)
  
  undebug(HandleNaNullNan)
  
  undebug(RemoveUnwantedColumns)
  
  undebug(DoPCA)
  undebug(GetDFForCB)
  undebug(GetDFForNCB)
}

ExecMain <- function(DoComplete = TRUE,
                     isDebug = TRUE) {
  # The main function from where all other functions are called.
  #
  # Args:
  #   DoComplete is TRUE  when you want to run from scratch-> ie  all the steps in for the model from loaddata to Generate report(). This is done in the initial phases of the development
  #   DoComplete is FALSE  when the model is already created, but you want to run only validation or graphing part.
  #   isDebug is TRUE when you want to breakpoint and debug. False if you are damn sure about what you just wrote and ready to fly. - Congrats.
  # Returns:
  #   None
  
  # Create all global variables that are computed once and used manytimes across funs.
  if (isDebug == TRUE) {
    SetDebug()
  }
  else
  {
    SetUndebug()
  }
  InitEnv()
  
  
  if (DoComplete == TRUE)
  {
    
    RawDataFrame<-GetDFForCB()
    save(RawDataFrame, file = "RawDataFrame.Rda")
    # handled the age of CB seperately here in a global var so that it can be used in other fn calls
    gAgeDf <- data.frame()
    gAgeDf <- createDFOneAgePerPatient(RawDataFrame)
    save(gAgeDf, file = "gAgeDf.Rda")
    load("gAgeDf.Rda")
    assign('gAgeDf', gAgeDf, .GlobalEnv)
    # ExploreDataHighLevel(RawDataFrame)
    # ExploreDataLowLevel(RawDataFrame)
    
    
    ncbdf<-GetDFForNCB()
    print("After getting df for NonCB ")
    save(ncbdf, file = "ncbdf.Rda")
    # ExploreDataHighLevel(ncbdf)
    # ExploreDataLowLevel(ncbdf)
    load("ncbdf.Rda")
    str(ncbdf)
    table(ncbdf$Unit)
    load("RawDataFrame.Rda")
    
    str(RawDataFrame)
    RunSliceBasedClassifications_wrapper(RawDataFrame, ncbdf)
    
  }
  else
  {
    # you came here because DoComplete is false and you have already created the model- Great job.
    
    # sink('C:\\Gaurav\\Project\\CB\\op\\Output.txt')
    
    # ModellingDataFrame <- PrepareFinalDataForModel(ncbdf12, cb12, "No", nSlice)
    # 
    # modelObject <- PerformClassification(ModellingDataFrame, nSlice)
    # 
    # ValidateModels(modelObject)
  }
}

ExecMain(DoComplete = TRUE, isDebug = TRUE)
