#importing train and Test data.
train <- read.csv(choose.files(),header = T,stringsAsFactors = F)
test <- read.csv(choose.files(),header = T,stringsAsFactors = F)


#Data analysation
dim(train)
dim(test)

# Train
train[1:5]
head(train,5)
names(train)
dim(train) str(train) View(train)
c <- lapply(train, function(x) sum(is.na(x)))

#================================Work on target Variable=============================#
str(train$income_level)
train$income_level = ifelse(train$income_level == -50000,0,1)
table(train$income_level)

#Frequency table 
round(prop.table(table(train$income_level))*100)



####################Differnciate factors and numerical variables#########################
factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)
library(data.table)
train <- data.table(train)
test <- data.table(test)

train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]


test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#subset categorical variables
cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]

#subset numerical variables
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE] 


rm(train,test) #save memory


#=========================Analysing the numerical variable=============================#
#load libraries
install.packages("plotly")
library(ggplot2)
library(plotly)

#write a plot function
tr <- function(a){
  ggplot(data = num_test, aes(x= a, y=..density..)) + xlab("My x label") +
    ylab("My y label") + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
  ggplotly()
}

lapply(num_test, tr)



