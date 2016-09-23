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


factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)
library(data.table)
train <- data.table(train)

train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

