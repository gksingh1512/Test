#Few new lines added

#importing data to carmileage

carmileage <- read.csv(choose.files(),header = T,stringsAsFactors = F)

#data Preparation

str(carmileage)

# To check for null
sum(is.na(carmileage$MPG))
sum(is.na(carmileage$Cylinders))
sum(is.na(carmileage$Displacement))
sum(is.na(carmileage$Horsepower))
sum(is.na(carmileage$Weight))
sum(is.na(carmileage$Acceleration))
sum(is.na(carmileage$Model_year))
sum(is.na(carmileage$Origin))
sum(is.na(carmileage$Car_Name))

# To check if it has ? or some thing like that.
table(carmileage$MPG)
table(carmileage$Cylinders)
table(carmileage$Displacement)
table(carmileage$Horsepower)
table(carmileage$Weight)
table(carmileage$Acceleration)
table(carmileage$Model_year)
table(carmileage$Origin)
table(carmileage$Car_Name)




#Variables Formatting

carmileage$Cylinders <- as.factor(carmileage$Cylinders)
carmileage$Model_year<- as.factor(carmileage$Model_year)
carmileage$Origin    <- as.factor(carmileage$Origin)
carmileage$Car_Name  <- as.factor(carmileage$Car_Name)

#Data Cleaning

str(carmileage)

#missing values

table(carmileage$Horsepower)
carmileage[carmileage$Horsepower=="?","Horsepower"] <- NA
table(is.na(carmileage$Horsepower))

carmileage[which(is.na(carmileage$Horsepower)),"Horsepower"] <- mean(as.numeric(carmileage$Horsepower),na.rm = T)
carmileage$Horsepower <- as.numeric(carmileage$Horsepower)

#outliers

#MPG

boxplot(carmileage$MPG)           #no outliers
boxplot(carmileage$Displacement)  #no outliers
boxplot(carmileage$Horsepower)    #no outliers
boxplot(carmileage$Weight)        #no outliers

boxplot(carmileage$Acceleration) # has outlier, So remove by capping flooring
quantile(carmileage$Acceleration,probs = seq(0,1,0.01))
carmileage[carmileage$Acceleration < 11,"Acceleration"] <- 11   
carmileage[carmileage$Acceleration > 19.654,"Acceleration"] <- 19.654

#Check again for outlier
#boxplot(carmileage$Acceleration)

#Variables Transformation

#Displacement

carmileage[carmileage$Displacement < 100 ,"Displacement"] <- "lessthan_100"
carmileage[carmileage$Displacement < 200 ,"Displacement"] <- "lessthan_200"
carmileage[carmileage$Displacement != "lessthan_100" & carmileage$Displacement != "lessthan_200","Displacement"] <- "greaterthan_200"

carmileage$Displacement <- as.factor(carmileage$Displacement)
levels(carmileage$Displacement)  

#Horsepower

table(carmileage$Horsepower)
carmileage[carmileage$Horsepower < 100,"Horsepower"] <- "Horsepower_lessthan_100"
carmileage[carmileage$Horsepower < 200,"Horsepower"] <- "Horsepower_lessthan_200"
carmileage[carmileage$Horsepower != "Horsepower_lessthan_100" & carmileage$Horsepower != "Horsepower_lessthan_200" ,"Horsepower"] <- "Horsepower_greaterthan_200"

carmileage$Horsepower <- as.factor(carmileage$Horsepower)
levels(carmileage$Horsepower)

#Model_year

levels(carmileage$Model_year)

levels(carmileage$Model_year)[1:4] <- "2003_to_2006"
levels(carmileage$Model_year)[2:6] <- "2007_to_2011"
levels(carmileage$Model_year)[3:6] <- "2012_to_2015"

#Car_Name

levels(carmileage$Car_Name)

levels(carmileage$Car_Name)[1:16]   <- "amc"
levels(carmileage$Car_Name)[2:7]    <- "audi"
levels(carmileage$Car_Name)[3:4]    <- "bmw"
levels(carmileage$Car_Name)[4:17]   <- "buick"
levels(carmileage$Car_Name)[5:6]    <- "cadillac"
levels(carmileage$Car_Name)[6]      <- "capri"
levels(carmileage$Car_Name)[7:32]   <- "chevroelt"
levels(carmileage$Car_Name)[8:10]   <- "chevy"
levels(carmileage$Car_Name)[9:14]   <- "chrysler"
levels(carmileage$Car_Name)[10:29]  <- "datsun"
levels(carmileage$Car_Name)[11:35]  <- "dodge"
levels(carmileage$Car_Name)[12:18]  <- "fiat"
levels(carmileage$Car_Name)[13:47]  <- "ford"
levels(carmileage$Car_Name)[14]     <- "hi"
levels(carmileage$Car_Name)[15:23]  <- "honda"
levels(carmileage$Car_Name)[16:17]  <- "maxda"
levels(carmileage$Car_Name)[17:25]  <- "mazda"
levels(carmileage$Car_Name)[18:20]  <- "mercedes"
levels(carmileage$Car_Name)[19:29]  <- "mercury"
levels(carmileage$Car_Name)[20]     <- "nissan"
levels(carmileage$Car_Name)[21:29]  <- "oldsmobile"
levels(carmileage$Car_Name)[22:23]  <- "opel"
levels(carmileage$Car_Name)[23:27]  <- "peugeot"
levels(carmileage$Car_Name)[24:48]  <- "plymouth"
levels(carmileage$Car_Name)[25:37]  <- "pontiac"
levels(carmileage$Car_Name)[26:30]  <- "renault"
levels(carmileage$Car_Name)[27:29]  <- "saab"
levels(carmileage$Car_Name)[28:29]  <- "subaru"
levels(carmileage$Car_Name)[29:45]  <- "toyota"
levels(carmileage$Car_Name)[30]     <- "triumph"
levels(carmileage$Car_Name)[31:43]  <- "vokswagen"
levels(carmileage$Car_Name)[32:37]  <- "volvo"
levels(carmileage$Car_Name)[33:37]  <- "vw"

#Create the dummy variables

str(carmileage)  

#for Cylinders

dummy_Cylinders <- data.frame(model.matrix(~Cylinders, data = carmileage))
dummy_Cylinders <- dummy_Cylinders[,-1]

#for Displacement

dummy_Displacement <- data.frame(model.matrix(~Displacement, data = carmileage))
dummy_Displacement <- dummy_Displacement[,-1]

#for Horsepower

dummy_Horsepower <- data.frame(model.matrix(~Horsepower, data = carmileage))
dummy_Horsepower <- dummy_Horsepower[,-1]

#for Model_year

dummy_Model_year <- data.frame(model.matrix(~Model_year, data = carmileage))
dummy_Model_year <- dummy_Model_year[,-1]

#for Origin

dummy_Origin <- data.frame(model.matrix(~Origin, data = carmileage))
dummy_Origin <- dummy_Origin[,-1]

#for Car_Name

dummy_Car_Name <- data.frame(model.matrix(~Car_Name, data = carmileage))
dummy_Car_Name <- dummy_Car_Name[,-1]

#Combine the dummy variables and the numeric columns of carmileage dataset.

carmileage_1 <- cbind(carmileage[ ,c(1,5,6)],dummy_Cylinders,dummy_Displacement,dummy_Horsepower,dummy_Model_year,dummy_Origin,dummy_Car_Name)

#Divide you data in taining and test datasets 

set.seed(100)
indices= sample(1:nrow(carmileage_1),0.7*nrow(carmileage_1))

train=carmileage_1[indices,]
test = carmileage_1[-indices,] 

#Develop the first model        

model_1 <-lm(MPG~.,data=train)
summary(model_1)   

#Apply the stepwise approach

library(MASS)
step <- stepAIC(model_1, direction="both")
step

model_2 <- lm(formula = MPG ~ Weight + Cylinders4 + Cylinders5 + Cylinders6 + 
                Cylinders8 + Displacementlessthan_100 + HorsepowerHorsepower_lessthan_100 + 
                Model_year2007_to_2011 + Model_year2012_to_2015 + Origin2 + 
                Origin3 + Car_Nameaudi + Car_Namebmw + Car_Namefiat + Car_Nameford + 
                Car_Namemercedes + Car_Nameopel + Car_Namepeugeot + Car_Nameplymouth + 
                Car_Namepontiac + Car_Namerenault + Car_Namesaab + Car_Namevokswagen + 
                Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_2)

#Now calculate the VIF of this model

library(car)

vif(model_2)

#Remove the variables from the model whose VIF is more than 2
#By check the maximum VIF and then the significance value of that variable, and then take the call of removing this variable

#removing HorsepowerHorsepower_lessthan_100

model_3 <- lm(formula = MPG ~ Weight + Cylinders4 + Cylinders5 + Cylinders6 + 
                Cylinders8 + Displacementlessthan_100 + 
                Model_year2007_to_2011 + Model_year2012_to_2015 + Origin2 + 
                Origin3 + Car_Nameaudi + Car_Namebmw + Car_Namefiat + Car_Nameford + 
                Car_Namemercedes + Car_Nameopel + Car_Namepeugeot + Car_Nameplymouth + 
                Car_Namepontiac + Car_Namerenault + Car_Namesaab + Car_Namevokswagen + 
                Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_3)

vif(model_3)

#removing Car_Nameaudi

model_4 <- lm(formula = MPG ~ Weight + Cylinders4 + Cylinders5 + Cylinders6 + 
                Cylinders8 + Displacementlessthan_100 + 
                Model_year2007_to_2011 + Model_year2012_to_2015 + Origin2 + 
                Origin3 + Car_Namebmw + Car_Namefiat + Car_Nameford + 
                Car_Namemercedes + Car_Nameopel + Car_Namepeugeot + Car_Nameplymouth + 
                Car_Namepontiac + Car_Namerenault + Car_Namesaab + Car_Namevokswagen + 
                Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_4)

vif(model_4)

#removing Car_Namevokswagen

model_5 <- lm(formula = MPG ~ Weight + Cylinders4 + Cylinders5 + Cylinders6 + 
                Cylinders8 + Displacementlessthan_100 + 
                Model_year2007_to_2011 + Model_year2012_to_2015 + Origin2 + 
                Origin3 + Car_Namebmw + Car_Namefiat + Car_Nameford + 
                Car_Namemercedes + Car_Nameopel + Car_Namepeugeot + Car_Nameplymouth + 
                Car_Namepontiac + Car_Namerenault + Car_Namesaab + 
                Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_5)

vif(model_5)

cor(train$Cylinders4,train$Weight)

#removing Cylinders4 since cylinder4 and weight has multicollinearity

model_6 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                Cylinders8 + Displacementlessthan_100 + 
                Model_year2007_to_2011 + Model_year2012_to_2015 + Origin2 + 
                Origin3 + Car_Namebmw + Car_Namefiat + Car_Nameford + 
                Car_Namemercedes + Car_Nameopel + Car_Namepeugeot + Car_Nameplymouth + 
                Car_Namepontiac + Car_Namerenault + Car_Namesaab + 
                Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_6)

vif(model_6)

#removing Cylinders8

model_7 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                Displacementlessthan_100 + 
                Model_year2007_to_2011 + Model_year2012_to_2015 + Origin2 + 
                Origin3 + Car_Namebmw + Car_Namefiat + Car_Nameford + 
                Car_Namemercedes + Car_Nameopel + Car_Namepeugeot + Car_Nameplymouth + 
                Car_Namepontiac + Car_Namerenault + Car_Namesaab + 
                Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_7)

vif(model_7)

#removing Origin2

model_8 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                Displacementlessthan_100 + 
                Model_year2007_to_2011 + Model_year2012_to_2015 + 
                Origin3 + Car_Namebmw + Car_Namefiat + Car_Nameford + 
                Car_Namemercedes + Car_Nameopel + Car_Namepeugeot + Car_Nameplymouth + 
                Car_Namepontiac + Car_Namerenault + Car_Namesaab + 
                Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_8)

vif(model_8)

#removing Origin3

model_9 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                Displacementlessthan_100 + 
                Model_year2007_to_2011 + Model_year2012_to_2015 + 
                Car_Namebmw + Car_Namefiat + Car_Nameford + 
                Car_Namemercedes + Car_Nameopel + Car_Namepeugeot + Car_Nameplymouth + 
                Car_Namepontiac + Car_Namerenault + Car_Namesaab + 
                Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_9)

vif(model_9)

#removing Car_Namebmw

model_10 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Namefiat + Car_Nameford + 
                 Car_Namemercedes + Car_Nameopel + Car_Namepeugeot + Car_Nameplymouth + 
                 Car_Namepontiac + Car_Namerenault + Car_Namesaab + 
                 Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_10)

#removing Car_Nameopel

model_11 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Namefiat + Car_Nameford + 
                 Car_Namemercedes + Car_Namepeugeot + Car_Nameplymouth + 
                 Car_Namepontiac + Car_Namerenault + Car_Namesaab + 
                 Car_Namevolvo + Car_Nametoyota + Car_Namesubaru, data = train)
summary(model_11)

#removing Car_Nametoyota

model_12 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Namefiat + Car_Nameford + 
                 Car_Namemercedes + Car_Namepeugeot + Car_Nameplymouth + 
                 Car_Namepontiac + Car_Namerenault + Car_Namesaab + 
                 Car_Namevolvo + Car_Namesubaru, data = train)
summary(model_12)

#removing Car_Namesaab

model_13 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Namefiat + Car_Nameford + 
                 Car_Namemercedes + Car_Namepeugeot + Car_Nameplymouth + 
                 Car_Namepontiac + Car_Namerenault + 
                 Car_Namevolvo + Car_Namesubaru, data = train)
summary(model_13)

#removing Car_Namerenault

model_14 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Namefiat + Car_Nameford + 
                 Car_Namemercedes + Car_Namepeugeot + Car_Nameplymouth + 
                 Car_Namepontiac + 
                 Car_Namevolvo + Car_Namesubaru, data = train)
summary(model_14)

#removing Car_Namevolvo

model_15 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Namefiat + Car_Nameford + 
                 Car_Namemercedes + Car_Namepeugeot + Car_Nameplymouth + 
                 Car_Namepontiac + 
                 Car_Namesubaru, data = train)
summary(model_15)

#removing Car_Namepeugeot

model_16 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Namefiat + Car_Nameford + 
                 Car_Namemercedes + Car_Nameplymouth + 
                 Car_Namepontiac + 
                 Car_Namesubaru, data = train)
summary(model_16)

#removing Car_Namesubaru

model_17 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Namefiat + Car_Nameford + 
                 Car_Namemercedes + Car_Nameplymouth + 
                 Car_Namepontiac, data = train)
summary(model_17)

#removing Car_Namefiat

model_18 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Nameford + 
                 Car_Namemercedes + Car_Nameplymouth + 
                 Car_Namepontiac, data = train)
summary(model_18)

#removing Car_Namemercedes

model_19 <- lm(formula = MPG ~ Weight + Cylinders5 + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Nameford + 
                 Car_Nameplymouth + 
                 Car_Namepontiac, data = train)
summary(model_19)

#removing Cylinders5

model_20 <- lm(formula = MPG ~ Weight + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Nameford + 
                 Car_Nameplymouth + 
                 Car_Namepontiac, data = train)
summary(model_20)

#removing Car_Namepontiac

model_21 <- lm(formula = MPG ~ Weight + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Nameford + 
                 Car_Nameplymouth, data = train)
summary(model_21)

#removing Car_Nameplymouth

model_22 <- lm(formula = MPG ~ Weight + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015 + 
                 Car_Nameford, data = train)
summary(model_22)

#removing Car_Nameford

model_23 <- lm(formula = MPG ~ Weight + Cylinders6 + 
                 Displacementlessthan_100 + 
                 Model_year2007_to_2011 + Model_year2012_to_2015, data = train)
summary(model_23)

#Test the model on test dataset

Predict_1 <- predict(model_22,test[,-c(1)])

#Add a new column "test_predict" into the test dataset

test$teat_MPG <- Predict_1

#calculate the test R square

cor(test$MPG,test$teat_MPG)
cor(test$MPG,test$teat_MPG)^2 
