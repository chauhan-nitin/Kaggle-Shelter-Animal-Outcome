###############################################################################
#~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ Ingesting & Preprocessing Datasets ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
###############################################################################
rm(list=ls(all=TRUE))
#Current working directory
getwd()
#Set working directory to where your data files are stored
setwd("D:\\My Files\\Animalsshelter") 

#Import the data and read the file.
traindata<-read.csv(file = "train.csv", header = TRUE,na.strings=c(""))
testdata<-read.csv(file = "test.csv", header = TRUE,na.strings=c(""))

#Structure of train and test dataset.
str(traindata)
str(testdata)

#Summary of test and train dataset.
summary(traindata)
summary(testdata)

#Checking the NA values per variable in train and test dataset.
na_train <-sapply(traindata, function(x) sum(is.na(x)))
na_train <- data.frame(na_train)

na_test <-sapply(testdata, function(x) sum(is.na(x)))
na_test <- data.frame(na_test)

#Checking total NA values present in test and train dataset.
sum(is.na(traindata))
sum(is.na(testdata))

#Checking names of variables present in test and train dataset.
names(traindata)
names(testdata)

#Converting AnimalID to ID in traindata to reduce inconsistency.
names(traindata)[1]='ID'

#The test dataset does not contain the 'OutcomeSubtype' column which means that we won't use it as a feature.
traindata$OutcomeSubtype<- NULL

#Converting the variable 'ID' to integer in traindata.
traindata$ID = as.integer(traindata$ID)

#Structure of traindata.
str(traindata)

#Plotting 
library(ggplot2)
ggplot(traindata, aes(OutcomeType, fill=AnimalType)) + 
  geom_bar(width=0.8) +
  facet_wrap(~ AnimalType)+
  labs(y = 'Frequency Of Animals', 
       x = 'Outcome') +
  ggtitle("Cats vs Dogs") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(face="bold", angle=45))


#Checking the levels present in the variable Ageupontime.
levels(traindata$AgeuponOutcome)

#Checking missing values in Ageupontime variable in traindata.
sum(is.na(traindata$AgeuponOutcome))


#Checking missing values in Ageupontime variable in testdata.
sum(is.na(testdata$AgeuponOutcome))


library(DMwR)
colMeans(is.na(traindata)) > 0.2

#Remove 18 rows with missing values in the train dataset.
traindata=traindata[-which(is.na(traindata$AgeuponOutcome)),]
str(traindata)

#Grouping all the information in days,month and years under '0 years' and creating a new numeric 
#variable 'AgeinYears' and sorting. 

traindata$AgeinYears = traindata$AgeuponOutcome
a = traindata$AgeinYears
summary(a)
str(a)
ind_train = union(union(grep('month',a),grep('day',a)),grep('week',a))
traindata$AgeinYears[ind_train] = '0 years'
traindata$AgeinYears= as.numeric(gsub('year|years',"",traindata$AgeinYears))
traindata= traindata[order(traindata$AgeinYears),]

str(traindata)

#Plotting 
ggplot(traindata, aes(AgeinYears, fill=OutcomeType)) + 
  geom_bar(width=1.0) +
  facet_wrap(~ AnimalType)+
  labs(y = 'Frequency Of Animals', 
       x = 'Outcome Age in Years') +
  ggtitle("Outcome Age and Outcome Type : Cats vs Dogs") + 
  theme(plot.title = element_text(hjust = 0.1)) +
  theme(axis.text.x = element_text(face="bold"))

#Break ages down in four 
#categories: very young [<1 year], young adults [1-5 yrs], adults [5-10], mature [>10]


traindata$AgeCatr = ifelse(traindata$AgeinYears == 0, 'very young',
                    ifelse(traindata$AgeinYears >=1 & traindata$AgeinYears <= 5, 'young adult',
                    ifelse(traindata$AgeinYears > 5 & traindata$AgeinYears <= 10,'adult','mature')))
traindata$AgeCatr = factor(traindata$AgeCatr,levels = c('very young','young adult','adult','mature'))




library(dplyr) #Data manipulation.
bycatr = group_by(traindata,AnimalType,AgeCatr,OutcomeType)
sumc = summarise(bycatr,count = n())

#Plotting
ggplot(sumc, aes(x = AgeCatr, y = count, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Percentage of Animals', 
       x = 'Age of Animals',
       title = 'Outcomes by Age : Cats and Dogs') 


#Grouping all the dates in days,month and years under '0 years' and creating a new numeric 
#variable 'AgeinYears' and sorting. [testdata set].

testdata$AgeinYears = testdata$AgeuponOutcome
b = testdata$AgeinYears
ind_test = union(union(grep('month',b),grep('day',b)),grep('week',b))
testdata$AgeinYears[ind_test] = '0 years'
testdata$AgeinYears= as.numeric(gsub('year|years',"",testdata$AgeinYears))


#Break ages down in four 
#Categories: very young [<1 year], young adults [1-5 yrs], adults [5-10], mature [>10].

testdata$AgeCatr = ifelse(testdata$AgeinYears == 0, 'very young',
                   ifelse(testdata$AgeinYears >=1 & testdata$AgeinYears <= 5, 'young adult',
                   ifelse(testdata$AgeinYears > 5 & testdata$AgeinYears <= 10,'adult','mature')))
testdata$AgeCatr = factor(testdata$AgeCatr,levels = c('very young','young adult','adult','mature'))


traindata$AgeinYears=NULL
testdata$AgeinYears=NULL

#Train a decision tree in order to fill in the missing values in the test dataset. 
library(rpart)
ageform = AgeCatr ~ AnimalType
agemodel = rpart(ageform,method="class", data=traindata)
summary(agemodel)
print(agemodel)

#Check the missing values in AgeCatr and replace with highest probability category.
sum(is.na(testdata$AgeCatr))

agepred = predict(agemodel, newdata = testdata[which(is.na(testdata$AgeCatr)),])
ageprediction = (colnames(agepred)[max.col(agepred,ties.method="first")])
#Since 'very young' has high probability replace 6 NA values with it.
testdata$AgeCatr[which(is.na(testdata$AgeCatr))]=ageprediction


#Checking missing values of Breed Variable in train and test dataset.
sum(is.na(traindata$Breed))

sum(is.na(testdata$Breed))

#Checking the levels present in Breed variable.
length(levels(traindata$Breed))

str(traindata)

#Splitting Breed in two types : crossbreed and purebreed and consider cross as 0 and pure as 1.

traindata$AggBreed=sapply(as.character(traindata$Breed),function(x) ifelse(grepl('/',x),paste(strsplit(x, split = '/')[[1]][1],'Mix',sep=' '),x))
traindata$IsPureBreed = ifelse(grepl('Mix',traindata$AggBreed),0,1)
#Converting Shorthair mix cat as Domestic as it is considered as pure breed.
traindata$IsPureBreed[grep('Domestic',traindata$AggBreed)]=0
traindata$AggBreed[grep('Domestic',traindata$AggBreed)] = 'Domestic'
length(traindata$AggBreed)

#Checking the amount of dogs and cats.

aggregate(data=traindata, AggBreed ~ AnimalType, function(x) length(unique(x)))

#Grouping the respected variables and counting.

bycatr = group_by(traindata,AnimalType,IsPureBreed,OutcomeType)
sumc = summarise(bycatr,count = n())

#Plotting
ggplot(sumc, aes(x = IsPureBreed, y = count, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Percentage of Animals', 
       x = 'Purity of Breed',
       title = 'Outcomes by Age : Cats & Dogs')



testdata$AggBreed=sapply(as.character(testdata$Breed),function(x) ifelse(grepl('/',x),paste(strsplit(x, split = '/')[[1]][1],'Mix',sep=' '),x))
testdata$IsPureBreed = ifelse(grepl('Mix',testdata$AggBreed),0,1)
testdata$IsPureBreed[grep('Domestic',testdata$AggBreed)]=0
testdata$AggBreed[grep('Domestic',testdata$AggBreed)] = 'Domestic'
testdata$AggBreed=NULL
traindata$AggBreed=NULL


#Check the missing values of SexuponOutcome variable in train and test data.
sum(is.na(traindata$SexuponOutcome))
sum(is.na(testdata$SexuponOutcome))

#Replace 1 missing NA value with 'unknown'.
traindata$SexuponOutcome[which(is.na(traindata$SexuponOutcome))]='Unknown'

#Checking the levels present in SexuponOutcome variable.
levels(traindata$SexuponOutcome)

#Grouping the respected variables and counting.
bycatr = group_by(traindata,AnimalType,SexuponOutcome,OutcomeType)
sumc = summarise(bycatr,count = n())

#Plotting
ggplot(sumc, aes(x = SexuponOutcome, y = count, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Percentage of Animals', 
       x = 'SexUponOutcome',
       title = 'Outcomes by Sex: Cats and Dogs') 


#According to plot above it is found that Adoptation rate is more for neutered and spayed 
#and it seems that sex is not very relevant
#Create a variable to group intact and unknown vs neutered and spayed. 
traindata$operated = 
  ifelse(traindata$SexuponOutcome == 'Neutered Male' | 
           traindata$SexuponOutcome == 'Spayed Female','yes','no/maybe')


#Grouping the respected variables and counting.
bycatr = group_by(traindata,AnimalType,AgeCatr,operated)
sumc = summarise(bycatr,count = n())

#Plotting
ggplot(sumc, aes(x = AgeCatr, y = count, fill = operated)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Percentage of Animals', 
       x = 'Age Category') +
       ggtitle("% of neutered/spayed animals by Age Category: Cats and Dogs") + 
       theme(plot.title = element_text(hjust = 0.3))


#Add operated variable to test data as well.
testdata$operated = ifelse(testdata$SexuponOutcome == 'Neutered Male' | 
                         testdata$SexuponOutcome == 'Spayed Female','yes','no/maybe')
traindata$operated = as.factor(traindata$operated)
testdata$operated = as.factor(testdata$operated)

str(traindata$operated)
str(testdata$operated)


#Checking the levels present in Color variable.
length(levels(traindata$Color))

#Checking color as per Animal Type.
aggregate(data=traindata, Color ~ AnimalType, function(x) length(unique(x)))
##Color variable seems to be less relevant.##

#Checking the na values present in datetime variable.
sum(is.na(traindata$DateTime))

sum(is.na(testdata$DateTime))


#Using library lubridate for performing operations on date.
library(lubridate)

#Storing month,weekdays,year and hour in variables accordingly.
traindata$month = month(traindata$DateTime)
traindata$day   = wday(traindata$DateTime)
traindata$year  = year(traindata$DateTime)
traindata$hour  = hour(traindata$DateTime)

#Creating vectors for seasonality.
winter = c(12,1,2)
spring = c(3,4,5)
summer = c(6,7,8)
autumn = c(9,10,11)
weekend = c(6,7)
working = c(1,4)
morning = c(8,9,10,11)
earlymorng = c(5,6,7)
midday = c(12,13,14,15)
afternoon = c(16,17,18)
evening = c(19,20,21)
night = c(22,23,0,1,2,3,4)

#Storing month under season variable.
traindata$season =  ifelse(is.element(traindata$month,winter),'winter',
                    ifelse(is.element(traindata$month,spring),'spring',
                    ifelse(is.element(traindata$month,autumn),'autumn','summer')))

#Storing days under daytype variable.
traindata$daytype = ifelse(is.element(traindata$day,weekend),'weekend',
                    ifelse(is.element(traindata$day,working),'working','Friday'))

#Storing time under time variable.
traindata$time   =  ifelse(is.element(traindata$hour,earlymorng),'early morning',
                    ifelse(is.element(traindata$hour,morning),'morning',
                    ifelse(is.element(traindata$hour,midday),'midday',
                    ifelse(is.element(traindata$hour,evening),'evening','night'))))

#Converting season,daytype,time to factors.
traindata$season = as.factor(traindata$season)
traindata$daytype = as.factor(traindata$daytype)
traindata$time = as.factor(traindata$time)

str(traindata)

#Grouping the respected variables and counting.
bycatr = group_by(traindata,AnimalType,season,OutcomeType)
sumc = summarise(bycatr,count = n())

#Plotting
ggplot(sumc, aes(x = season, y = count, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Percentage of Animals', 
       x = 'Seasons') +
       ggtitle("Outcomes by Seasons") + 
        theme(plot.title = element_text(hjust = 0.1))



bycatr = group_by(traindata,AnimalType,daytype,OutcomeType)
sumc = summarise(bycatr,count = n())

#Plotting
ggplot(sumc, aes(x = daytype, y = count, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Percentage of Animals', 
       x = 'Type of Day',
       title = 'Outcomes by Weekdays')

#Performing the same on test dataset for later use.

testdata$month = month(testdata$DateTime)
testdata$day   = wday(testdata$DateTime)
testdata$year  = year(testdata$DateTime)
testdata$hour  = hour(testdata$DateTime)

testdata$season  =  ifelse(is.element(testdata$month,winter),'winter',
                    ifelse(is.element(testdata$month,spring),'spring',
                    ifelse(is.element(testdata$month,autumn),'autumn','summer')))
testdata$daytype =  ifelse(is.element(testdata$day,weekend),'weekend',
                    ifelse(is.element(testdata$day,working),'working','Friday'))
testdata$time    =  ifelse(is.element(testdata$hour,earlymorng),'early morning',
                    ifelse(is.element(testdata$hour,morning),'morning',
                    ifelse(is.element(testdata$hour,midday),'midday',
                    ifelse(is.element(testdata$hour,evening),'evening','night'))))
testdata$season = as.factor(testdata$season)
testdata$daytype = as.factor(testdata$daytype)
testdata$time = as.factor(testdata$time)

str(testdata)

#############################################################
#---------------------- Modeling ---------------------------#
#############################################################

#Split the data into train and test.
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
set.seed(111)
ind = createDataPartition(traindata$OutcomeType,p=0.75)[[1]]
train = traindata[ind,]
valid = traindata[-ind,]

#Using Decision Tree Model
mypred = OutcomeType~ AnimalType + AgeCatr + IsPureBreed + operated + season + daytype + time
model_dt <-rpart(mypred, method="class", data=traindata)
print(model_dt)
fancyRpartPlot(model_dt)


pred = predict(model_dt, newdata = valid)
prediction = (colnames(pred)[max.col(pred,ties.method="first")])
accuracy = sum(prediction== as.character(valid$OutcomeType))/length(prediction)
accuracy

#Using Random Forest Model.
library(randomForest)
model_rf  <- randomForest(mypred, data=traindata)
print(model_rf)

pred = predict(model_rf, newdata = valid)
accuracy_rf = sum(pred == as.character(valid$OutcomeType))/length(pred)
accuracy_rf


#Checking the variable importance.
importance(model_rf)


#Checking the prediction with month and day.

mypred_2<- OutcomeType~ AnimalType + AgeCatr + IsPureBreed + operated + month + day + time
model_rf_2<- randomForest(mypred_2, data=traindata)


pred_2 = predict(model_rf_2, newdata = valid)
accuracy_rf_2 = sum(pred_2 == as.character(valid$OutcomeType))/length(pred_2)
accuracy_rf_2

#Checking the variable importance.
importance(model_rf_2)

model_rf_2  <- randomForest(mypred_2, data=traindata)
prediction <- predict(model_rf_2, testdata, type = 'vote')
submission <- data.frame('ID' = testdata$ID, prediction)
write.csv(submission, 'submission_rf.csv', row.names = F)


