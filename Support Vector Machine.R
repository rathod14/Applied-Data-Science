# IST687 – HW 10  - Prediction with Support Vector Machines
# Rahul Rathod 
# Due date- 11/08/2018
# Submitted Date-11/07/2018


install.packages("RJSONIO") #install package RJSONIO
library(RJSONIO)

#Part A: Load and condition the data  

#loading the data
data<-"hotelsurveysherison.json" #storing json file in a variable
hotelSurvey<- fromJSON(data, simplify = TRUE, nullValue = NA)#loading json file
hotelSurvey<- data.frame(hotelSurvey) #converting json file to dataframe
hotelSurvey<- hotelSurvey[,-11]# remove free text column
View((hotelSurvey))

#Part B: Create a happy customer variable 

#generating a new column happyCust where overallCustSat is 8 or higher
hotelSurvey$happCust <- hotelSurvey$overallCustSat >7
View(hotelSurvey)

#Part C: Create training and test data sets

install.package ("kernlab") #install package kernlab
library(kernlab)

#creating train and test dataset
randIndex<-  sample(1:dim(hotelSurvey)[1])
summary(randIndex)
head(randIndex)
cutPoint2_3 <- floor(2*dim(hotelSurvey)[1]/3)
cutPoint2_3
trainData<- hotelSurvey[randIndex[1:cutPoint2_3],]
testData<- hotelSurvey[randIndex[(cutPoint2_3+1):dim(hotelSurvey)[1]],]

#verfying that train and test dataset contain appropriate number of cases
dim(trainData)
dim(testData)

#Part D: Build a Model using ksvm( ) 
#model 1s
#building a support vector model using 3 variables to predict a happy customer
svmOutput <- ksvm(happCust ~ hotelClean+hotelFriendly+guestAge, data=trainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
svmOutput
#kernel ='rbfdot' refers to the radial basis function kernel takes the set of inputs from each row in a dataset
#and calculates a distance value based on the combination of the many variables in the row.
#the "kpar" argument refers to a variety of parameters that can be used to control the operation of the radial 
#basis function kernel.
#the C argument refers to the so called "cost of constraints."
#we have specified "cross=3." Cross refers to the cross validation model that the algorithm uses.
#"prob.model=TRUE," dictates that we use a so called three-fold cross validation in order to generate the 
#probabilities associate with whether a message is or isn’t a spam message

#Part E: Predict Values in the Test Data and Create a Confusion Matrix

#validating the model by predicting for testdata
svmPred <- predict(svmOutput, testData, type = "votes")
svmPred
#the first command in the block uses model svmOutput as parameter for prediction
#it uses testData to generate predictions, it request votes from the prediction process

str(svmPred) #review content of svmPred
head(svmPred)
#creating a confusion matrix that compares the second row of svmPred to the contents of testData$happy variable.
happypred <- svmPred[,1]
happypred[happypred>= .8] <- 1
happypred[happypred< .8] <- 0
happypred

compTable<- data.frame(testData$happCust,happypred)
table(compTable)
# (250+481)/3334
#error rate = 21.9%% that means our model didnt predicted that well

#Part F: Find a good prediction
#model 2
svmOutput1 <- ksvm(happCust ~ checkInSat+hotelClean+ hotelFriendly+guestAge, data=trainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
svmOutput1
svmPred1 <- predict(svmOutput1, testData, type = "votes")
svmPred1
str(svmPred1) #review content of svmPred
head(svmPred1)
happypred1 <- svmPred1[,1]
happypred1[happypred1>= .8] <- 1
happypred1[happypred1< .8] <- 0
happypred1

compTable<- data.frame(testData$happCust,happypred1)
table(compTable)
#error rate= (412+239)/3334
#19.52% 

#model 3
svmOutput2 <- ksvm(happCust ~ checkInSat+hotelClean+ whenBookedTrip, data=trainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
svmOutput2
svmPred2 <- predict(svmOutput2, testData, type = "votes")
svmPred2
str(svmPred2) #review content of svmPred
head(svmPred2)
happypred2 <- svmPred2[,1]
happypred2[happypred2>= .8] <- 1
happypred2[happypred2< .8] <- 0
happypred2

compTable<- data.frame(testData$happCust,happypred2)
table(compTable)
#error rate =(460+158)/3334
#18.53
#model no. 3 provided the lowest error rate

#It is important to have test dataset so that we can validate our model by predicting on test Data 
#which will show that the model which we created is good or bad
