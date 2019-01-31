# IST687 – Linear Modelling
#Rahul Rathod 
#Due date- 10/25/2018
#Submitted Date-10/24/2018

install.packages("RJSONIO") #install package RJSONIO
library(RJSONIO)
install.packages("ggplot2")
library(ggplot2)

hss<-"hotelS.json" #storing json file in a variable
hotelSurvey<- fromJSON(hss, simplify = TRUE, nullValue = NA)#loading json file
hotelSurvey<- data.frame(hotelSurvey) #converting json file to dataframe
str(hotelSurvey) #view structure of file
View(hotelSurvey) #viewing the dataframe

#The dependent variable will be overallCustSat therefore overallCustSat will be on y-axis
#plotting overallCustSat with respect to other independent variables
#Size of the hotel
hotelSize<-jitter(hotelSurvey$hotelSize) #giving more spreadout datapoints on the plot
overallCustSat<- jitter(hotelSurvey$overallCustSat)
x1<- ggplot(hotelSurvey,aes(x=hotelSize,y=overallCustSat)) + geom_point() 
x1

checkInSat<-jitter(hotelSurvey$checkInSat)
x2<-ggplot(hotelSurvey, aes(x=checkInSat,y=overallCustSat)) + geom_point()
x2

hotelState<- jitter(hotelSurvey$hotelState)
x3<-ggplot(hotelSurvey, aes(x=hotelState,y=overallCustSat)) + geom_point()
x3

hotelClean<- jitter(hotelSurvey$hotelClean)
x4=ggplot(hotelSurvey,aes(x=hotelClean,y=overallCustSat)) + geom_point()
x4

hotelFriendly=jitter(hotelSurvey$hotelFriendly)
x5=ggplot(hotelSurvey,aes(x=hotelFriendly,y=overallCustSat)) + geom_point()
x5

gender<- jitter(hotelSurvey$gender)
x6<-ggplot(hotelSurvey,aes(x=gender,y=overallCustSat)) + geom_point()
x6

guestAge<- jitter(hotelSurvey$guestAge)
x7<- ggplot(hotelSurvey, aes(x=guestAge,y=overallCustSat)) + geom_point()
x7

lenghtOfStay<- jitter(hotelSurvey$lengthOfStay)
x8<- ggplot(hotelSurvey, aes(x=lengthOfStay,y=overallCustSat)) + geom_point()
x8

whenBookedTrip<- jitter(hotelSurvey$whenBookedTrip)
x9<- ggplot(hotelSurvey, aes(x=whenBookedTrip,y=overallCustSat)) + geom_point()
x9

#creating model for overallCustSat w.r.t to all other variable but not freeText
model<- lm(formula=overallCustSat ~hotelSize + checkInSat + hotelState + hotelClean + hotelFriendly + gender + guestAge + lenghtOfStay + whenBookedTrip, data= hotelSurvey )
summary(model)
#The R -squared value is 0.6685 and the p-value is extremely less than 0.05,
#which signifies that the model developed is a very good one, which is quite 
#obvious as the number of independent variables are more in this particular model
#which provide an extremely good fit for the model on the data, thus the high value of R-squared

#The significant variables as obtained from the summary with their coefficients are
#1. checkInSat -  0.0000046240720
#2. hotelClean- 1.01e-08 
#3. hotelFriendly - < 2e-16
#4. guestAge - < 2e-16

#The above model is mostly dependent on the above four signifacnt varaibles,
#which are satisfaction while checking in, friendliness of the hotel,
#age of guests and hotel in the state of maine. These independent variables have a 
#very low p-value and contribute the most to the fit of the model on the overall customer 
#satisfaction values.

#considering hotelFriendly as the highest significant hotel because it has low p value
model2<- lm(formula=overallCustSat ~ hotelFriendly, data= hotelSurvey)
plot(overallCustSat, hotelFriendly)
abline(model2) #to draw line over linear model
plot<- ggplot(hotelSurvey, aes(x=hotelFriendly,y=overallCustSat)) + geom_point() + stat_smooth(method="lm", color='blue')
plot
summary(model2)
#considering Cleanliness and age of guest of the hotel for modelling
model3<- lm(formula=overallCustSat ~ hotelClean + guestAge , data= hotelSurvey)
plot<- ggplot(hotelSurvey, aes(x=hotelClean + guestAge ,y=overallCustSat)) + geom_point() + stat_smooth(method="lm", color='red')
plot
summary(model3)
#From the two linear models, it can inferred that friendliness of the hotel
#when considered alone, is the most significant variable
#This is because in the first liunear model, the number of significant variables were more
#and thus , the model there provided us with a better R-squared value and a better fit
#through the data. However, in the second model, the most significant variable alone here 
#cannot provide the best fit, but it provides a better fit than the other 3 significant variables
#because of its comparatively high R-squared value and a very low p-value, which is an 
#important contributor. Still, the first linear model would be a more accurate one because of both
#a high R-squared and a low p-value



