#IST687- Accident Analysis using JSON- HW5
#Rahul Rathod 
#Due date- 10/03/2018
#Submitted Date-10/03/2018

#Installing Packages & Libraries:

install.packages("RCurl") #install package Rcurl
library(RCurl)
install.packages("RJSONIO") #install package RJSONIO
library(RJSONIO)
install.packages("jsonlite") #install package jsonlite
library(jsonlite)

#Step A: Load the data
# 1	Read in the following JSON dataset
# http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD

url = "http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD" #Downloading The JSON Dataset From The Web Using URL
x = getURL(url) 

#fromJSON() Function & Its Methods Read Content In JSON & Deserializes Into R Objects:
result=fromJSON(x)
str(result)
summary(result)
View(result)

#After Viewing The First Data You'll See That It Is The Metadata i.e The Information About Data
Data1=result[[1]] 
View(Data1)

#After Viewing The Second Data You'll See It Is The Actual Dataset
Data2=result[[2]]
View(Data2)

#Step B:Clean the data
# 2.	Remove the first 8 columns, 
#Cleaning The Data By Removing The First 8 Columns & Storing It Into A New Dataframe Named "newframe":
newdataframe=Data2[,-1:-8] #remove 8 columns
View(newdataframe) #validate new dataset
str(newdataframe) #structure of new data frame

#Giving Names To Each Variable(Column) Using "colnames()" Function:
colnames(newdataframe)=c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2") #rename columns
View(newdataframe) #validate new data frame
str(newdataframe) #structure of new data frame

#Step C: Exploring Data Using Dataframe:

# 4. What Was The Total Number Of Accidents With Injuries?
newdataframe1=data.frame(newdataframe)
newdataframe1

new<-newdataframe1$INJURY # Storing A Single Column "INJURY" Into A "new" Variable
View(new) #validate new
y=table(new) #table() Function Builds A Contingency Table Of The Counts At Each Combination Of Factor Levels: (Here It Is "YES" & "NO")
y

#5.  How Many Injuries Occured Each Day Of Week?
#6.  What Is The Number Of Accidents On SUNDAY?

new1=newdataframe1$DAY_OF_WEEK #storing A Single Column "DAY_OF_WEEK" Into a "new1" Variable
View(new1) #valiate new1
w=table(new1) #table() Function Builds A Contingency Table Of The Counts At Each Combination Of Factor Levels: (Here It Is "SUNDAY","MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY")
w

#Step D: Exploring Data Using dplyr:

#Installing Package & Library "dplyr":
install.packages("dplyr") #install package dply
library(dplyr)

#7 : What Was The Total Number Of Accidents With Injuries?
#filter() Funtion Finds The Rows Where The Particular Condition Is True
AccidentsDueToInjury=filter(newdataframe1,newdataframe1$INJURY=="YES")
nrow(AccidentsDueToInjury) #Returns The Number Of Rows

#8 : What Is The Number Of Accidents On SUNDAY?
AccidentsOnSunday=filter(newdataframe1,trimws(newdataframe1$DAY_OF_WEEK)=="SUNDAY")
nrow(AccidentsOnSunday)

#9 : How Many Injuries Occured Each Day Of Week?
df=group_by(newdataframe1, newdataframe1$DAY_OF_WEEK)
View(df)
InjuriesOnEachDay=summarise(df,total=n())
InjuriesOnEachDay

#10 : It Is Much Easier To Use dplyr Package & Library, Not Just Because The Syntax Becomes Easy,
#But One Can Use Multiple Conditions In The filter() Function To Find Subset Of A Dataframe

#Step E: Explore The Distribution Of The Number Of Vehicles In Accidents:

#11 :	What Is The Distribution Of The Number Of Vehicles In Accidents On FRIDAY?
AccidentsOnFriday=filter(newdataframe1,trimws(newdataframe1$DAY_OF_WEEK)=="FRIDAY")
nrow(AccidentsOnFriday)
NoOfAccidentFriday=AccidentsOnFriday$VEHICLE_COUNT
View(NoOfAccidentFriday)
#Omitting Any NA Values
NewNoOfAccidentFriday=na.omit(NoOfAccidentFriday)
#Using as.numeric() Function To Convert The VEHICAL_COUNT Data Values Into Numerical
NewNoOfAccidentFriday=as.numeric(as.character(NewNoOfAccidentFriday))
View(NewNoOfAccidentFriday)
hist(NewNoOfAccidentFriday)
quantile(NewNoOfAccidentFriday)

  
#12 :	How Does This Distribution Compare With The Distribution Of The Number Of Vehicles In Accidents On Sunday?
AccidentsOnSunday=filter(newdataframe1,trimws(newdataframe1$DAY_OF_WEEK)=="SUNDAY")
nrow(AccidentsOnSunday)
NoOfAccidentSunday=AccidentsOnSunday$VEHICLE_COUNT
View(NoOfAccidentSunday)
#Omitting Any NA Values
NewNoOfAccidentSunday=na.omit(NoOfAccidentSunday)
#Using as.numeric() Function To Convert The VEHICAL_COUNT Data Values Into Numerical
NewNoOfAccidentSunday=as.numeric(as.character(NewNoOfAccidentSunday))
View(NewNoOfAccidentSunday)
hist(NewNoOfAccidentSunday)
quantile(NewNoOfAccidentSunday)
