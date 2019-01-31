#IST687-Doing some initial dtat analysis-HW3
#Rahul Rathod 
#Due date- 09/19/2018
#Submitted Date-09/19/2018

#Step A: Use read.csv( ) and url( ) to read a CSV file form the web into a data frame
#1.	Use R code to read directly from a URL on the web. Store the dataset into a new dataframe, called dfStates. Use stringsAsFactors=FALSE. 
dfstates<- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv")#read dataset from web
dfstates#view dataset
str(dfstates)#view structure of dataset
#Step B: Clean the dataframe
#2.	Use View( ), head( ), and tail( ) to examine the data frame. 
View(dfstates)#view dataset
head(dfstates)#view top of dataset
tail(dfstates)#view bottom of dataset
#3.	Remove unneeded columns and rows by using the minus sign in the rows or columns of the [ , ] accessor. 
nrow(dfstates)#verifying rows
dfstates<- dfstates[-1,]#deleting row
nrow(dfstates)#verfiy rows
View(dfstates)#view dataset
#4.	Remove the last Row (for Puerto Rico)
dfstates<-dfstates[-52,]#deleting row
#5.	Make sure there are exactly 51 rows (one per state + the district of Columbia). 
nrow(dfstates)#verfiying rows
#6.	Make sure there are precisely 4 columns, with the following names:stateName, population, popOver18, percentOver18. 
ncol(dfstates)#verfiying columns
dfstates<-dfstates[,-1:-4]#deleting 4 columns
ncol(dfstates)#verfiy columns
colnames(dfstates)<-c("stateName","population","popOver18","percentOver18")#rename columns
View(dfstates)#veiw dataset
#Step C: Create a Function
#7.	Create a function that takes no parameters and returns the clean dataframe created in step 6 above.
readstates<- function()#creating function
{
  states<- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv")
  str(states)#view structure of dataset
  View(states)#view dataset
  head(states)#view top of dataset
  tail(states)#view bottom of dataset
  nrow(states)#verfiying rows
  states<- states[-1,]#deleting row
  nrow(states)#verfiying rows
  View(states)#view dataset
  states<-states[-52,]#deleting row
  nrow(states)#verfiying rows
  ncol(states)#verfiy columns
  states<-states[,-1:-4]#deleting columns
  ncol(states)#verifying columns
  colnames(states)<-c("stateName","population","popOver18","percentOver18")#rename columns
  View(states)#view dataset
  return()#returning value
}
states<- readstates()#calling function
str(states)
#Step D: Explore the dataframe
#8.	Calculate the average population of the states
mean(dfstates$population)#mean of population
#9.	Find the state with the highest population  (use which.max)
High.population.index<- dfstates[which.max(dfstates$population),]#finding highest population
High.population.index#view highest population
#10.	Create a histogram of the state populations, what do you observe?
hist(dfstates$population, breaks=20)#ploting a histogram
#Highest bar of histogram reaches till frequency 15
#Lowest bar of histogram reaches close to frequency 1
#Histogram is in descending order(Higher-Lower)
#11.	Sort the data frame by population 
sortdata<-dfstates[order(dfstates$population),]#create sorted dataframe
#12.	Show the 10 states with the lowest populations
sortdata[1:10,]#view 10 states with lowest population
#13.	Use barplot( ) to create a plot of each of the population from the sorted dataframe.  What do you observe?
barplot(sortdata$population)#create bar plot for population
#There is a Expontential increase in the bar plot from low to high
#The last bar is reaches the maxiumu height compare to other bars
#The bar plot forms a half U-cure
barplot(sortdata$popOver18)#create bar plot for popOver18
#There is a Expontential increase in the bar plot from low to high
#The last bar is reaches the maxiumu height compare to other bars
#The bar plot forms a half U-cure
barplot(sortdata$percentOver18)#create bar plot for percentOver18
#All the bars heights are almost equal to each other
#The bar plot from a wave curve.
