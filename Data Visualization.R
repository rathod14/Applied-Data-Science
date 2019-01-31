# IST687 – Getting Started with ggplot2
#Rahul Rathod 
#Due date- 10/10/2018
#Submitted Date-10/10/2018

#Step A: Load and Merge datasets
#1)	Read in the census dataset (using the function created in HW 3)
readstates<- function()#creating function
{
  #states<- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv")
  states <- read.csv("states.csv")
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
  return(states)#returning value
}
states<-readstates()#calling function
View(states) #display states
#2)Copy the USArrests dataset into a local variable (similar to HW 2)
str(states)
arrests<- USArrests#USArrests copied to arrests
View(arrests)#display arrests

#3)	Create a merged dataframe -- with the attributes from both dataset

arrests$stateName<- row.names(arrests)
View(arrests)
mergeDF<- merge(states, arrests, by = "stateName")
View(mergeDF)
#Hint: get the state names from the USArrests dataset with the rownames() 
#Hint: use the merge() command 

#Step B: Explore the Data – Understanding distributions

#4)	Create a histogram using ggplot2() for the population and a different histogram for the murder rate

install.packages("ggplot2") #install ggplot
library(ggplot2)
#creating histogram  for population 
myplotpop1<- ggplot(mergeDF, aes(x=population)) #defining table for which we need to make graph the values on X-axis and Y-axis
myplotpop1<- myplotpop1 + geom_histogram(binwidth = 500000,color="red",fill="yellow",na.rm=TRUE) #define which graph to be made and define different parameters
myplotpop1<- myplotpop1 +ggtitle("Histogram of Population")#adding title to the hsitogram
myplotpop1
#creating histogram  for Assault
myplotpop1<- ggplot(mergeDF, aes(x=Assault)) 
myplotpop1<- myplotpop1 + geom_histogram(binwidth = 500000,color="red",fill="yellow",na.rm=TRUE)
myplotpop1<- myplotpop1 +ggtitle("Histogram of Assault")
myplotpop1
#creating histogram  for Rape
myplotpop1<- ggplot(mergeDF, aes(x=Rape)) 
myplotpop1<- myplotpop1 + geom_histogram(binwidth = 500000,color="red",fill="yellow",na.rm=TRUE)
myplotpop1<- myplotpop1 +ggtitle("Histogram of Rape")
myplotpop1
#creating histogram  for UrbanPop
myplotpop1<- ggplot(mergeDF, aes(x=UrbanPop)) 
myplotpop1<- myplotpop1 + geom_histogram(binwidth = 500000,color="red",fill="yellow",na.rm=TRUE)
myplotpop1<- myplotpop1 +ggtitle("Histogram of UrbanPop")
myplotpop1
#creating hustogram for murder
myplotpop2<- ggplot(mergeDF, aes(x=Murder))
myplotpop2<- myplotpop2 + geom_histogram(binwidth = 5)
myplotpop2<- myplotpop2 +ggtitle("Histogram of Murder_Rate")
myplotpop2S

#5)	Create a boxplot for the population, and a different boxplot for the murder rate.
#creating boxplot for population
myplotpop3<- ggplot(mergeDF, aes(y=population, x=factor(0)))
myplotpop3<- myplotpop3 + geom_boxplot()
myplotpop3<- myplotpop3 +ggtitle("Histogram of Population")
myplotpop3          
#creating boxplot for murder
myplotpop4<- ggplot(mergeDF, aes(y=Murder, x=factor(0)))
myplotpop4<- myplotpop4 + geom_boxplot()
myplotpop4<- myplotpop4 +ggtitle("Histogram of Murder_Rate")
myplotpop4

#6)	Create a block comment explaining which visualization (boxplot or histogram) you thought was more helpful (explain why)
#I feel histogram is more useful because it provides more information than boxplot. Histograms are better when we need visualization of wide variance data.

#Step C: Which State had the Most Murders – bar charts
#7)Calculate the number of murders per state
#create new column for murder per state
mergeDF$numMurders<- (mergeDF$Murder/100000)*(mergeDF$population)
View(mergeDF)
#8)	Generate a bar chart, with the number of murders per state
#Hint: use the geom_col() function
#create bar chart for murder per state using geom_col
myplotpop5<- ggplot(mergeDF, aes(y=numMurders, x=stateName))
myplotpop5<- myplotpop5 + geom_col()
myplotpop5<- myplotpop5 +ggtitle("Bar Chart of Murder per state")
myplotpop5

#9)Generate a bar chart, with the number of murders per state. Rotate text (on the X axis), so we can see x labels, also add a title named “Total Murders”.
#creat bar chart and rotate text 
myplotpop5<- ggplot(mergeDF, aes(y=numMurders, x=stateName))
myplotpop5<- myplotpop5 + geom_col() + theme(axis.text.x=element_text(angle=90, hjust=1))
myplotpop5<- myplotpop5 +ggtitle("Total Murder")
myplotpop5
#10)Generate a new bar chart, the same as in the previous step, but also sort the x-axis by the murder rate
#create bar chart and sorting x-axis
myplotpop5<- ggplot(mergeDF, aes(y=numMurders, x=reorder(stateName,numMurders)))
myplotpop5<- myplotpop5 + geom_col() + theme(axis.text.x=element_text(angle=90, hjust=1))
myplotpop5<- myplotpop5 +ggtitle("Total Murder")
myplotpop5
#11)Generate a third bar chart, the same as the previous step, but also showing percentOver18 as the color of the bar
#create bar chart and showing percent over 18 as color
myplotpop5<- ggplot(mergeDF, aes(y=numMurders, x=reorder(stateName,numMurders),fill=percentOver18))
myplotpop5<- myplotpop5 + geom_col() + theme(axis.text.x=element_text(angle=90, hjust=1))
myplotpop5<- myplotpop5 +ggtitle("Total Murder")
myplotpop5

#Step D: Explore Murders – scatter chart

#12)Generate a scatter plot – have population on the X axis, the percent over 18 on the y axis, and the size & color represent the murder rate
#creating scatter plot poplution on X-axis, the percent over 18 on Y-axis
myplotpop6 <-ggplot(mergeDF, aes(y=percentOver18, x=population))
myplotpop6 <-myplotpop6 + geom_point(aes(size=numMurders, color=numMurders))
myplotpop6 <-myplotpop6 + geom_text(aes(label=stateName), size=4, hjust=1,  vjust=-1)#adding text for the points
myplotpop6




