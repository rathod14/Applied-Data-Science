#IST687-Creating a Function and Sampling-HW4
#Rahul Rathod 
#Due date- 09/26/2018
#Submitted Date-09/26/2018

#Part A: Write a function to reveal the distribution of a vector of numeric values
#1.	Create a new function ’ and have it take one numeric vector as its input argument. 
#2.	Make the function print the following information for the vector supplied in the argument:
    #a.	Mean b.	Median c.	Min & Max d.	Standard deviation e.	0.05 and 0.95 quantiles 
#4.	Add labels to each element of the function’s output.
printVecInfo <- function(vec) #create function name 'printVecInfo'
{
  print(paste("The mean is ",mean(vec))) # print and label mean
  print(paste("The median is ",median(vec))) # print and label median
  print(paste("The max is ",max(vec), "The min is ",min(vec))) # print and label max
  print(paste("The standard deviation is ",sd(vec))) # print and label standard deviation
  print(paste("The quantile for 0.05 is ",quantile(vec,probs=0.05))) # print and label qunatile for 0.05
  print(paste("The quantile for 0.95 is ",quantile(vec,probs=0.95))) # print and label quantitle for 0.95
}

#3.	Test the function with this vector: testVector <- 1:10. 
testVector<- 1:10 #testVector is an inbuilt dataframe (Printing first 10 Values)
testVector #print testVector
printVecInfo(testVector) #testing function on testVector
hist(testVector) #create histogram

#Part B: Read the census dataset
#5.	Read in the Census dataset.
readstates<- function()#creating function
{
  states<- read.csv("states.csv")
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
states <- readstates() #calling function and storing result in states

#Part C: Sample from the state population data frame
#6.	Sample 20 observations from states$population and use printVecInfo( ) to display the characteristics of the resulting sample, and then display the results as a histogram.
#7.	Repeat step five two more times. Each time that you create a sample, run the resulting vector through printVecInfo( ) and create a histogram. 
#8.	Using a block comment, explain in a comment why each result is different.

par(mfrow = c(3,2)) #create visualization with 3 rows two columns

samp1 <- sample(states$population, size= 20, replace=TRUE) #sample() function
printVecInfo(samp1) #passing samp1 in printVecInfo() function created abvove which will calculate mean(), median() etc
options(scipen=999)
hist(samp1, main="Sampled Histogram 1", xlab="population", ylab="frequency") #displaying results by creating histogram

samp2 <- sample(states$population, size= 20, replace=TRUE) #sample() function 
printVecInfo(samp2) #passing samp2 in printVecInfo() function created abvove which will calculate mean(), median() etc
options(scipen=999)
hist(samp2, main="Sampled Histogram 2", xlab="population", ylab="frequency") #displaying results by creating histogram

samp3 <- sample(states$population, size= 20, replace=TRUE)
printVecInfo(samp3) #passing samp3 in printVecInfo() function created abvove which will calculate mean(), median() etc
options(scipen=999)
hist(samp3, main="Sampled Histogram 3", xlab="population", ylab="frequency") #displaying results by creating histogram

#Observations: 

#The shape of histogram is its most informative characteristic: it allows us to see where a large amount of data is situated and where there is less data.It basically shows the frequency.
#For each sample it shows where majority of sample lies.

#Part D: Replicate the sampling

#9.	Use the replicate function, to replicate the sampling (described in step 5 above). Replicate the sampling 2000 times, then use printVecInfo( ) to display the characteristics of the resulting replicated sample, and then display the results as a histogram.
#10.	Repeat step 8 two more times. Each time that you create the replicated sample, run the resulting vector through printVecInfo( ) and create a histogram. 
#11.	 Using a block comment,  explain why the histograms generated in Part C are different than the histograms generated in Part D

rep1 <- replicate(2000,mean(sample(states$population, size= 20, replace=TRUE)),simplify=TRUE)
printVecInfo(rep1) #passing rep1 in printVecInfo() function created abvove which will calculate mean(), median() etc
options(scipen=999)
hist(rep1, main = "Replicated Histogram 1", xlab="population", ylab="frequency") #displaying results by creating histogram

rep2 <- replicate(2000,mean(sample(states$population, size= 20, replace=TRUE)),simplify=TRUE)
printVecInfo(rep2) ##passing rep2 in printVecInfo() function created abvove which will calculate mean(), median() etc
options(scipen=999)
hist(rep2, main = "Replicated Histogram 2", xlab="population", ylab="frequency") #displaying results by creating histogram

rep3 <- replicate(2000,mean(sample(states$population, size= 20, replace=TRUE)),simplify=TRUE)
printVecInfo(rep3)#passing rep3 in printVecInfo() function created abvove which will calculate mean(), median() etc
options(scipen=999)
hist(rep3, main = "Replicated Histogram 3", xlab="population", ylab="frequency") #displaying results by creating histogram

#Observation: 

#Replicate Function creates replicas of values around a particular sample. It basically combines similar values together as we could see in the histogram.










