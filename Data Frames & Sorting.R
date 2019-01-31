#IST687-Doing some initial data analysis-HW2
#Rahul Rathod 
#Due date- 09/12/2018
#Submitted Date-09/12/2018
data()

#Step A: Initialize an ‘arrests’ dataframe

#Copy USArrests into a new variable (called ‘arrests’).  The USarrests data should be right in your R datasets.
arrests<- USArrests#USArrests copied to arrests
View(arrests)#display arrests
str(arrests)
summary(arrests)

#Step B: Explore the assault rate

#Write a comment: Is a higher or lower assault rate best?
#Lower Assaukt is better

#Which state has the best assault rate? 
min.arrests.index<-arrests[which.min(arrests$Assault),]#Minimum assault rate assgined to min.arrest.index
min.arrests.index#To Run Command

#Step C: Explore the murder rate 

#Which state has the highest murder rate?
High.Murder.State<- arrests[which.max(arrests$Murder),]#Highest murder rate assigned to variable Highe.Murder.State
High.Murder.State#To run command

#Create a sorted dataframe, based on descending murder rate
Sort.arrests<- arrests[order(-arrests$Murder),]#sorted frame assgined to variable Sort.arrests
View(Sort.arrests)#To display the sorted frome

#Show the 10 states with the highest murder rate
Sort.arrests[1:10,]#displaying 10 states with highest murder rate

#What is the value of the 20th row, third column (in the sorted dataframe)? Use R code (not visual inspection)
Sort.arrests[20,3]#Displaying the value of 20th row, 3rd column

#Step D: Which state is the least safe? Explain your logic

#Write the R code to determine your answer
Total<- arrests$Murder + arrests$Assault + arrests$Rape#SToring the Addition of Murder,Assault and Rape of each state in Variable Total
Total#Display Total
New_arrests<-cbind(arrests,Total)#Adding Column Total to arrest data set and sotring the new dataset in New_arrest
New_arrests#Display New_arrests
New_arrests[which.max(New_arrests$Total),]#Finding the Maximum Total which corresponds to the least safe state
