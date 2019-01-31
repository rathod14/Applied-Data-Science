# HW 1: Basic R Coding (Vectors,Conditionals) 
#Submitted By Rahul Rathod SUID: 303187206 On September 05, 2018
#IST 687: Introduction to Data Science

#Step A: Create A Vector  

# 1.Define a vector ‘grades’, which contains the numbers 4.0, 3.3 and 3.7 
# Answer:

grades <- c(4.0, 3.3, 3.7) #Vector 'grades'
grades #To Run Command

# 2.Define A Vector 'courseName', Which Contain The Strings 'Bio', 'Math', 'History'. 
#Answer:

courseName <- c("Bio", "Math", "History") #Vector 'courseName'
courseName #To Run Command

# 3.Define A Variable 'BetterThanB', That Is Equal To 3
#Answer:

BetterThanB <- 3 #Variable BetterThanB Assigned Value 3
BetterThanB #To Run Command

#Step B: Calculating Statistics Using R  

#4. Compute The Average Of The Grades Vector With The mean() Function.
#Answer

mean(grades) #Inbuilt mean() Function To Calculate The Average Of A Vector

# 5.Calculate The Number Of Observations In The grades Vector With The length() Function.
#And Store The Result In The Variable 'total.length'.
#Output The Value Of 'total.length'.
#Answer:

length(grades) #Calculates The Number Of Values In grades Vector
total.length <- length(grades) #Copyin The Value In length(grades) To total.length Variable
total.length #Display The Value


#6. Calculate The Sum Of  'grades' With The sum() Function.
#Store The Result In 'total'.
#Answer:

sum(grades) #Calculates The Sum Of All Values In grades Vector
total <- sum(grades) #Storing The Answer In A Variable 'total'

#7. Recompute The Average Of All The Grades By Combining Questions 5 And 7.
#Answer:

mean(total/total.length) 

#Step C: Using The max/min Functions In R.

#8. Compute The Max Grades.
#Store The Result In 'maxG'.
#Answer:

max(grades) #Inbuilt Function In R Which Calculates The Maximum Value In A Vector
maxG <- max(grades) #Copying The Value Into A Variable 'maxG'

#9. Compute The Min Grades.
#Store The Results In 'minG'.
#Answer:

min(grades) #Inbuilt Function In R Which Calculates The Minimum Value In A Vector
minG <- min(grades) #Copying The Value Into A Variable 'minG'

#Step D: Vector Math.
#10. Create A New Vector Called betterGrades,Which Is The 'grades + 0.3'
#Answer:

betterGrades <- grades + 0.3 #Each Grade Improved Each Grade By 0.3 Points
betterGrades #Display New Updated Grades

#11. Compute The Average Of betterGrades.
#Answer:

mean(betterGrades) #Calulates The Average of Values In The Vector 'betterGrades'

#Step E: Using Conditional If Statements.

#12. Test If maxG Is Greater Than 3.5.
#Answer:

if(maxG > 3.5) "yes" else "no" #Conditional Statement

#13. Test If minG Is Greater Than The Variable 'BetterThanB'.
#Answer:

if(minG > BetterThanB) "yes" else "no" #Conditional Statement

#Step F: Accessing An Element In A Vector.

#14. Output The Name Of The Second Clas in the 'courseName' Vector.
#Answer:

courseName[2] #Returns The Value At A Particular Index In A Vector 












