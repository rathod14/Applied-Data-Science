# IST687 – Visualize Median Income on a Map
#Rahul Rathod 
#Due date- 10/17/2018
#Submitted Date-10/17/2018

#Step A: Load and Merge datasets

#1)	Read in the census and the USArrests datasets and merge them. 
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

str(states) #view structure
arrests<- USArrests #USArrests copied to arrests
View(arrests) #display arrests

arrests$stateName<- row.names(arrests)
View(arrests)
mergeDF<- merge(states, arrests, by = "stateName") # merging two dataframes by statename
View(mergeDF) #view merged datafram

install.packages("maps") #install package maps
library(maps)
install.packages("ggplot2") #install package ggplot2
library(ggplot2)
install.packages("ggmap") ##install package ggmap
library(ggmap)
#2)	 Create a new Data frame that has the area of each state (state.area), and the center of each state (state.center), and then merge (by stateName) it with your final data frame in step #1. 
stateName<- state.name #storing state.name in stateName
area<- state.area #storing state.area in area
center<- state.center #storing state.centre in centre
dataframe<- data.frame(stateName,area,center) #create new data frame with columns stateName, area and centre
View(dataframe) #view new datafram
mergeDF<- merge(mergeDF, dataframe, by="stateName") #merge datafrome with mergeDF by stateName
View(mergeDF) #viewing the new mergeDF

mergeDF$stateName= tolower(mergeDF$stateName) #converting stateName to lower case
us<-map_data("state") 

#Step B: Generate a color coded map

#3)	Create a color coded map, based on the area of the state 
#creating color coded map based on the area of the state
map.simple<- ggplot(mergeDF, aes(map_id=stateName))
map.simple<- map.simple + geom_map(map=us, aes(fill=mergeDF$area))
map.simple<- map.simple +expand_limits(x= us$long, y= us$lat)
map.simple
map.simple<-map.simple +coord_map()+ ggtitle("basic map of USA")
map.simple
#Step C: Create a color shaded map of the U.S. based on the Murder rate for each state

#4)	Repeat step B, but color code the map based on the murder rate of each state.
#creating color coded map based on the murder rate
map.Murder<- ggplot(mergeDF, aes(map_id=stateName))
map.Murder<- map.Murder + geom_map(map=us, aes(fill=mergeDF$Murder))
map.Murder<- map.Murder +expand_limits(x= us$long, y= us$lat)
map.Murder
map.Murder<-map.Murder +coord_map()+ ggtitle("basic map of USA")
map.Murder
#darkest part is the highest murder rate and lightest is the lowest murder rate

#5)	 Show the population as a circle per state (the larger the population, the larger the circle), using the location defined by the center of each state
#create a color coded map and show the population as a cricel per state
map.point<- ggplot(mergeDF, aes(map_id=stateName))
map.point<- map.point + geom_map(map=us, aes(fill=mergeDF$Murder))
map.point<- map.point +expand_limits(x= us$long, y= us$lat)
map.point<- map.point + geom_point(aes(x = x, y = y, size=population))
map.point<-map.point +coord_map()+ ggtitle("basic map of USA")
map.point

#Step D: Zoom the map

#6)	Repeat step C, but only show the states in the north east
#get lat and lon of NYC
latlon<- geocode(source= "dsk", "nyc, newyork, ny")
latlon
#xlim and ylim helps to zoom to -10 and +10 of the plotted corodinates
map.point<- ggplot(mergeDF, aes(map_id=stateName))
map.point<- map.point + geom_map(map=us, aes(fill=mergeDF$Murder))
map.point<- map.point +geom_point(data=mergeDF, aes(x= mergeDF$x, y= mergeDF$y,size=mergeDF$population,color=mergeDF$UrbanPop))
map.point <- map.point + xlim(latlon$lon-10,latlon$lon+10)  + ylim(latlon$lat-10,latlon$lat+10)
map.point<-map.point +coord_map()+ ggtitle("basic map of USA")
map.point
