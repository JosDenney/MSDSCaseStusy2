library(lubridate) # Date Functions
library(dtplyr) #Data Frame and dplyr Functions
library(ggplot2) #Graphics creation Functions
CityTemp <- read_csv("C:/Users/jdenney/Downloads/CityTemp.csv",
                     col_types = cols(Date = col_character()))
#Create working copy of data
CityTemp2 <- data.table(CityTemp)
#Remove unneeded columns
CityTemp2[, c("Latitude","Longitude","Monthly AverageTemp Uncertainty","Country") := NULL]
#Remove all the dashes in the date column
CityTemp2$Date <- gsub("-","",CityTemp2$Date)
#Remove all the slashes in the date column
CityTemp2$Date <- gsub("/","-",CityTemp2$Date)
#create a new column called Flag and fill w/1
CityTemp2$Flag <- 1
#Check every row in the data set to see if the date starts w/ 18, if so set flag to 0
CityTemp2$Flag [(substr (CityTemp2$Date, start = 1, stop = 2)== "18" )] <-0
#Check every row in the data set to see if the date starts w/ 17, if so set flag to 0
CityTemp2$Flag [(substr (CityTemp2$Date, start = 1, stop = 2)== "17" )] <-0
#Subset based on only dates after 12/31/1899
City_Temp_Sub <- subset(CityTemp2, Flag == 1)
#Convert Date from string to date using lubridate function
City_Temp_Sub$Date <- mdy(City_Temp_Sub$Date)
#Create new output data set and make into data.table
City_Temp_Output <- data.table(City_Temp_Sub)
#Rename variable from Monthly AverageTemp to AvgTemp
colnames(City_Temp_Output)[colnames(City_Temp_Output)=="Monthly AverageTemp"] <- "AvgTemp"
#Calculate the min and max temps for each city using data.frame syntax
City_Temp_Output <- City_Temp_Output[,c(min=.SD[which.min(AvgTemp)],max=.SD[which.max(AvgTemp)]),by=City]
#Create diff variable and then assign value max-min to it
City_Temp_Output$Diff <- City_Temp_Output$max.AvgTemp - City_Temp_Output$min.AvgTemp
#Order the temps in ascending order
City_Temp_Viz <- City_Temp_Output[order(-Diff)]
#take the top 20 deltas
City_Temp_Viz <- City_Temp_Viz[1:20]
#Print bar graph, sorted largest to smallest
ggplot(City_Temp_Viz, aes(x= reorder(City,-Diff), y= Diff, fill = City))+
  geom_bar(colour="black", position = position_dodge(), stat = "identity") +
  theme_minimal()
ggplot(data = City_Temp_Viz, aes(x= reorder(City,-Diff), y=Diff, fill = City)) +
   geom_bar(stat="identity") +
   guides(fill = FALSE) +
   xlab("Cities") + ylab("Largest Temp Deltas Degrees C") 

   

