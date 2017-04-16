library(lubridate) # Date Functions
library(dtplyr) #Data Frame and dplyr Functions
library(ggplot2) #Graphics creation Functions
library(readr)
#TO DO: Delete 2013 values from Years_Temp since not complete
#Import Data from csv, changing Date to string for manipulation and deleting Uncertainty column
TempData <- read_csv("~/IntroDataScienceDocs/TempData.csv", 
                     col_types = cols(Date = col_character(), 
                                      `Monthly AverageTemp Uncertainty` = col_skip()))
#View(TempData)
#Create working copy of data
Temp2 <- data.table(TempData)
#Create a subset with just the US Readings
Temp2 <- subset(Temp2, Country == "United States")
#Remove all the dashes in the date column
Temp2$Date <- gsub("-","",Temp2$Date)
#Remove all the slashes in the date column
Temp2$Date <- gsub("/","-",Temp2$Date)
#create a new column called Flag and fill w/1
Temp2$Flag <- 1
#Check every row in the data set to see if the date starts w/ 18, if so set flag to 0
Temp2$Flag [(substr (Temp2$Date, start = 1, stop = 2)== "18" )] <-0
#Check every row in the data set to see if the date starts w/ 17, if so set flag to 0
Temp2$Flag [(substr (Temp2$Date, start = 1, stop = 2)== "17" )] <-0
#Subset based on only dates after 12/31/1899
USTemps <- subset(Temp2, Flag == 1)

#Rename Monthly AverageTemp to get rid of spaces
colnames(USTemps)[colnames(USTemps)=="Monthly AverageTemp"] <- "CTemp"
#Create new column FTemps - Temp in Farenheit, calculate value and write
USTemps$FTemp <- (USTemps$CTemp *1.8) + 32
#Convert Date column from character to Date format (YYYY-mm-dd)
USTemps$Date <- as.Date(USTemps$Date,"%m-%d-%Y")
#Create Year column to group by for average Calculations
USTemps$Year <- year(USTemps$Date)
#Delete partial year values for 2013
USTemps <- subset(USTemps,Year != 2013)
#Group by Year
Years_Group <- group_by(USTemps,Year)

#Summarize by year and calculate average temp in Farenheit
Years_Temp <- summarize(Years_Group, sum(FTemp)/12)
#Write value as a stored colum
Years_Temp$FTemp <- Years_Temp$`sum(FTemp)/12`
#Print bar graph by year

ggplot(data = Years_Temp, aes(x=Year, y= FTemp)) +
  geom_bar(stat="identity", alpha=0.25) +
  guides(fill = FALSE) +
  xlab("Years") + ylab("Average US Temps F") +
  geom_line(colour="red", size=1.5)
#Another possible chart, I think this is better showing the changes
#Will work on smoothing this a bit, probably using a running average to smooth out annualize variations
ggplot(data=Years_Temp, aes(x=Year, y= FTemp)) + geom_line() 
