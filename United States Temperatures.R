library(maps)
library(ggplot2)
library(dplyr)
library(tidyr)

#install.packages('maps')
#install.packages('dplyr')
#install.packages('tidyr')

path = getwd()
setwd(path)

US_Data = read.csv(file = "GlobalLandTemperaturesByState.csv", header = T, sep=",")

#Piping to filter out States and separate Date column
US_Data %>%
  filter(Country=="United States") %>%
  filter(State!="Hawaii" & State!="Alaska") %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) ->US_Data
  
#Uneeded Columns (Same value)
US_Data$Day <- NULL
US_Data$Country <- NULL

head(US_Data)

#Change State names to match state names in package
US_Data$State <- tolower(US_Data$State)

US_Data[US_Data$State == "georgia (state)",]$State = "georgia"

#Omit null values
US_Data = na.omit(US_Data)

colnames(US_Data)[5] <- "region"

#Filter for years that all states have
US_Data %>% 
  filter(Year>=1850) %>%
  filter(Year<=2012) %>% 
  group_by(Month) ->US_Data2

#Test map for Februaury 1850
US_Data2 %>% 
  filter(Year== 1850) %>%
  filter(Month==2) ->US_Data3

map_states <- map_data("state")
US_Data3_map <- merge(map_states, US_Data3, sort = FALSE, by="region")
US_Data3_map <- US_Data3_map[order(US_Data3_map$order), ]
ggplot(US_Data3_map, aes(x = long, y = lat, group = group, fill = AverageTemperature)) + geom_polygon() + ggtitle("February 1850")