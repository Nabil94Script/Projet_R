#### Load the required packages ####
# if packages are not installed already,
# install them using function install.packages(" ")

library(shiny) # shiny features
library(shinydashboard) # shinydashboard functions
library(DT)  # for DT tables
library(dplyr)  # for pipe operator & data manipulations
library(plotly) # for data visualization and plots using plotly 
library(ggplot2) # for data visualization & plots using ggplot2
library(ggtext) # beautifying text on top of ggplot
library(maps) # for USA states map - boundaries used by ggplot for mapping
library(ggcorrplot) # for correlation plot
library(shinycssloaders) # to add a loader while graph is populating


library(data.table)
library(skimr)
library(tidyr)
library(rAmCharts)
library(stringi)
library(scales)
library(zoo)
library(ggpubr)
library(TTR)
library(gridExtra) # to organise plots nicely 
library(fpp2)


library(modeltime)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(timetk)

#### Dataset Manipulation ####
# USArrests dataset comes along with base R
# you can view the data by simply
# USArrests  # uncomment if running this

## create a states object from rownames 
states = rownames(USArrests)

## Add a new column variable state into the dataset. This will be used later to merge the dataset with US states map data
my_data= USArrests %>% mutate(State = states) 

df=fread("N:/Echanges/Projets R/Time Series/temp_train.csv")


dff=tibble(df)

dff$Date=paste0(stringi::stri_sub(dff$datetime, 7, 10),"-",stringi::stri_sub(dff$datetime, 4, 5),"-",stringi::stri_sub(dff$datetime, 1, 2))
dff$DateC=as.POSIXct(dff$Date)

dff$Temperator=gsub(c(","),".",as.character(dff$T_mu))
dff$Temperator=as.numeric(dff$Temperator)

dff$Ventilator_throughput=gsub(c(","),".",as.character(dff$debit_Ventulation))
dff$Ventilator_throughput=as.numeric(dff$Ventilator_throughput)

dff=dff %>% select("equipement","Date","DateC","Temperator","Ventilator_throughput")

#df2=dff %>% subset(equipement=='Baie_0002')
#df1=dff %>% subset(equipement=='Baie_0001')
#df3=dff %>% subset(equipement=='Baie_0003')



mydata=tibble(dff)


# Column names without state. This will be used in the selectinput for choices in the shinydashboard
#c1 = my_data %>% select(-"State") %>% names(
c0 = mydata %>% select("equipement") %>% group_by(equipement) %>% slice(1)

#c0 =c('Baie_0002','Baie_0001','Baie_0003')

c1 = mydata %>% select("Temperator","Ventilator_throughput") %>% names()

# Column names without state and UrbanPopulation. This will be used in the selectinput for choices in the shinydashboard
c2 = mydata %>% select("Temperator") %>% names()
c3 = mydata %>% select("Ventilator_throughput") %>% names()

c4 = c(3,6,12,15,18,24,36)

####   Preparing data for Arrests Map   ####
# map data for US states boundaries using the maps package
# map_data from ggplot package
# map_data() converts data fom maps package into a dataframe which can be further used for mapping

state_map = map_data("state") # state from maps package contains information required to create the US state boundaries
# state_map %>% str() # you can see that state_map has a region column. region column has US state names but in lower case


# convert state to lower case
my_data1 = my_data %>% mutate(State = tolower(State))  # converting the state names from USArrests dataset to lower case so we can later merge the maps data to our dataset


## Add the latitude, longitude and other info needed to draw the ploygon for the state map
# For the state boundaries available - add the USAArrests info.
# Note that Alaska and Hawaii boundaries are not available, those rows will be omitted in the merged data
# right_join from dplyr package
merged =right_join(my_data1, state_map,  by=c("State" = "region"))

# Add State Abreviations and center locations of each states. Create a dataframe out of it
st = data.frame(abb = state.abb, stname=tolower(state.name), x=state.center$x, y=state.center$y)

# Join the state abbreviations and center location to the dataset for each of the observations in the merged dataset
# left_join from dplyr package
# there is no abbreviation available for District of Columbia and hence those rows will be dropped in the outcome
new_join = left_join(merged, st, by=c("State" = "stname"))





df=tibble(df1)


 #IT SEEMS THAT THIS CREATES THE PROBLEM  





