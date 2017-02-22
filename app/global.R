library(shiny)
library(reshape2)
library("googleVis")
library("gridExtra")
library("ggplot2")
library(shinydashboard)
library(DT)
library(reshape2)
library(corrplot)
library(dplyr)
library(ggmap)
library(ggplot2)
library(maps)
library(flexdashboard)
library(leaflet)

##### load data #####
## US State Crime Data ##
state_crimes <- read.csv("/Users/ouminamikun/Desktop/Columbia/Spring 2017/ADS/Spr2017-proj2-grp15/data/CrimeStatebyState-clean.csv",header=T)
state_crimes <- subset(na.omit(state_crimes), 
                       !(State %in% c("United States", "District of Columbia")))
state_crimes <- na.omit(state_crimes[,-7])

## California crime ## 
ca_crime_data <- read.csv("/Users/ouminamikun/Desktop/Columbia/Spring 2017/ADS/Spr2017-proj2-grp15/data/crimes_data_2006-2015.csv",header = T)
ca_crime_data$sum <- rowSums(ca_crime_data[,-c(1:3)])
ca_agg <- aggregate(ca_crime_data$sum, by = list(ca_crime_data$Year, ca_crime_data$County), sum)

##### Change the colnames #####
state_crimes$Crimesper <- state_crimes$total.number/state_crimes$Population*100000
colnames(state_crimes)<-c("Year","State","Population","Violent Crime Total","Murder", "Rape", "Robbery","Assault","Property Crime Total","Burglary","Larceny Theft","Motel Vehicle Theft","Total Number Of Crime","# Crimes per 10000 people")
colnames(ca_agg)<-c("Year","region","value")

##### Get the longitutide and latitude #####
# coordinate_ca <- geocode(as.character(ca_agg$region))
#write.csv(coordinate_ca,file="coordinaca.csv")
#coordinate_ca_all <- read.csv("coordinaca.csv",header=T)
#coordinate_ca_all <- coordinate_ca_all[,-1]
#ca_agg <- cbind(ca_agg,coordinate_ca_all)

# Check NA values 
#na.index <- which(is.na(ca_agg[,4]))
#for (i in na.index){
#  ca_agg[i, 4:5] <- c(-117.3961,33.95330)
#}
#ca_agg[331,4:5] <- ca_agg[332,4:5]

##### Reording Data #####
state_crime <- state_crimes[order(state_crimes$State,state_crimes$Year),]
US_Crime_Visual <- "US Crime Data Visualization"

