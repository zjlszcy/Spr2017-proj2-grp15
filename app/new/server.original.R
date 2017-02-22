library(shiny)
library(reshape2)
library("googleVis")
library("gridExtra")
library("ggplot2")
library(shinydashboard)
library(DT)
library(dplyr)
library(ggmap)
library(ggplot2)
library(maps)
library(flexdashboard)

# load data 
state_crimes <- read.csv("/Users/ouminamikun/Desktop/Columbia/Spring 2017/ADS/Spr2017-proj2-grp15/data/CrimeStatebyState-clean.csv",header=T)
state_crimes <- subset(na.omit(state_crimes), 
                       !(State %in% c("United States", "District of Columbia")))
state_crimes <- na.omit(state_crimes[,-7])
# Change the colnames 
colnames(state_crimes)<-c("Year","State","Population","Violent Crime Total","Murder", "Rape", "Robbery","Assault","Property Crime Total","Burglary","Larceny Theft","Motel Vehicle Theft","Total Number Of Crime")
choice <- c(names(state_crimes))
# Reording Data 
state_crime <- state_crimes[order(state_crimes$State,state_crimes$Year),]
heatYChoices<-c('Year','Type')
US_Crime_Visual <- "US Crime Data Visualization"

shinyServer(function(input, output) {
  
  
  #### Reactive data frame that will dynamically change based on year chosen ####
    crime_data <- reactive({
      #Subset the data frame based on year chosen
      if(!is.null(input$Year)){
          state_crime_data <- subset(state_crime, Year == as.character(input$Year))
     }
     return(state_crime_data)
   })
  
  ##### US state total number of crimes Map #####
    output$map <- renderGvis({
      DF<-crime_data()
      gvisGeoChart(DF, locationvar = "State", colorvar ="Total Number Of Crime",
                   options=list(region="US", 
                                title='State Crime',
                                resolution="provinces", 
                                width='1000px',height='600px',
                                backgroundColor='#F6E3CE'))
   
    })   
    
  ###### infoBoxes for each year user choose ######
    output$maxbox <- renderInfoBox(
      {
        # XChoice = input$selected
        DF <- crime_data()
        outData <- DF[,"Total Number Of Crime"]
        max_value <- max(outData,na.rm = T)
        max_state <- DF$State[outData==max_value]
        infoBox(max_state,max_value,icon = icon("chevron-up"),color='blue')
      }
    )
    output$minbox <- renderInfoBox(
      {
        # XChoice = input$selected
        DF <- crime_data()
        outData <- DF[,"Total Number Of Crime"]
        min_value <- min(outData,na.rm = T)
        min_state <- DF$State[outData==min_value]
        infoBox(min_state,min_value,icon = icon("chevron-down"),color='yellow')
      }
    )
    output$medbox <- renderInfoBox(
      {
        # XChoice = input$selected
        DF <- crime_data()
        outData <- DF[,"Total Number Of Crime"]
        med_value <- median(outData,na.rm = T)
        med_state <- DF$State[outData==med_value]
        infoBox(paste("Median",input$Year),med_state,med_value,icon = icon("calendar"),color='red')
      }
    )
 
    output$title1<-renderText(US_Crime_Visual)
  
})
  