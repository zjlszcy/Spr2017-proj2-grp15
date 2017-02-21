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
state_crimes <- read.csv("CrimeStatebyState-clean.csv",header=T)
state_crimes <- subset(na.omit(state_crimes), 
                       !(State %in% c("United States", "District of Columbia")))
state_crimes <- na.omit(state_crimes[,-7])

## California crime ## 
ca_crime_data <- read.csv("crimes_data_2006-2015.csv",header = T)
ca_crime_data$sum <- rowSums(ca_crime_data[,-c(1:3)])
ca_agg <- aggregate(ca_crime_data$sum, by = list(ca_crime_data$Year, ca_crime_data$County), sum)

##### Change the colnames #####
state_crimes$Crimesper <- state_crimes$total.number/state_crimes$Population*100000
colnames(state_crimes)<-c("Year","State","Population","Violent Crime Total","Murder", "Rape", "Robbery","Assault","Property Crime Total","Burglary","Larceny Theft","Motel Vehicle Theft","Total Number Of Crime","# Crimes per 10000 people")
colnames(ca_agg)<-c("Year","region","value")

##### Get the longitutide and latitude #####
coordinate_ca <- geocode(as.character(ca_agg$region))
ca_agg <- cbind(ca_agg,coordinate_ca)
na.index <- which(is.na(ca_agg[,4]))

##### Reording Data #####
state_crime <- state_crimes[order(state_crimes$State,state_crimes$Year),]
US_Crime_Visual <- "US Crime Data Visualization"


###########################Shiny Server############################
shinyServer(function(input, output) {
  
  #### Reactive data frame that will dynamically change based on year chosen(for state) ####
    crime_data <- reactive({
      #Subset the data frame based on year chosen
      if(!is.null(input$Year)){
          state_crime_data <- subset(state_crime, Year == as.character(input$Year))
     }
     return(state_crime_data)
   })
  
    #### Reactive data frame that will dynamically change based on year chosen(for california) ####
    ca_crime <- reactive({
      #Subset the data frame based on year your choose
      if(!is.null(input$YEAR)){
        ca_crime_county_data <- subset(ca_agg, Year == as.character(input$YEAR))
        ca_crime_county_data <- ca_crime_county_data[,2:3]
      }
      return(ca_crime_county_data)
    })
    
  ##### US state total number of crimes Map #####
    output$map <- renderGvis({
      DF<-crime_data()
      gvisGeoChart(DF, locationvar = "State", colorvar ="# Crimes per 10000 people",
                   options=list(region="US", 
                                title='State Crime',
                                resolution="provinces", 
                                width='1000px',height='600px',
                                backgroundColor='#F6E3CE'))
   
    })   

    
    
  ###### infoBoxes for each year user choose ######
    output$maxbox <- renderInfoBox(
      {
        DF <- crime_data()
        outData <- DF[,"# Crimes per 10000 people"]
        max_value <- max(outData,na.rm = T)
        max_state <- DF$State[outData==max_value]
        infoBox(max_state,round(max_value),icon = icon("chevron-up"),color='blue')
      }
    )
    output$minbox <- renderInfoBox(
      {
        DF <- crime_data()
        outData <- DF[,"# Crimes per 10000 people"]
        min_value <- min(outData,na.rm = T)
        min_state <- DF$State[outData==min_value]
        infoBox(min_state,round(min_value),icon = icon("chevron-down"),color='yellow')
      }
    )
    output$medbox <- renderInfoBox(
      {
        DF <- crime_data()
        outData <- DF[,"# Crimes per 10000 people"]
        med_value <- median(outData,na.rm = T)
        med_state <- DF$State[outData==med_value]
        infoBox(paste("Median",input$Year),round(med_value),icon = icon("calendar"),color='red')
      }
    )
 
    
    ##### California  Map  #####
    output$camap <- renderGvis({
      DF<-ca_crime()
      county_choropleth(DF,
                        title="aa",
                        legend = "Total number of Crimes",
                        state_zoom = "california",
                        reference_map = F)
    })  
    
    
    
    
    ###### Reactive data frame that will dynamically change based on year and states chosen ####  
    state_details <- reactive({
      
    })
    
    
    ###### User click on States to see details ######
    output$details <- renderInfoBox({
      
    })
    
    output$title1<-renderText(US_Crime_Visual)
  
})



ggplot(ca_agg, aes(map_id = region)) +
geom_map(aes(fill = value), map= ca_agg, color ="black") +
  expand_limits(x = ca_agg$long, y = ca_agg$lat) +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  scale_fill_gradient(low="white", high="blue") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + facet_wrap(~P)


  