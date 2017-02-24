library(shiny)
library(reshape2)
library("googleVis")
library("gridExtra")
library("ggplot2")
library(shinydashboard)
library(DT)
library(reshape2)
library(corrplot)
library(plyr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(maps)
library(flexdashboard)
library(leaflet)
library(googleVis)
library(RColorBrewer)
library(plotly)

if(! require(plotly))
{
  install.packages("plotly")
}
if(! require(leaflet))
{
  install.packages("leaflet")
}

require(plotly)
require(leaflet)
 
load("workspace1.RData")

shinyServer(function(input, output) {
  
  #### Reactive data frame that will dynamically change based on year chosen(for state) ####
    crime_data <- reactive({
      #Subset the data frame based on year chosen
      if(!is.null(input$Year)){
          state_crime_data <- subset(state_crime, Year == as.character(input$Year))
     }
     return(state_crime_data)
   })
  

    
  ##### US state total number of crimes Map #####
    output$stmap <- renderPlotly({
      DF<-crime_data()
      DF$hover <- with(DF, paste(State, '<br>',"Crimes: ", round(value), "<br>"))
      # give state boundaries a white border
      l <- list(color = toRGB("white"), width = 2)
      # specify some map projection/options
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      )
      
      plot_geo(DF, locationmode = 'USA-states') %>%
        add_trace(z = ~value, 
                  text = ~hover, 
                  locations = ~code,
                  color = ~value, 
                  colors = 'Blues') %>%
        colorbar(title = "Total Crimes per 100000 people") %>%
        layout(geo = g)
      
    })   

    
  ###### infoBoxes for each year user choose ######
    output$maxbox <- renderInfoBox(
      {
        DF <- crime_data()
        outData <- DF[,"value"]
        max_value <- max(outData,na.rm = T)
        max_state <- DF$State[outData==max_value]
        infoBox(max_state,round(max_value),icon = icon("chevron-up"),color='blue')
      }
    )
    output$minbox <- renderInfoBox(
      {
        DF <- crime_data()
        outData <- DF[,"value"]
        min_value <- min(outData,na.rm = T)
        min_state <- DF$State[outData==min_value]
        infoBox(min_state,round(min_value),icon = icon("chevron-down"),color='green')
      }
    )
    output$medbox <- renderInfoBox(
      {
        DF <- crime_data()
        outData <- DF[,"value"]
        med_value <- median(outData,na.rm = T)
        med_state <- DF$State[outData==med_value]
        infoBox(paste("Median",input$Year),round(med_value),icon = icon("calendar"),color='yellow')
      }
    )

    
################################### Schools leaflet #############################################

    campus_data <- reactive({
      subset(combine, Year == input$YEAR)
    })
    
    output$campus_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>% 
        addMarkers(clusterOptions = markerClusterOptions(), lng=new_campus[,2], lat=new_campus[,3], 
                   popup= paste(new_campus$campus,"<br>","Crime Rate (Per 1000 People):", new_campus$rate))
    })
    
    

      output$school_map <- renderLeaflet({
        leaflet(new_campus) %>%
          addTiles() %>% 
          addMarkers(lng=new_campus[,2], lat=new_campus[,3], 
                     popup=paste(as.character(new_campus[,1]),
                                 as.character(new_campus[,7]), sep = ", "),
                     clusterOptions = markerClusterOptions(freezeAtZoom = 10))
      })

##################################################################################################
    
   
    
   
  
##########################################Schools Comparasion#####################################
    school <- t.schools2014
    schools.selected <- reactive({school[, input$show_schools, drop = FALSE]})
    
    
    output$criminal.offenses <- DT::renderDataTable({
      DT::datatable(schools.selected(), 
                    extensions = 'Buttons', 
                    options = list(pageLength = 25, dom = 'Bfrtip', buttons = I('colvis')),
                    class = "display")
    })
    
    
    output$rate.bar <- renderPlot({
      barplot(as.matrix(schools.selected()[dim(schools.selected())[1],]), 
              col = "pink", xlab = "Schools", ylab = "Crime Rate")
    })
    
    school.names <- reactive( {unique(input$show_schools)})
    output$schoolTrend <- renderPlot({
      xrange <- range(agg1$Group.1) 
      yrange <- c(0,500)
      plot(xrange, yrange, type="n", xlab="Years",ylab="Total Crimes")
      for(i in 1:length(school.names())){
        d <- subset( agg1, agg1$Group.2 == school.names()[i] )
        lines(d$Group.1, d$total, col = i)
        legend("topright", col = c(1:length(school.names())),
               legend = school.names(), lty = rep(1,length(school.names())))
      }
      
    })
######################################################################################################    
})



  