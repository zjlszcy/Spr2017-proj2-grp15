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

####################################global#############################################################
library(DT)
library(plyr)
crimes <- read.csv("../data/OPE CSS Custom Data 2017-02-12 134856/Criminal_Offenses_On_campus.csv", header = T)
for(i in 6:19){
  as.numeric(crimes[,i])
} 
##calculate total num of crimes for every year
crimes$total <- rowSums(crimes[,-c(1:6)],na.rm = TRUE)
crimes.split <- split(crimes, f = as.factor(crimes$Survey.year))
year.sum <- NA
for(i in 1: length(crimes.split)){
  year.sum[i] <- sum(crimes.split[[i]]$total) 
} 

#calculate num of every type of crimes for every year
agg <- aggregate(crimes[,-c(1:6)], list(crimes$Survey.year), sum)




##read in all universities files
schools.list <- list.files(path = "../data/Ranking/", pattern = "*.csv")
schools.list <- paste("/Users/ouminamikun/Desktop/Columbia/Spring 2017/ADS/Spr2017-proj2-grp15/data/Ranking/", schools.list, sep = "")
schools <- lapply(schools.list, read.csv)

#calculate total crimes for each campus
for(i in 1:30){
  schools[[i]]$total <- rowSums(schools[[i]][,-c(1:6)], na.rm = T)
}


##calulate the num of every type of crime for each institution every year
d.schools <- ldply(schools)
agg1 <- aggregate(d.schools[,-c(1:6)], list(d.schools$Survey.year, d.schools$Institution.name), sum)
enrollment <- c(9181, 2209, 12587, 27589, 21679, 6298, 15865, 14769, 17858, 28791, 21372, 11319, 21554  ,8808, 6621, 16963,10907, 37565, 41845, 15097, 43625, 29135, 12179, 24806, 42453, 23732, 12686, 7788, 14348, 12336)
enrollment <- as.vector(sapply(enrollment, rep, 14))
agg1$institution.size <- enrollment
agg1$crime.rate <- round(agg1$total/agg1$institution.size,4)

#2014 data for every school
schools2014 <- agg1[agg1$Group.1 == "2014",]
t.schools2014 <- t(schools2014)[-1,]
colnames(t.schools2014) <- as.character(t.schools2014[1,])
t.schools2014 <- as.data.frame(t.schools2014[-1,])
#######################################################################################################
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

    
################################### Schools leaflet #############################################
    output$school_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>% 
        addMarkers(lng=new_campus[,2], lat=new_campus[,3], popup=as.character(new_campus[,1]))
    })
##################################################################################################
    
    ##### California  Map  #####
    output$camap <- renderPlot({
      DF<-ca_crime()
      myLocation <- 'california'
      myMap <- get_map(location=myLocation, source="google", maptype="roadmap", crop=FALSE,zoom=6)
      p <- ggmap(myMap, base_layer = ggplot(aes(x = lon, y = lat,color=value),data = DF)) + 
        geom_point(aes(x=lon,y=lat,level=value),data = DF,color="pink") + 
        stat_density2d(aes(fill=..level..), geom="polygon", alpha=0.2) 
      print(p)
    })  
    
   
    
    ###### Reactive data frame that will dynamically change based on year and states chosen ####  
    state_details <- reactive({
      
    })
    
    
    ###### User click on States to see details ######
    output$details <- renderInfoBox({
      
    })
    
    
    output$title1<-renderText(US_Crime_Visual)
  
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
    
    school.rows <- reactive( {agg1$Group.2 == input$show_schools})
    output$schoolTrend <- renderPlot({
      plot(x = c(2001:2014), 
           y = agg1$total[agg1$Group.2 == unique(input$show_schools)[1]], 
           type = "lines",
           xlab = "Year",
           ylab = "Total Crimes",
           col = "Red")
      lines(x = c(2001:2014), 
            y = agg1$total[agg1$Group.2 == unique(input$show_schools)[2]],
            col = "blue")
      legend("topright", col = c("red", "blue"), lty = c(1,1), 
             legend = c(unique(input$show_schools)[1], unique(input$show_schools)[2]))
      
      
    })
######################################################################################################    
})



  