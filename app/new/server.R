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
library(googleVis)
library(RColorBrewer)

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

########################################campus_crime_map################################################
## Total Crimes on Campus in U.S.
crimes <- read.csv("OPE CSS Custom Data 2017-02-12 134856/Criminal_Offenses_On_campus.csv")

## Get Unique Campus Names
length(unique(crimes$Institution.name))
# campus_1 <- as.character(unique(crimes$Institution.name))[1:960] # chenyun
# campus_2 <- as.character(unique(crimes$Institution.name))[961:1921] # nanjun
# campus_3 <- as.character(unique(crimes$Institution.name))[1922:2884] # yuxin

## To run the following code takes a long time so we run seperately and save it to a file named "All_Coordinate"
# coordinate_1 <- geocode(campus_1) 
# coordinate_2 <- geocode(campus_2) 
# coordinate_3 <- geocode(campus_3) 
coordinate <- read.csv("All_Coordinate.csv")
coordinate <- coordinate[,-1]

campus <- unique(crimes$Institution.name)
campus <- cbind(campus, coordinate)

## Check the missing value and search for the longitude & latitude manually
# na.index <- which(is.na(campus[,2]))
# campus[31, 2:3] <- geocode("alfred university")
# campus[43, 2:3] <- geocode("American Baptist College")
# campus[178, 2:3] <- c(-85.1892, 34.2904)
# campus[194, 2:3] <- geocode(as.character(campus[194,1]))
# campus[253, 2:3] <- geocode(as.character(campus[253,1]))

# na.index <- which(is.na(campus[,2]))

# for (i in na.index){
# campus[i, 2:3] <- geocode(as.character(campus[i,1]))
# }

# which(is.na(campus[,2]))

# campus[924, 2:3] <- c(-81.3626, 28.3025)
# campus[2376, 2:3] <- c(-77.6493, 39.9492)

## Delete the campus we cannot even find the location manually
campus <- campus[-c(710,945,1121,1340,1531,2797),]

## Check NA value
sum(is.na(campus[,2:3]))

## Update dataframe crimes as well
unselect <- unique(crimes$Institution.name)[c(710,945,1121,1340,1531,2797)]
new_crimes <- crimes[! crimes$Institution.name %in% unselect,]

# write.csv(coordinate_3,file="test3.csv")
# which(is.na(coordinate)) some of them are NA


## Get U.S. Map
# map <- get_map(location = "united states", zoom = 4, maptype = "toner",source = "stamen")

## Plot the All researched Campus Locations
# campus_map = ggmap(map, base_layer = ggplot(aes(x = lon, y = lat), data = campus)) + geom_point(color = 2, alpha = 1)
# campus_map

## Total crimes for each campus
new_crimes$sub_total <- rowSums(new_crimes[, 7:19], na.rm = T)

## Remove campus that have no reports of crimes all these year
all_year_ttl <- aggregate(new_crimes$sub_total, by = list(new_crimes$Institution.name), FUN = sum)
colnames(all_year_ttl) <- c("Institution.name", "Total")
ttl_zero <- as.vector(all_year_ttl$Institution.name[which(all_year_ttl$Total == 0)])
new_all_year_ttl <- all_year_ttl[! all_year_ttl$Institution.name %in% ttl_zero,]
new_campus <- campus[campus$campus %in% new_all_year_ttl$Institution.name,]
final_crimes <- new_crimes[new_crimes$Institution.name %in% new_all_year_ttl$Institution.name,]
table(new_all_year_ttl$Institution.name %in% new_campus$campus)

## Sort the data frames by alphabetic order of campus name
new_all_year_ttl <- new_all_year_ttl[order(new_all_year_ttl$Institution.name), ]
new_campus <- new_campus[order(new_campus$campus),]
all.equal(new_all_year_ttl$Institution.name, new_campus$campus)

## All year total crime for each campus 
new_campus$total <- new_all_year_ttl$Total

## Append institution size
size <- aggregate(final_crimes$sub_total, by = list(final_crimes$Institution.name, final_crimes$Institution.Size), FUN = sum)
order_size <- size[order(size$Group.1),]
names(order_size) <- c("insti.name", "size")
final_size <- aggregate(order_size$size, by = list(order_size$insti.name), FUN = sum)
all.equal(final_size$Group.1, new_campus$campus)
new_campus$pop <- final_size$x
new_campus$rate <- new_campus$total * 1000/ new_campus$pop # Every 1000 people

## Removed St Andrews University and Hebrew Union College-Jewish Institute of Religion(NOT IN US)
ind1 <- which(new_campus$campus == "Hebrew Union College-Jewish Institute of Religion" | new_campus$campus == "St Andrews University")
ind2 <- which(final_crimes$Institution.name == "Hebrew Union College-Jewish Institute of Religion" | final_crimes$Institution.name == "St Andrews University")
new_campus <- new_campus[-ind1,]
final_crimes <- final_crimes[-ind2,]

## Aggregate crimes for each year each school
each_year <- aggregate(final_crimes$sub_total, by = list(final_crimes$Survey.year, final_crimes$Institution.name), FUN = sum)
names(each_year) <- c("Year", "Name", "Total")

## Combine each_year and new_campus
combine <- merge(each_year, new_campus, by.x = "Name", by.y = "campus")
map1 <- get_map(location = "united states", zoom = 4, , maptype = "toner",source = "stamen")

######################################################################################################
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
    campus_data <- reactive({
      subset(combine, Year == input$YEAR)
    })
    
    output$campus_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>% 
        addMarkers(clusterOptions = markerClusterOptions(), lng=new_campus[,2], lat=new_campus[,3], 
                   popup=paste(new_campus[,1], new_campus$rate))
    })
    
    output$map <- renderPlot({
      campus <- campus_data()
      myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
      sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 5))
      p <- ggmap(map1, base_layer = ggplot(campus, aes(x = lon, y = lat, colour = rate))) + geom_point(aes(x=lon, y=lat, colour = rate), alpha=1) + sc
      print(p)
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



  