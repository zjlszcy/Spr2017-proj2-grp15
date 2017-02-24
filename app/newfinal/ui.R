load("workspace1.RData")
library(shinydashboard)
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
if(! require(maptools))
{
  install.packages("maptools")
}
require(plotly)
require(leaflet)
require(maptools)

shinyUI(dashboardPage(
  # Application title
  dashboardHeader(title = "US Campus Crime App"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("US Crime Map", tabName = "crime_map", icon = icon("map-marker"), badgeColor='light-blue'),
      menuItem("US Campus Crime", tabName = "dashboard", icon = icon("university")),
      menuItem("Comparison", tabName = "comparasion", icon = icon("graduation-cap"))
    )
  ),
 
   # Body 
  dashboardBody(
    tabItems(
      # tab-item one: state crime map
      tabItem(tabName = "crime_map",
              fluidRow(box(htmlOutput("title1"),width=12,background='light-blue')),
        
              fluidRow(infoBoxOutput("maxbox"),
                       infoBoxOutput("medbox"),
                       infoBoxOutput("minbox")),
              fluidRow(plotlyOutput("stmap"), title='US Crime Colored Map'),
              fluidRow(
                column(10,
                       sliderInput("Year",
                                   label = "Choose year:",min=2001,max=2014, value=2008, animate=T)
                )
              )
      ),
  
      
#################tab-item two: Comparasion ##########################################################      
      tabItem(tabName = "comparasion",
              fluidPage(
                titlePanel( 'Compare Data for Multiple Schools'),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput(inputId = 'show_schools', 
                                       label = 'Schools to compare:',
                                       choices = as.character(colnames(t.schools2014)), 
                                       selected = as.character(colnames(t.schools2014)[c(1,2)]), 
                                       width = "200px"
                    )
                  ),
                  mainPanel(
                    tabsetPanel(
                      id = 'criminal.offenses',
                      tabPanel('Criminal Offenses', 
                               DT::dataTableOutput('criminal.offenses'),
                               plotOutput("rate.bar"),
                               plotOutput("schoolTrend")))
                  )
                )
              )),

#######################################################################################################    
   
  tabItem(tabName = "dashboard",

        fluidRow(
          column(width = 9,
                 box(width = NULL, solidHeader = T,
                     leafletOutput("campus_map", height = 800))),
                 column(width = 3,
                        box(width = NULL, status = "warning",
                            uiOutput("state_select"),
                            checkboxInput("all_state", "Show All", FALSE)))
          
                 ))
        
        
        )
        
        )
        
)    
)
  




