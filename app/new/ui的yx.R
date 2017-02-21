library(shinydashboard)

shinyUI(dashboardPage(
  # Application title
  dashboardHeader(title = "US Crime Data App"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("US Crime Map", tabName = "crime_map", icon = icon("globe"),badgeColor='light-blue'),
      menuItem("CA Crime Map", tabName = 'ca_map', icon = icon('globe'),badgeColor='red')
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
              fluidRow(htmlOutput("map"), title='US Crime Colored Map'),
              fluidRow(
                column(10,
                       sliderInput("Year",
                                   label = "Choose year:",min=2001,max=2014,value=2008,animate=T)
                )
              )
      ),
      
      # tab-item two: ca crime map 
      tabItem(tabName = "ca_map",
              fluidRow(box(plotOutput("camap"),width = 12,background='navy')),
              fluidRow(
                column(10,
                       sliderInput("YEAR",
                                   label="Choose your Year",min=2006,max=2015,value=2008,animate=T))
              )
      )
    
    )
  )
))


