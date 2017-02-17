library(shinydashboard)
shinyUI(dashboardPage(
  # Application title
  dashboardHeader(title = "US Crime Data App"),
  
  # Sidebar with a slider input for years
  dashboardSidebar(
    sidebarMenu(
      menuItem("US Crime Map", tabName = "crime_map", icon = icon("globe"),badgeColor='light-blue'),
      menuItem("State Crime 2D Heatmap", tabName = 'heatmap', icon = icon('globe'),badgeColor='red')
    ),
    sliderInput("Year", 
                label="Choose your year:", min=2001, max=2014, value=2008,animate = T)
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "crime_map",
              fluidRow(box(htmlOutput("title1"),width=12,background='light-blue')),
              fluidRow(infoBoxOutput("maxbox"),
                       infoBoxOutput("medbox"),
                       infoBoxOutput("minbox")),
              fluidRow(htmlOutput("map"), title='US Crime Colored Map')), 
      tabItem(tabName = "heatmap",
              fluidRow(box(plotOutput("heatmap"),width = 12,background='navy')),
              fluidRow(box(selectizeInput('heatY',"Heat map Y variable:",heatYChoices),height=100,width=4,background='navy'))
      )
    
      ))
))


