library(shiny)
library("dplyr")
library("ggplot2")
library("tidyr")
library("ggmap")
library("viridis")
library("shinydashboard")
#library(devtools)
#install_github("nik01010/dashboardthemes")
library("dashboardthemes")

#Data Sorting
netflix_originals <- read.csv("netflix.csv", stringsAsFactors = FALSE)
hulu <- read.csv("hulu.csv", stringsAsFactors = FALSE)
netflix <- read.csv("netflix_shows.csv", stringsAsFactors = FALSE)
#Ratings
ratings <- unique(c(netflix$rating))
#Genre
genres <- unique(c(hulu$major_genre))


#Creates the Logo for the Header
app_logo <- shinyDashboardLogoDIY(
  boldText = "Netflix vs Hulu",
  mainText = "",
  textSize = 16,
  badgeText = "BETA",
  badgeTextColor = "white",
  badgeTextSize = 2,
  badgeBackColor = "#5280ff",
  badgeBorderRadius = 3
)

# Define UI for application that draws a histogram
ui <- dashboardPage (
  dashboardHeader(
    title = app_logo
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Age Comparison", tabName = "dashboard", icon = icon("user-circle")),
      menuItem("Genre Diversity", icon = icon("film"), tabName = "genre"),
      menuItem("Nostalgia Meter", tabName = "nostalgia", icon = icon("clock")),
      menuItem("Placeholder", tabName = "placeholder", icon = icon("question-circle")),
      #Year Range
      sliderInput("year_range",
                  "Years",
                  min = 1940,
                  max = 2017,
                  value = c(1940,2016)),
      #Reviews
      sliderInput("review_range",
                  "Review Scores",
                  min = 0,
                  max = 100,
                  value = c(0,100)),
      #Ratings
      selectInput("rating", 
                  "Rating Select:",
                  ratings),
      #Genre 
      selectInput("genre", 
                  "Genre Select:",
                  genres)
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_light"),
    tabItems(
      tabItem(tabName = "dashboard",
        tableOutput("feature_one")),
      tabItem(tabName = "genre",
        tableOutput("feature_two")),
      tabItem(tabName = "nostalgia",
        tableOutput("feature_three")),
      tabItem(tabName = "placeholder",
        tableOutput("feature_four"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$feature_one <- renderTable({
    x    <- faithful 
    x
  })
  
  output$feature_two <- renderTable({
    x    <- mtcars 
    x
  })
  
  output$feature_three <- renderTable({
    x    <- Titanic 
    x
  })
  
  output$feature_four <- renderTable({
    x    <- faithful 
    x
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

