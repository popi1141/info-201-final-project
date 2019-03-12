#rm(list=ls())

library(devtools)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(viridis)
library(shinydashboard)
library(dashboardthemes)
#install_github("nik01010/dashboardthemes")

#Reads the Data for Netflix Originals
netflix_orig <- read.csv("netflix.csv", stringsAsFactors = FALSE) 
colnames(netflix_orig)[1] <- "Major_Genre"
netflix_orig <- netflix_orig %>%
  head(108) %>% 
  mutate(group = "Netflix") %>% 
  mutate(major_genre = Major_Genre) %>% 
  mutate(viewer_score = IMDB_Rating)

#Reads the Data for Netflix Shows in General
netflix_shows <- read.csv("netflix_shows.csv", stringsAsFactors = FALSE) %>% 
  distinct() %>% 
  head(108) %>% 
  mutate(viewer_score = user.rating.score) %>% 
  mutate(release_data = release.year) %>% 
  mutate(group = "Netflix")
ratings <- unique(c("All", netflix_shows$rating))

#Reads the Data for Hulu Shows
hulu_shows <- read.csv("hulu.csv", stringsAsFactors = FALSE) %>% 
  select(major_genre:release_data) %>% 
  mutate(group = "Hulu")
genres <- unique(c("All", hulu_shows$major_genre)) # genres

#Joins the Data Set
both_shows <- full_join(netflix_shows, hulu_shows)
both_originals <- full_join(netflix_orig, hulu_shows)
both_shows_year <- full_join(netflix_shows,hulu_shows)

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

ui <- dashboardPage (
  
  #Creates the Header w/ Logo
  dashboardHeader(title = app_logo),
  
  #Creates the Sidebar
  dashboardSidebar(
    sidebarMenu(
      
      #Age Menu
      menuItem("Age Comparison", tabName = "", icon = icon("user-circle"),
        menuSubItem("Plot", tabName = "ageplot", icon = icon("bar-chart")),
        menuSubItem("Table", tabName = "agetable", icon = icon("th"))),
      
      #Genre Menu
      menuItem("Genre Diversity", tabName = "", icon = icon("film"), 
               menuSubItem("Plot", tabName = "genreplot", icon = icon("bar-chart")),
               menuSubItem("Table", tabName = "genretable", icon = icon("th"))),
      
      #Nostalgia Menu
      menuItem("Nostalgia Meter", tabName = "", icon = icon("clock"),
        menuSubItem("Plot", tabName = "releasedplot", icon = icon("bar-chart")),
        menuSubItem("Table", tabName = "releasedtable", icon = icon("th"))),
      
      #Originals Table
      menuItem("Original Reviews", tabName = "", icon = icon("question-circle"),
        menuSubItem("Plot", tabName = "originalsplot", icon = icon("bar-chart")),
        menuSubItem("Table", tabName = "originalstable", icon = icon("th"))),
      
      #Year Range
      sliderInput("year_range",
                  "Years",
                  min = 1940,
                  max = 2017,
                  value = c(1940,2017)),
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
  #Generates the Body
  dashboardBody(
    shinyDashboardThemes(theme = "poor_mans_flatly"),
    tabItems(
      #Age Stuff
      tabItem(tabName = "ageplot",
              plotOutput("agePlot")),
      tabItem(tabName = "agetable",
              tableOutput("ageTable")),
      #Genre Stuff
      tabItem(tabName = "genreplot",
              plotOutput("genrePlot")),
      tabItem(tabName = "genretable",
              tableOutput("genreTable")),
      #Release Stuff
      tabItem(tabName = "releasedplot",
              plotOutput("releasedPlot")),
      tabItem(tabName = "releasedtable",
              tableOutput("releasedTable")),
      #Originals Stuff
      tabItem(tabName = "originalsplot",
              plotOutput("originalPlot")),
      tabItem(tabName = "originalstable",
              tableOutput("originalsTable"))
    )
  )
)

server <- function(input, output) {
  
  #Question 1 - plot
  output$agePlot <- renderPlot({
    if (input$rating != "All") {
      both_shows <- filter(both_shows, rating == input$rating)
    }
    
    ggplot(both_shows, aes(rating)) +
      geom_bar(aes(fill = group),
               position = "dodge") +
      scale_fill_manual("Platform", 
                        values = c("Hulu" = "#09e41f", "Netflix" = "#fd4040")) +
      labs(title = "Number of Shows by MPAA Rating",
           x = "MPAA Rating",
           y = "Number of Shows",
           fill = "Platform")
  })
  
  # Question 1 - table
  # static data frame for age rating shows
  output$ageTable <- renderTable({
    hulu_ratings <- hulu_shows %>% 
      group_by(rating) %>% 
      summarize("Hulu" = n())
    
    netflix_ratings <- netflix_shows %>% 
      group_by(rating) %>% 
      summarize("Netflix" = n())
    
    both_ratings <- full_join(hulu_ratings, netflix_ratings)
    both_ratings[is.na(both_ratings)] <- 0
    return(both_ratings)
  })
  
  # Question 2 - Plot
  # reactive bar chart about diversity of genres between platforms (originals)
  output$genrePlot <- renderPlot({
    if (input$genre != "All") {
      both_originals <- filter(both_originals, major_genre == input$genre)
    }
    
    ggplot(both_originals, aes(x = major_genre)) +
      geom_bar(aes(fill = group),
               position = "dodge") +
      scale_fill_manual("Platform", 
                        values = c("Hulu" = "green", "Netflix" = "red")) +
      labs(title = "Number of Shows by Genre",
           x = "Genre",
           y = "Number of Shows",
           fill = "Platform") +
      theme(axis.text.x = element_text(angle=30,margin = 
                                         margin(0.5, unit = "cm"), 
                                       vjust = 1, size = 5.5))
  })
  
  # question 2: table
  # reactive table about diversity of genres between platforms
  output$genreTable <- renderTable({
    hulu_genres <- hulu_shows %>% 
      group_by(major_genre) %>% 
      summarize("Hulu" = n())
    
    netflix_genres <- netflix_orig %>% 
      group_by(major_genre) %>% 
      summarize("Netflix" = n())
    
    both_genres <- full_join(hulu_genres, netflix_genres)
    both_genres[is.na(both_ratings)] <- 0
    return(both_genres)
  })
  
  #3 Plots the number of shows Netflix release vs the number of shows Hulu 
  #released in the given year range 
  
  
  
  output$releasedPlot <- renderPlot({
    both_shows_year <- filter(both_shows_year, release_data >= input$year_range[1] &
                                release_data <= input$year_range[2]) 
    
    ggplot(both_shows_year, aes(x = release_data)) +
      geom_bar(aes(fill = group),
               position = "dodge") +
      scale_x_continuous(limits=c(input$year_range[1], input$year_range[2])) +
      scale_fill_manual("Platform", 
                        values = c("Hulu" = "green", "Netflix" = "red")) +
      labs(title = "Number of Shows by Year",
           x = "Year",
           y = "Number of Shows",
           fill = "Platform")
  })
  
  # question 4 - plot
  output$originalPlot <- renderPlot({
    both_originals <- filter(both_originals,
                         viewer_score >= input$review_range[1],
                         viewer_score <= input$review_range[2])
    factor = cut(both_originals$viewer_score, 
                 breaks = c(-Inf, 20, 40, 60, 80, Inf),
                 labels = c("0-20", "21-40", "41-60", "61-80", "81-100"))
    both_originals <- mutate(both_originals, factor = factor)
    
    ggplot(both_originals, aes(x = factor)) + 
      geom_bar(aes(fill = group),
               position = "dodge") +
      scale_fill_manual("Platform", 
                        values = c("Hulu" = "green", "Netflix" = "red")) +
      labs(title = "Number of Shows by User Rating",
           x = "User Ratings",
           y = "Number of Shows",
           fill = "Platform")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

