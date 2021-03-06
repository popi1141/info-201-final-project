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
        menuSubItem("Plot", tabName = "originalplot", icon = icon("bar-chart")),
        menuSubItem("Table", tabName = "originaltable", icon = icon("th"))),
      
      #Year Range
      sliderInput("year_range",
                  "Years",
                  min = 1963,
                  max = 2017,
                  value = c(1963,2017),
                  step = 1, sep = ""),
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
              p("Which age rating group is more prevalent in each platform?"),
              br(),
              plotOutput("agePlot"),
              br(), 
              p("The age rating most prevalent on Hulu is TV-14 with 65 shows with this rating, followed by TV-PG with 18 shows holding this rating. 
The most prevalent rating on Netflix is also TV-14 with 40 shows holding this rating followed by TV-MA with 39 shows. In addition to this Netflix had shows
                that were rated R, Unrated or Not Rated where Hulu had no shows in these categories. Based upon this it seems that the platforms 
                may be aimed at different audiences and that Netflix has an older audience in mind than Hulu. Hulu comparativley had 5 shows that were
                rated G where Netflix had none. With this in mind we encourage users to think about who is the streaming service for? If it is for younger 
                audiences Hulu may be a better option since it has a greater number of child friendly shows, if the audience is older than Netflix has a wider
                range of ratings and content that may appeal to a more mature audience.")),
      tabItem(tabName = "agetable",
              tableOutput("ageTable")),
      #Genre Stuff
      tabItem(tabName = "genreplot",
              p("What is the difference in genre diversity between platforms?"),
              br(),
              plotOutput("genrePlot"),
              br(),
              p("We found that although Netflix and Hulu had some genre's that overlapped between the two platforms they were coded differently in the data.
        Some genre's weren't comparabale for example Netflix had a genre simply dubbed 'Marvel' that referred to their original series like Daredevil
                which Hulu simply did not have. In short the although there were overlaps in some of the more common genres like Comedy we found that each platform
                tends to have a slightly different offering in their genres as can be seen in the graph above. Hulu had 11 specific genres including three Netflix
                did not have listed: Action and Adventure, Anime, and Science Fiction. Whereas Netflix had 10 genres two that Hulu did not have which were 
                Foreign Language and Marvel. The problem we found is that each of these streaming platforms has a different method of denoting genre categories and
                some shows may fall into multiple categories but be sorted into only one. This is a limitation of the data format.")),
      tabItem(tabName = "genretable",
              tableOutput("genreTable")),
      #Release Stuff
      tabItem(tabName = "releasedplot",
              p("Which platform is better for nostalgia?"),
              br(),
              plotOutput("releasedPlot"), textOutput("textplot"),
              br(),
              p("Based on the above data if one is looking for older shows we recommend Hulu. Hulu had 12 show offerings that were released prior to the 2000s whereas
                Netflix did not. The oldest show was Sesame Street which was first released back in 1963. If one is looking for more modern shows
                we would recommend Netflix as majority of their content (108 shows sampled) were produced post 2000 with a marked increase in content produced from 
                2015 onwards. This increase in content production is expected as other streaming services such as Amazon Prime are crowding in on the new market in order to compete Netflix needs to
                offer original content that can only be viewed on their platform so they can distinguish and hold out aganist the competition.")),
      tabItem(tabName = "releasedtable",
              textOutput("texttable"),
              tableOutput("releasedTable")),
      #Originals Stuff
      tabItem(tabName = "originalplot",
              p("Which platform produces better reviewed series?"),
              br(),
              plotOutput("originalPlot"),
              br(),
              p("Based on the above data analyzed it appears that Hulu has a greater number
                of better reviewed series. By better reviewed series we mean series that recieved
                audience scores in the range of 81-100. Hulu had 77 shows that fell within this range
whereas Netflix had only 58 that fell within this range. One important limitation of this analysis is 
                that Netflix had 39 shows that simply were not reviewed and were missing audience scores.
Additionally, this analysis did not have subanzlyze ratings by genre. It's possible that one platform produces
better reviewed shows in certain genres which may be a deciding factor in determing which platform the user wants
                to pay for.")),
      tabItem(tabName = "originaltable",
              tableOutput("originalTable"))
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
    if (input$rating != "All") {
      both_ratings <- filter(both_ratings, rating == input$rating)
    }
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
                                       vjust = 1, size = 8))
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
    both_genres[is.na(both_genres)] <- 0
    if (input$genre != "All") {
      both_genres <- filter(both_genres, major_genre == input$genre)
    }
    return(both_genres)
  })
  
  # question 3 - plot

  output$releasedPlot <- renderPlot({
    both_shows <- filter(both_shows, release_data >= input$year_range[1] &
                                release_data <= input$year_range[2]) 
    
    ggplot(both_shows, aes(x = release_data)) +
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
  
  output$textplot <- renderText({
    paste0("This graph shows the number of shows from ", input$year_range[1], " to ", input$year_range[2], ".")
  })
  
  # question 3 - table
  output$releasedTable <- renderTable({
    hulu_released <- hulu_shows %>% 
      group_by(release_data) %>% 
      summarize("Hulu" = n())
    
    netflix_released <- netflix_shows %>% 
      group_by(release_data) %>% 
      summarize("Netflix" = n())
    
    both_released <- full_join(hulu_released, netflix_released)
    both_released[is.na(both_released)] <- 0
    both_released <- filter(both_released, release_data >= input$year_range[1] &
                           release_data <= input$year_range[2])
    names(both_released)[1] <- "Year"
    return(both_released)
  })
  
  output$texttable <- renderText({
    paste0("This table shows the number of shows from ", input$year_range[1], " to ", input$year_range[2], ".")
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
  
  # question 4 - table
  output$originalTable <- renderTable({
    hulu_original <- hulu_shows %>% 
      group_by(viewer_score) %>% 
      summarize("Hulu" = n())
    
    netflix_original <- netflix_shows %>% 
      group_by(viewer_score) %>% 
      summarize("Netflix" = n())
    
    both_original <- full_join(hulu_original, netflix_original)
    both_original[is.na(both_original)] <- 0
    both_original <- filter(both_original,
                             viewer_score >= input$review_range[1],
                             viewer_score <= input$review_range[2])
    names(both_original)[1] <- "Review Score"
    return(both_original)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

