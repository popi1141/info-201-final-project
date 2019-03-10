#rm(list=ls())

library(shiny)
library(dplyr)
library(ggplot2)

# read data files and clean them up
netflix_orig <- read.csv("netflix.csv", stringsAsFactors = FALSE) %>% 
  head(108) %>% 
  mutate(group = "Netflix")
netflix_shows <- read.csv("netflix_shows.csv", stringsAsFactors = FALSE) %>% 
  distinct() %>% 
  head(108) %>% 
  mutate(viewer_score = user.rating.score) %>% 
  mutate(group = "Netflix")
hulu_shows <- read.csv("hulu.csv", stringsAsFactors = FALSE) %>% 
  select(major_genre:release_data) %>% 
  mutate(group = "Hulu")
both_shows <- full_join(netflix_shows, hulu_shows)

# ui goes here






server <- function(input, output) {
  
  # filter data to display only shows within the input year range.
  both_shows <- filter(both_shows, 
                       release.year >= input$year[1],
                       release.year <= input$year[2])
  
  # filter data to display only shows within the input review score range.
  both_shows <- filter(both_shows,
                       viewer_score >= input$review[1],
                       viewer_score <= input$review[2])
   
  # static bar chart for age rating shows
  output$agePlot <- renderPlot({
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
  
  # reactive histogram about diversity of genres between platforms
  
}



# Run the application 
shinyApp(ui = ui, server = server)

