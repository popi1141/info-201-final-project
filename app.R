# rm(list=ls())

library(shiny)
library(dplyr)
library(ggplot2)

# read data files into data frames and clean them up
netflix_orig <- read.csv("netflix.csv", stringsAsFactors = FALSE)
netflix_shows <- read.csv("netflix_shows.csv", stringsAsFactors = FALSE) %>% 
  distinct() %>% 
  head(108) %>% 
  mutate(group = "Netflix")
hulu_shows <- read.csv("hulu.csv", stringsAsFactors = FALSE) %>% 
  select(major_genre:release_data) %>% 
  mutate(group = "Hulu")

# ui goes here




# Question 1: which age rating group is more prevalent in each platform?
#             static bar graph with each platform and then bars with age ratings

both_shows <- full_join(netflix_shows, hulu_shows)

server <- function(input, output) {
   
  # static bar chart for age rating shows
  ggplot(both_shows, aes(rating)) +
    geom_bar(aes(fill = group),
             position = "dodge") +
    scale_fill_manual("Platform", 
                      values = c("Hulu" = "#09e41f", "Netflix" = "#fd4040")) +
    labs(title = "Number of Shows by MPAA Rating",
         x = "MPAA Rating",
         y = "Number of Shows",
         fill = "Platform")
   
}

# Run the application 
shinyApp(ui = ui, server = server)

