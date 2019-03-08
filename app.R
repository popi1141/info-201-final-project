# rm(list=ls())

library(shiny)
library(dplyr)
library(ggplot2)


netflix_orig <- read.csv("netflix.csv", stringsAsFactors = FALSE)
netflix_shows <- read.csv("netflix_shows.csv", stringsAsFactors = FALSE) %>% 
  distinct() %>% 
  head(109) %>% 
  mutate(group = "netflix")
hulu_shows <- read.csv("hulu.csv", stringsAsFactors = FALSE) %>% 
  select(major_genre:release_data) %>% 
  distinct() %>% 
  mutate(group = "hulu")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)
# Question 1: which age rating group is more prevalent in each platform?
#             static bar graph with each platform and then bars with age ratings


both_shows <- full_join(netflix_shows, hulu_shows)

ggplot(both_shows, aes(rating)) +
  geom_bar(aes(fill = group),
           position = "dodge"
) +
  labs(title = "Number of Shows by MPAA Rating",
       x = "MPAA Rating",
       y = "Number of Shows",
       fill = "Platform")
  

server <- function(input, output) {
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

