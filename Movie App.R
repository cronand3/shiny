library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

movie_data <- read.csv("movie_ratings.csv")
movie_data$id <- seq(1:nrow(movie_data))

movie_data$genres <- str_sub(movie_data$genres, start =1, end=5) # clean data so each movie is assocaited with only primary genre (and first five characters only for simplicity)
# Clean the data - Content rating column 
movie_data$content_rating[is.na(movie_data$content_rating)] <- "Not Rated"
movie_data$content_rating[movie_data$content_rating == ""] <- "Not Rated"
movie_data$content_rating[movie_data$content_rating == "Unrated"] <- "Not Rated"

movie_data$content_rating[movie_data$content_rating == "TV-PG"] <- "TV-PG and below"
movie_data$content_rating[movie_data$content_rating == "TV-G"] <- "TV-PG and below"
movie_data$content_rating[movie_data$content_rating == "TV-Y"] <- "TV-PG and below"
movie_data$content_rating[movie_data$content_rating == "TV-Y7"] <- "TV-PG and below"


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Movie Ratings by Genre"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Can't decide what to watch? Pick a genre and explore options, easily see IMDB rating and content rating."),
            selectInput("genre",
                        "Movie genre:", c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Documentary", "Drama", "Family", "Fantasy", "Horror", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("myPlot", hover = "movieSelection"),
          tableOutput("suggestion")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Switch the input from the interactive button to be associated with the actual vairable *MUST BE REACTIVE*
    genreInput <- reactive({
        # the user clicks a genre and R subsets the dataframe to only use drows with that genre
        switch (input$genre,
                "Action"=movie_data[movie_data$genres=="Actio",], "Adventure"=movie_data[movie_data$genres=="Adven",], "Animation"=movie_data[movie_data$genres=="Anima",], "Biography"=movie_data[movie_data$genres=="Biogr",], 
                "Comedy"=movie_data[movie_data$genres=="Comed",], "Crime"=movie_data[movie_data$genres=="Crime",], "Documentary"=movie_data[movie_data$genres=="Docum",], "Drama"=movie_data[movie_data$genres=="Drama",], 
                "Family"=movie_data[movie_data$genres=="Famil",], "Fantasy"=movie_data[movie_data$genres=="Fanta",], "Horror"=movie_data[movie_data$genres=="Horro",], "Mystery"=movie_data[movie_data$genres=="Myste",], 
                "Romance"=movie_data[movie_data$genres=="Roman",], "Sci-Fi"=movie_data[movie_data$genres=="Sci-F",], "Thriller"=movie_data[movie_data$genres=="Thril",], "Western"=movie_data[movie_data$genres=="Weste",])
    })

    output$myPlot <- renderPlot({
        qplot(id, imdb_score, data = genreInput(), geom = "point", colour = factor(content_rating), size = .05, xlab="", ylab = "IMDB Rating") 
      
    })
    
    output$suggestion <- renderTable({
        nearPoints(genreInput(), input$movieSelection, threshold = 10, maxpoints =1)
      
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
