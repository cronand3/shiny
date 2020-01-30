#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googleVis)
require(googleVis)
data("USArrests")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("USData"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("var","Variable to explore:",choices = c("Murder", "Assault", "UrbanPop", "Rape"),
                        selected = NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("newPlot"),
           textOutput("crime")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    myCrime <- reactive({
        input$var
    })
    
    output$crime <- renderText({
        paste("The crime stats in arrests per 100,000 residents in 1973 for", myCrime())
    })
        
states <- data.frame(state.name, state.x77)
    
    output$newPlot <- renderGvis({
        gvisGeoChart(states, locationvar = "state.name", colorvar = myCrime(), options = list(region= "US", displayMode="regions", 
                            resolutions="provinces", width=500, height = 400, colorAxis="{colors:['#FFFFFF', '#0000FF']}"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
