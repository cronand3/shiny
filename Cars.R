#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(mapproj)
library(ggplot2)
data(mtcars)

avgmpg <- sum(mtcars$mpg)/nrow(mtcars)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cars"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("avgmpg",
                        "Would you like to know the average MPG of the dataset?", choices = c("Yes", "No"))
            ,
            selectInput("checkbox", label = "Choose an *x* variable:", choices = c("MPG", "Weight","Horsepower", "1/4 mile time"), selected = "MPG"),
            selectInput("checkbox2", label = "Choose a *y* variable:", choices = c("MPG", "Weight","Horsepower", "1/4 mile time"), selected =  "Weight"),
            selectInput("checkbox3", label = "Choose a variable to factor by:", choices = c("Number of Cylinders","Carburetors", "Gear"), selected =  "Gear"),
            sliderInput("slider", label = "Choose number of bins:", min = 5, max = 25, value = 10)
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Scatter"),
           plotOutput("hist"),
           textOutput("averageMPG")
        )
    )
)


server <- function(input, output) {
  # Reactive functions to *get* actual variables to work with
    #### switch first input
    check1Input <- reactive({
        switch (input$checkbox,
                "MPG" = mtcars$mpg,"Weight"=mtcars$wt,"Horsepower"=mtcars$hp, "1/4 mile time"=mtcars$qsec)
        })
    #### switch second input 
    check2Input <- reactive({
        switch (input$checkbox2,
                "MPG" = mtcars$mpg,"Weight"=mtcars$wt,"Horsepower"=mtcars$hp, "1/4 mile time"=mtcars$qsec)
    })
    #### switch third input 
    check3Input <- reactive({
        switch (input$checkbox3,
                 "Number of Cylinders" = mtcars$cyl, "Carburetors"=mtcars$carb, "Gear"=mtcars$gear)
    })

    
    output$averageMPG <- renderText({
        
        if( input$avgmpg == "Yes"){
            paste("The average MPG is", avgmpg)
        } else{}
    })
  

    output$Scatter <- renderPlot({
            x <- check1Input()
            y <- check2Input()
            f <- check3Input()
            
            p <- ggplot(mtcars, aes(x, y)) + geom_point(aes(colour=factor(f), size = 1.5))
            p <- p + xlab(input$checkbox) + ylab(input$checkbox2) + labs(colour = input$checkbox3) + ggtitle("Car Info Plot")
            p
        

    })
    
    output$hist <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- mtcars$mpg
        n_breaks <- seq(min(x), max(x), length.out = input$slider + 1)
        
        # draw the histogram of mpg with the specified number of bins from the slider 
        ggplot(data = mtcars, aes(x=mtcars$mpg)) + geom_histogram(color = "black", fill = "light gray", linetype = "dashed", breaks = n_breaks) + labs(x = "MPG",title = "MPG Frequency")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
