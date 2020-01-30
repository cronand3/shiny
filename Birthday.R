#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(RCurl)
library(readxl)
require(XLConnect)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(eeptools)
# use this excel data
#BirthdayRankings <- read_excel("BirthdayRankings.xlsx")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny Birthday App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Use the calendar to select your birthday, then upload the BirthdayRankings.xlsx file"),
            dateInput("bday","Your Birthday:", min = '1900-01-01',max='2030-12-31'), #as.date()
            fileInput("datafile", "Upload .xlsx file", multiple = FALSE, placeholder = "No file selected"),
            radioButtons("yn","Would you like to view the table of rankings?", c("Yes","No")),
            tableOutput("filetable")
        ),

        # Show a plot and text of data bassed on user input
        mainPanel(
          textOutput("yours"),
          textOutput("season"),
          uiOutput("seasongif"),
          textOutput("age"),
          textOutput("leap"),
          plotOutput("ranks"),
          uiOutput("cake")
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # read in file  
  my_data <- reactive({
      infile <- input$datafile
      if(is.null(infile)){
        #User did not add file
        return(NULL)
      }
      ext <- tools :: file_ext(infile$name)
      file.rename(infile$datapath, paste(infile$datapath, ext, sep = "."))
      read_excel(paste(infile$datapath, ext, sep="."), 1)
    
  })
            
  birth_month <- reactive({as.numeric(format(as.Date(input$bday), "%m"))}) #extract month
  birth_day <- reactive({as.numeric(format(as.Date(input$bday), "%d"))}) # extract day
  
  output$filetable <- renderTable({
      # only show the table of rankings if the user selected the "YES" option
      if(input$yn == "Yes"){
        my_data()
      }
  })
  
  output$yours <- renderText({
      # print selected date in mm-dd-yyyy format
      paste("Your birthday is", format(as.Date(input$bday), "%m/%d/%Y"))
  })
  
  
  output$season <- renderText({
      # define the seasons
      wint <- c("Dec", "Jan", "Feb")
      spr <- c("Mar", "Apr", "May")
      sum <- c("Jun", "Jul", "Aug")
      fall <- c("Sep", "Oct", "Nov")

    # print birth month and season       
      if (month.abb[birth_month()] %in% wint){ # Month between Decemeber and februrary
        paste("Your birthday is in",month.abb[birth_month()],", it is in the Winter.")      
      }
      else if (month.abb[birth_month()] %in% spr){ # Month between March and May
        paste("Your birthday is in",month.abb[birth_month()],", it is in the Spring.")
      }
      else if (month.abb[birth_month()] %in% sum){ # Month between June and August
        paste("Your birthday is in",month.abb[birth_month()],", it is in the Summer.")
      }
      else if (month.abb[birth_month()] %in% fall) { # Month between September and November
        paste("Your birthday is in",month.abb[birth_month()],", it is in the Fall.")
      }

  })
  
  output$seasongif <- renderUI({
    # define the seasons
    wint <- c("Dec", "Jan", "Feb")
    spr <- c("Mar", "Apr", "May")
    sum <- c("Jun", "Jul", "Aug")
    fall <- c("Sep", "Oct", "Nov")
    
    if (month.abb[birth_month()] %in% wint){
      tags$img(src="https://media3.giphy.com/media/DLEOWCHRnoUKI/200.webp?cid=790b7611e5432ba427b20b18be17ff063f64d684e4b6c263&rid=200.webp")
    }
    else if (month.abb[birth_month()] %in% spr){
      tags$img(src="https://media0.giphy.com/media/gB4KWtd3uSsJq/giphy.gif?cid=790b761136c8a92b69cd4fa3f50f3bde990829d24ebc1665&rid=giphy.gif")
    }
    else if (month.abb[birth_month()] %in% sum){
      tags$img(src="https://media0.giphy.com/media/EtgAMHErbcJIQ/200.webp?cid=790b76115f705220abee66eff645c2d603fe2a3f2153e10c&rid=200.webp")
    }
    else if (month.abb[birth_month()] %in% fall){
      tags$img(src="https://media2.giphy.com/media/yburpnPcPzBYc/200.webp?cid=790b76110b59cae7f09fb110607ec999a769da689503f3d5&rid=200.webp")
    }
    
  })
    
  output$age <- renderText({
      user_age <- age_calc(input$bday, enddate = Sys.Date(), units = "years", precise = FALSE)
      paste(c("You are",user_age," years old."))
  })
  
  output$leap <- renderText({
      # determine if the user was born in a leap year or not
      user_bday <- as.Date(input$bday)
      if(leap_year(user_bday)== TRUE){
        paste("You were born in a leap year - How fun!")
      }
      else{
        paste("You were not born in a leap year.")
      }
    
  })
    
  output$ranks <- renderPlot({
      
      infile <- input$datafile
      if(is.null(infile)){
        #User did not add file
        return(NULL)
      }
    
      x <- seq(1,366)
      y <- my_data()$Rank

      user_x <- cumsum(yday(ymd(input$bday))) # find total days passed from Jan 1(=0) until your birthday regardless of year
  
      # find input in birthday dataset
      user_listing <- which(my_data()$Month == birth_month() & my_data()$Day == birth_day()) # finds the row that that the birthday is in
      user_rank <- my_data()$Rank[user_listing]
      
    # make scatter plot, make each month a different color
      s <- ggplot(data=my_data(), aes(x=x, y = my_data()$Rank, colour = factor(my_data()$Month))) + geom_point() 
      s_labs <- s  + xlab("Days of Year") + ylab("Rank") + ggtitle( "How Common is Your Birthday?") + labs(colour  = "Month", subtitle = "Source: https://www.nytimes.com/2006/12/19/business/20leonhardt-table.html" )
      s_cap <- s_labs +labs(caption = "Reading the plot: 364th place is an extremely popular birthday.")
      s_with_you <- s_cap + annotate("point", x=user_x, y=my_data()$Rank[user_x], colour = "black", size = 3, shape = 8) # add a black star to the user's birthday
      
      s_with_you
      
  })
    
  output$cake <- renderUI({
    tags$img(src="https://media.giphy.com/media/YFF3cm2cgnDtm/giphy.gif")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
