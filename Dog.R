#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pennsylvania Lifetime Dog License Check"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Find out if you are registered for a lifetime dog license."),
            fileInput("datafile", "Upload file", multiple = FALSE, placeholder = "No file selected"),
            textInput("textinput1", "Input your zip code", placeholder = NULL),
            textInput("textinput2", "Input dog's name", placeholder = NULL)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("description"),
            tableOutput("info"),
            uiOutput("dogpic")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # read in file  
    dog_data <- reactive({
        validate(
            need(input$datafile != "", "Please select a datafile")
        )
        infile <- input$datafile
        if(is.null(infile)){
            #User did not add file
            return(NULL)
        }
        read.csv(infile$datapath)
    })
    
    
    
     data_zip <- reactive({
         # check if the zipcode matches
         subset(dog_data(), OwnerZip == input$textinput1)
    })
    
     data_zip_dogname <- reactive({
         subset(data_zip(), DogName == toupper(input$textinput2), select = c(LicenseType, Breed, Color, ValidDate))
     })
    
    output$description <- renderText({
        paste("Find out the type of license you are registered for, the dog breed and color it is listed under, and the date you purchased it.")
    })    

    output$info <- renderTable({
        data_zip_dogname()
    })
    
    output$dogpic <- renderUI({
        if(data_zip_dogname()$Color == "BLACK"){
            tags$img(src="https://www.dogingtonpost.com/wp-content/uploads/2015/11/black3.jpg", height = "75%", width = "75%")
         }
        else if(data_zip_dogname()$Color == "BLACK/BROWN"){
            tags$img(src="http://e993.com/forex/imgs/www.doggenetics.co.uk/photos/agoutirottie.jpg")
        }
        else if(data_zip_dogname()$Color == "BLACK/TAN"){
            tags$img(src="https://sitmeanssit.com/dog-training-mu/austin-dog-training/files/2011/07/Scout-Medlock-233x300.jpg")
        }
        else if(data_zip_dogname()$Color == "BRINDLE"){
            tags$img(src="https://img.dog-learn.com/dog-breeds/treeing-tennessee-brindle/treeing-tennessee-brindle-i12-sz14.jpg")
        }
        else if(data_zip_dogname()$Color == "BROWN"){
            tags$img(src="https://dogzone-tcwebsites.netdna-ssl.com/wp-content/uploads/2017/09/brown-dog-names-2.jpeg", height = "75%", width = "75%")
        }
        else if(data_zip_dogname()$Color == "GOLD"){
            tags$img(src="https://www.petsworld.in/blog/wp-content/uploads/2015/06/golden-retriever.jpg")
        }
        else if(data_zip_dogname()$Color == "RED"){
            tags$img(src="https://thehappypuppysite.com/wp-content/uploads/2019/06/red-dog-names-HP-long.jpg", height = "75%", width = "75%")
        }
        else if(data_zip_dogname()$Color == "SPOTTED"){
            tags$img(src="https://thehappypuppysite.com/wp-content/uploads/2019/06/Miniature-Dalmatian-HP-LONG.jpg", height = "75%", width = "75%")
        }
        else if(data_zip_dogname()$Color == "WHITE/BLACK"){
            tags$img(src="http://www.doggenetics.co.uk/photos/tickingeg.jpg")
        }
        else{
            tags$img(src="https://www.fairfaxcounty.gov/news2/wp-content/uploads/2018/01/dog-nose-620x264.jpg",width = "80%")
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
