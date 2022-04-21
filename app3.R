#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)

master_data <- list(faithful = faithful, mtcars = mtcars, diamonds = diamonds)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    #titlePanel(shiny::div("Old Faithful Geyser Data", style = "color:red")),
    titlePanel(uiOutput("title_panel")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2,
            selectInput("dataset", 
                        label = "Data Set",
                        choices = setNames(nm = names(master_data))),
            selectInput("variable", 
                        label = "Variable",
                        choices = setNames(nm = names(master_data[["faithful"]]))),
                     
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            selectInput("color", label = "Bar Color",
                        choices = setNames(nm = c("green", "blue", "red"))),
            selectInput("border", label = "Border Color",
                        choices = setNames(nm = c("white", "black"))),
            checkboxInput("density", "Density"),
            helpText("This is my first App, way cool, yeah!")
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 10,
                  fluidRow(
                      column(5, plotOutput("distPlot")),
                      column(5, plotOutput("distPlot2")))
        )
    )
)
get_histogram <- function(data, col, bins, color, border, density ){
    if(is.numeric(col) && col > length(data)){
        return(NULL)
    }
    if(is.character(col) && !(col %in% names(data))){
        return(NULL)
    }
    x    <- as.data.frame(data)[, col] 
    
    bins <- seq(min(x), max(x), length.out = bins + 1)
    title <- names(data)[col]
    if(density){
        #browser()
        d <- density(x, n = 2^ceiling(log2(length(bins))))
        plot(d, main = title)
        polygon(d, col = color, border = border)
    }
    else{
        hist(x, main = title, breaks = bins, col = color, border = border, density)
    }
    
}
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$dataset, {
        updateSelectInput(session, 
                          "variable", 
                          choices = names(master_data[[input$dataset]] %>% 
                                              select(where(is.numeric))))
    })
    output$title_panel <- renderUI({
        
        shiny::div(glue::glue("Ye Old {str_to_title(input$dataset)}  Data"), style = glue::glue("color:{input$color}"))
    })
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #browser()
        get_histogram(master_data[[input$dataset]], input$variable, input$bins, input$color, input$border, input$density)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
