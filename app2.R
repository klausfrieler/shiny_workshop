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
    titlePanel(shiny::div("Old Faithful Geyser Data", style = "color:red")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2,
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("bins2",
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
    if(col > length(data)){
        return(NULL)
    }
    x    <- data[, col]
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
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        get_histogram(faithful, 1, input$bins, input$color, input$border, input$density)
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # title <- names(faithful)[2]
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, main = title, breaks = bins, col = input$color, border = input$border_color)
    })
    output$distPlot2 <- renderPlot({
        get_histogram(faithful, 2, input$bins2, input$color, input$border, input$density)
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 1]
        # bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
        # title <- names(faithful)[1]
        # # draw the histogram with the specified number of bins
        # hist(x, main = title, breaks = bins, col = input$color, border =  input$border_color)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
