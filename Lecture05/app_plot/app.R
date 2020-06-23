
library(shiny)
library(tidyverse)
library(moderndive)
library(colourpicker)
library(patchwork)

house_prices <- house_prices %>% mutate(
    log10_price = log10(price), log10_size = log10(sqft_living) )
themes <- list("grey" = theme_grey(), "bw" = theme_bw(), "Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "dark" = theme_dark())
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Seattle House Prices"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("price_bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            colourInput("color", "Select Fill colour", value="orange", showColour="background"),
            colourInput("colorLine","Select Line colour", value="white"),
            selectInput("theme", label = h4("Select theme for plot"), choices = names(themes))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        bins <- input$bins
        colorfill <- input$color
        colorline <- input$colorLine
        # draw the histogram with the specified number of bins
        p1 <- ggplot(house_prices, aes(x=log10_size))+
            geom_histogram(bins=bins, color=colorline, fill=colorfill)+
            labs(x="log10 living space (square feet)", title="House Size")+
            themes[[input$theme]]
        p2 <- ggplot(house_prices, aes(x=log10_price))+
            geom_histogram(bins=input$price_bins, color=colorline, fill=colorfill)+
            labs(x="log10 Price", title="House Price")+
            themes[[input$theme]]
        p1+p2
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
