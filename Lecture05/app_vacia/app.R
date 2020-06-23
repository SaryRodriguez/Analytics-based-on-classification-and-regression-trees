
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Titulo del panel"),
    sidebarLayout(position="right",
        sidebarPanel("sidebar panel"),
        mainPanel("main panel")
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
