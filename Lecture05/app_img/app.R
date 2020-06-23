
library(shiny)
options(shiny.autoreload = TRUE)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(strong("La Saruki")),
    sidebarLayout(
        sidebarPanel(
            h2("R Geek"),
            br(),
            p("You can look at my code in:",
              code("https://github.com/SaryRodriguez")
              ),
            img(src="geek.png", height=200, width=200),
            p(em("There's no place like 127.0.0.1"))
                     ),
        mainPanel(
            h2(strong("Current position")),
            br(),
            p("Doing weird stuff as Data Scientist at HP - Customer Analytics"),
            h3(strong("Interests")),
            br(),            fluidRow(
                column(width = 1,"Coding"),
                column(width = 1, "Travel"),
                column(width = 1, "Rugby")
            ),
            img(src = "code.jpg", width = 70, height = 90), img(src = "travel.jpg", width = 70, height = 90), img(src = "rugby.jpg", width = 70, height = 90)
        )
    )
)



server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)
