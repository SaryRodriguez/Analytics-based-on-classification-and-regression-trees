
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Titulo del panel"),
    sidebarLayout(
                  sidebarPanel("sidebar panel"),
                  mainPanel(
                     h1("Primer nivel", align="center"),
                     h2("Segundo nivel"),
                     h3("tercer nivel"),
                     p("Este funciona para hacer nuevos parrafos...",
                       strong("strong() funciona para hacer negrita la letra"),
                       em("em() crea texto en italica"),
                       code("@la.saruki")),
                     p("texto en color", style="color:red")
                  )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)
