library(shiny)
library(tidyverse)
library(moderndive)
library(colourpicker)
library(patchwork)
library(gapminder)
library(plotly)
library(data.table)

house_prices <- house_prices %>% mutate(
    log10_price = log10(price), log10_size = log10(sqft_living) )
themes <- list("grey" = theme_grey(), "bw" = theme_bw(), "Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "dark" = theme_dark())

continents <- unique(gapminder$continent)
data <- mpg
# Define UI for application
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    #p("Tidymodels", style="color:firebrick"),
                    a("Actividad05", style="color:firebrick"),
                    tabPanel("Seattle House Prices",
                             h3("Seattle House Prices"),
                             br(),
                             img(src = "house.png", width = 150, height = 150),
                             br(),
                             fluidRow(
                                 column(2,
                                 selectInput("variable", "Choose a variable:",
                                             choices=c("bathrooms","bedrooms","price","sqft_living15","sqft_lot15") ),
                                 #list(`package` = tidymodels_packages()) # hacer una lista limpia con los paquetes principales
                                 
                                 sliderInput("bins",
                                             "Number of bins:",
                                             min = 1,
                                             max = 50,
                                             value = 30)
                             ),
                             column(10,
                                    plotOutput("distPlot")
                                    )),
                             
                    ),
                    tabPanel("Gapminder",
                             h3("Gapminder"),
                             br(),
                             img(src = "world.jpeg", width = 150, height = 100),
                             br(),
                             fluidRow(
                                 column(2,
                                        selectInput("continent", "Choose a Continent:",
                                                    choices=continents ),
                                        #list(`package` = tidymodels_packages()) # hacer una lista limpia con los paquetes principales
                                 ),
                                 column(10,
                                        plotOutput("worldPlot")
                                 )),
                             
                    ),
                    tabPanel("Cars",
                             h3("Mtcars"),
                             br(),
                             img(src = "car.png", width = 130, height = 150),
                             br(),
                             fluidRow(
                                 column(4,
                                        selectInput("man",
                                                    "Manufacturer:",
                                                    c("All",
                                                      unique(as.character(mpg$manufacturer))))
                                 ),
                                 column(4,
                                        selectInput("trans",
                                                    "Transmission:",
                                                    c("All",
                                                      unique(as.character(mpg$trans))))
                                 ),
                                 column(4,
                                        selectInput("cyl",
                                                    "Cylinders:",
                                                    c("All",
                                                      unique(as.character(mpg$cyl))))
                                 )
                                 ),
                             DT::dataTableOutput("table")
                    )
                )
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        req(input$variable)
        icq <- sym(input$variable)
        bins <- input$bins
        title <- paste0("Histogram for ",input$variable," in Seattle")
        ggplot(house_prices, aes_string(input$variable)) +
            geom_histogram(bins=bins,color="blue", fill="lightblue") +
            ggtitle(title) +
            xlab("Variable") + ylab("Price")
    })
    
    output$worldPlot <- renderPlot({
        continente <- input$continent
        filtro <- subset(gapminder, continent==continente)
        
       p1 <- ggplot(filtro, 
               aes(year, lifeExp)) + 
            geom_smooth() + ggtitle("Life Expectation over Time by Continent") +
            xlab("Year") + ylab("Life Expectancy") +
            theme_classic()
       p2 <- ggplot(filtro, 
                    aes(year,pop)) + 
           geom_smooth() + ggtitle("Population over Time by Continent") +
           xlab("Year") + ylab("Avg. Population") +
           theme_classic()+scale_y_continuous(labels=comma)
       p1+p2
    }) 
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        
        if (input$man != "All") {
            data <- data[data$manufacturer == input$man,]
        }
        if (input$cyl != "All") {
            data <- data[data$cyl == input$cyl,]
        }
        if (input$trans != "All") {
            data <- data[data$trans == input$trans,]
        }
        data
    }))
}



# Run the application 
shinyApp(ui = ui, server = server)
