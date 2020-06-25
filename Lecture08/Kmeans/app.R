library(shiny)
library(EBImage)
upload_image <- list()

ui <- fluidPage(theme = shinytheme("flatly"),
    titlePanel(title="Kmeans Image Clustering"),
    fileInput("file1", "Upload an Image",accept = c('image/png', 'image/jpeg','image/jpg')),
   
    plotOutput("img"),
    plotOutput("paleta")
    
)

server <- function(input, output) {
    output$img <- renderPlot({ 
        req(input$file1)
        upload_image[[1]] <- readImage(input$file1$datapath)
        plot(upload_image[[1]])
    })
    output$paleta<- renderPlot({
        req(input$file1)
        upload_image[[1]] <- readImage(input$file1$datapath)
        im <-  upload_image[[1]]
        dimension    <- dim(im)
        painting_rgb <- data.frame(
            x = rep(1:dimension[2], each = dimension[1]),
            y = rep(dimension[1]:1, dimension[2]),
            R = as.vector(im[,,1]), #slicing our array into three
            G = as.vector(im[,,2]),
            B = as.vector(im[,,3])
        )
        set.seed(123)
        k_means <- kmeans(painting_rgb[,c("R","G","B")], centers = 5, iter.max = 30)
        show_col(rgb(k_means$centers), labels=FALSE)
    })
}


shinyApp(ui , server)