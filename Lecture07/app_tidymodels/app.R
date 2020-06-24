
# https://www.tidymodels.org/
#https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
library(shiny)
library(tidymodels)
library(ggplot2)
library(tidyr)
library(shinythemes)
library(modeldata)
data(Orange)
Orange <- as_tibble(Orange)

d_tidymodels <- "tidymodels is a 'meta-package' for modeling and statistical analysis
that share the underlying design philosophy, grammar, and data structures of the tidyverse.
It includes a core set of packages that are loaded on startup:

-broom takes the messy output of built-in functions in R, such as lm, nls, or t.test,
and turns them into tidy data frames.
-dials has tools to create and manage values of tuning parameters.
-dplyr contains a grammar for data manipulation.
-ggplot2 implements a grammar of graphics.
-infer is a modern approach to statistical inference.
-parsnip is a tidy, unified interface to creating models.
-purrr is a functional programming toolkit.
-recipes is a general data preprocessor with a modern interface. It can create
model matrices that incorporate feature engineering, imputation, and other help tools.
-rsample has infrastructure for resampling data so that models can be assessed
and empirically validated.
-tibble has a modern re-imagining of the data frame.
-tune contains the functions to optimize model hyper-parameters.
-workflows has methods to combine pre-processing steps and models into a single object.
-yardstick contains tools for evaluating models (e.g. accuracy, RMSE, etc.)

There are a few modeling packages that are also installed along with tidymodels
(but are not attached on startup):

-tidypredict translates some model prediction equations to SQL for high-performance computing.
-tidyposterior can be used to compare models using resampling and Bayesian analysis.
-tidytext contains tidy tools for quantitative text analysis, including basic text
summarization, sentiment analysis, and text modeling.
"
d_rsample <- "rsample contains a set of functions to create different types of 
resamples and corresponding classes for their analysis. The goal is to have a modular
set of methods that can be used across different R packages for:

- traditional resampling techniques for estimating the sampling distribution of
a statistic and
- estimating model performance using a holdout set

The scope of rsample is to provide the basic building blocks for creating and
analyzing resamples of a data set but does not include code for modeling or calculating
statistics. The 'Working with Resample Sets' vignette gives demonstrations of how rsample
tools can be used.

Note that resampled data sets created by rsample are directly accessible in a 
resampling object but do not contain much overhead in memory. Since the original data is
not modified, R does not make an automatic copy"

d_parsnip <- "The goal of parsnip is to provide a tidy, unified interface to models that
can be used to try a range of models without getting bogged down in the syntactical minutiae of the underlying packages."

d_recipes <- "The recipes package is an alternative method for creating and preprocessing design matrices that can be 
used for modeling or visualization.

The idea of the recipes package is to define a recipe or blueprint that can be used to sequentially define the encodings 
and preprocessing of the data (i.e. 'feature engineering')."

d_workflows <- "A workflow is an object that can bundle together your pre-processing, modeling, and post-processing requests.
For example, if you have a recipe and parsnip model, these can be combined into a workflow. The advantages are:

- You don't have to keep track of separate objects in your workspace.

- The recipe prepping and model fitting can be executed using a single call to fit().

- If you have custom tuning parameter settings, these can be defined using a simpler interface when combined with tune.

- In the future, workflows will be able to add post-processing operations, such as modifying the probability cutoff for two-class models."

d_tune <- "The goal of tune is to facilitate the tuning of hyper-parameters the tidymodels packages. 
It relies heavily on recipes, parsnip, and dials."

d_yardstick <- "yardstick is a package to estimate how well models are working using tidy data principles.
See the package webpage for more information."

d_broom <- "broom summarizes key information about models in tidy tibble()s. 
broom provides three verbs to make it convenient to 
interact with model objects:
- tidy() summarizes information about model components
- glance() reports information about the entire model
- augment() adds informations about observations to a dataset
For a detailed introduction, please see vignette(\"broom\").

broom tidies 100+ models from popular modelling packages and almost all of the model
objects in the stats package that comes with base R. vignette(\"available-methods\")
lists method availability.

If you are not familiar with tidy data structures and want to know how they
can make your life easier, we highly recommend reading Hadley Wickham’s Tidy Data.
"

d_dials <- "This package contains tools to create and manage values of tuning parameters
and is designed to integrate well with the parsnip package.

The name reflects the idea that tuning predictive models can be like turning a set of dials 
on a complex machine under duress."

overviews_keys <- c("broom","tidymodels","rsample","parsnip","recipes","workflows","tune","yardstick","dials")
overviews_descriptions <- c(d_broom,d_tidymodels,d_rsample,d_parsnip,d_recipes, d_workflows, d_tune, d_yardstick, d_dials)
names(overviews_descriptions) <- overviews_keys

# Define UI for application
ui <- fluidPage(theme = shinytheme("sandstone"),
    navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        a("Tidymodels", style="color:firebrick", href="https://www.tidymodels.org/"),
        tabPanel("Installation",
                 fluidRow(
                     column(5, img(src = "tidymodels.png", hight=300, width = 300)),
                     column(3,
                            h3("TIDYMODELS"), br(),
                            p("The tidymodels framework is a collection of packages for modeling 
                            and machine learning using", strong("tidyverse"),"principles."), 
                            p("Install tidymodels with:"),
                            br(),br(), code("install.packages(\"tidymodels\")"), br(),br(),
                            p("Run", em("library(tidymodels)"), "to load the core packages and make 
                              them available in your current R session"))
                 )
                ),
        tabPanel("Packages", 
                 h3("CORE TIDYMODELS"),
                 br(),
                 img(src = "tidymodels.png", width = 70, height = 80), img(src = "rsample.png", width = 70, height = 80), img(src = "parsnip.png", width = 70, height = 80),img(src = "recipes.png", width = 70, height = 80),img(src = "workflows.png", width = 70, height = 80),img(src = "tune.png", width = 70, height = 80),img(src = "yardstick.png", width = 70, height = 80),img(src = "broom.png", width = 70, height = 80),img(src = "dials.png", width = 70, height = 80),
                 br(),
                 br(),
                 p("The core tidymodels packages work together to enable a wide variety of modeling approaches."),
                 selectInput("state", "Choose a tidymodel library:",
                             list(`package` = c("tidymodels","rsample","parsnip","recipes","workflows","tune","yardstick","broom","dials"))
                             #list(`package` = tidymodels_packages()) # hacer una lista limpia con los paquetes principales
                             ),
                 uiOutput(outputId="logo"),
                 verbatimTextOutput("result")
                 #htmlOutput("frame")
                 
                 
                 ),
        tabPanel("Learn", "After you know what you need to get started with tidymodels, you can learn more and go further.
                 Find articles here to help you solve specific problems using the tidymodels framework. Articles are organized into four categories:",
                 
                     tabsetPanel(
                         tabPanel("Statistical Analysis",
                                  h4("Perform Statistical Analysis"),
                                  #tableOutput("table"),
                                  tabsetPanel(
                                      tabPanel("Correlation and regression",
                                               h2("Correlation and regression fundamentals with tidy data principles", style = "color:firebrick"),
                                               br(),
                                               h4("LEARNING OBJECTIVE"),
                                               p("Analyze the results of correlation tests and simple regression models for many data sets at once."),
                                               br(),
                                               h3("INTRODUCTION"),
                                               p("To use the code in this article, you will need to install the following packages: tidymodels and tidyr.
                                               While the tidymodels package broom is useful for summarizing the result of a single analysis in a consistent format,
                                                 it is really designed for high-throughput applications, where you must combine results from multiple analyses. 
                                                 These could be subgroups of data, analyses using different models, bootstrap replicates, permutations, and so on. 
                                                 In particular, it plays well with the nest()/unnest() functions from tidyr and the map() function in purrr."),
                                               br(),
                                               plotOutput("correlation")),
                                      tabPanel("K-means Clustering",
                                               h2("K-means clustering with tidy data principles", style = "color:firebrick"),
                                               br(),
                                               h4("LEARNING OBJECTIVE"),
                                               p("Summarize clustering characteristics and estimate the best number of clusters for a data set."),
                                               br(),
                                               h3("INTRODUCTION"),
                                               p("To use the code in this article, you will need to install the following packages: tidymodels and tidyr.
                                                K-means clustering serves as a useful example of applying tidy data principles to statistical analysis, 
                                                and especially the distinction between the three tidying functions:
                                                -tidy()
                                                -augment()
                                                -glance()
                                                Let's start by generating some random two-dimensional data with three clusters. 
                                                 Data in each cluster will come from a multivariate gaussian distribution, with different means for each cluster:"),
                                               br(),
                                               plotOutput("Kmeans"),
                                               br()),
                                      tabPanel("Bootstrap Resampling",
                                               h2("Bootstrap resampling and tidy regression models", style = "color:firebrick"),
                                               br(),
                                               h4("LEARNING OBJECTIVE"),
                                               p("Apply bootstrap resampling to estimate uncertainty in model parameters."),
                                               br(),
                                               br(),
                                               h3("INTRODUCTION"),
                                               p("To use the code in this article, you will need to install the following packages: tidymodels and tidyr.
                                               Combining fitted models in a tidy way is useful for performing bootstrapping or permutation tests. 
                                               These approaches have been explored before, for instance by Andrew MacDonald here, and Hadley has explored 
                                               efficient support for bootstrapping as a potential enhancement to dplyr. The tidymodels package broom fits 
                                               naturally with dplyr in performing these analyses.

                                               Bootstrapping consists of randomly sampling a data set with replacement, then performing the analysis individually
                                               on each bootstrapped replicate. The variation in the resulting estimate is then a reasonable approximation of the variance in our estimate.

                                               Let's say we want to fit a nonlinear model to the weight/mileage relationship in the mtcars data set."),
                                               br(),
                                               plotOutput("Bootstrap")
                                               ),
                                      tabPanel("Hypothesis Testing",
                                               h2("Hypothesis testing using resampling and tidy data", style = "color:firebrick"),
                                               br(),
                                               h4("LEARNING OBJECTIVE"),
                                               p("Perform common hypothesis tests for statistical inference using flexible functions."),
                                               br(),
                                               br(),
                                               h3("INTRODUCTION"),
                                               p("Throughout this vignette, we make use of gss, a data set available in infer containing 
                                                 sample of 500 observations of 11 variables from the General Social Survey."),
                                               br(),
                                               plotOutput("Hypothesis")
                                               ),
                                      tabPanel("Statistical Analysis",
                                               h2("Statistical analysis of contingency tables", style = "color:firebrick"),
                                               br(),
                                               h4("LEARNING OBJECTIVE"),
                                               p("Use tests of independence and goodness of fit to analyze tables of counts."),
                                               br(),
                                               br(),
                                               h3("INTRODUCTION"),
                                               p("In this vignette, we'll walk through conducting a X2 (chi-squared) test of 
                                               independence and a chi-squared goodness of fit test using infer. We'll start out
                                               with a chi-squared test of independence, which can be used to test the association
                                               between two categorical variables. Then, we'll move on to a chi-squared goodness of 
                                               fit test, which tests how well the distribution of one categorical variable can be 
                                               approximated by some theoretical distribution.

                                               Throughout this vignette, we'll make use of the ad_data data set (available in the 
                                                 modeldata package, which is part of tidymodels). This data set is related to cognitive
                                                 impairment in 333 patients from Craig-Schapiro et al (2011). See ?ad_data for more 
                                                 information on the variables included and their source. One of the main research questions
                                                 in these data were how a person's genetics related to the Apolipoprotein E gene affect their
                                                 cognitive skills."),
                                               br(),
                                               plotOutput("Statistical")
                                               )
                                  ),
                                  br(),
                                  img(src = "StatisticalAnalysis.png", width = 80, height = 70)
                                  
                                  
                         ),
                         tabPanel("Robust Models", "This panel is intentionally left blank"),
                         tabPanel("Tune & Compare Models", "This panel is intentionally left blank"),
                         tabPanel("Develop Modeling Tools", "This panel is intentionally left blank")
                     )
                 
                 )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$txtout <- renderText({
        paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
        head(cars, 4)
    })
    output$result <- renderText({
        paste(overviews_descriptions[input$state])
        #HTML(overviews_descriptions[input$state])
    })
    output$logo <- renderUI({
        imagen <- input$state
        imagen <- paste0(input$state,".png")
        tags$img(src=imagen,heigth=100, width=110)
        
    })
    output$correlation <- renderPlot({
        ggplot(Orange, aes(age, circumference, color = Tree)) +
            geom_line()
    })
    output$Kmeans <- renderPlot({
        set.seed(27)
        centers <- tibble(
            cluster = factor(1:3), 
            num_points = c(100, 150, 50),  # number points in each cluster
            x1 = c(5, 0, -3),              # x1 coordinate of cluster center
            x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
        )
        
        labelled_points <- 
            centers %>%
            mutate(
                x1 = map2(num_points, x1, rnorm),
                x2 = map2(num_points, x2, rnorm)
            ) %>% 
            select(-num_points) %>% 
            unnest(cols = c(x1, x2))
        
        ggplot(labelled_points, aes(x1, x2, color = cluster)) +
            geom_point(alpha = 0.3)
    })
    
    output$Bootstrap <- renderPlot({
    nlsfit <- nls(mpg ~ k / wt + b, mtcars, start = list(k = 1, b = 0))
    ggplot(mtcars, aes(wt, mpg)) +
        geom_point() +
        geom_line(aes(y = predict(nlsfit)))    
    })
    
    output$Hypothesis <- renderPlot({
        data(gss)
        # find the point estimate
        point_estimate <- gss %>%
            specify(response = hours) %>%
            calculate(stat = "mean")
        
        # generate a null distribution
        null_dist <- gss %>%
            specify(response = hours) %>%
            hypothesize(null = "point", mu = 40) %>%
            generate(reps = 5000, type = "bootstrap") %>%
            calculate(stat = "mean")
        
        null_dist %>%
            visualize() +
            shade_p_value(obs_stat = point_estimate, direction = "two_sided")
    })
    
    output$frame <- renderUI({
        my_test <- tags$iframe(src=paste0("https://", input$state,".tidymodels.org"), height=600, width=1500)
        print(my_test)
    })
    
    output$Statistical <- renderPlot({
        data(ad_data, package = "modeldata")
        # calculate the observed statistic
        observed_indep_statistic <- ad_data %>%
            specify(Genotype ~ Class) %>%
            calculate(stat = "Chisq")
        # visualize both null distributions and the test statistic!
        null_distribution_simulated <- ad_data %>%
            specify(Genotype ~ Class) %>%
            hypothesize(null = "independence") %>%
            generate(reps = 5000, type = "permute") %>%
            calculate(stat = "Chisq")
        
        null_distribution_simulated %>%
            visualize(method = "both") + 
            shade_p_value(observed_indep_statistic,
                          direction = "greater")   
    }) 
}



# Run the application 
shinyApp(ui, server)
