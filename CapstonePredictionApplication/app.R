# 
# app.R
# 
# Coursera Data Science Capstone Project
# 
# (c) 2018 Mark Smith
# 
# This Shiny Application is the front-end of my prediction system for generating next word predictions off pre-processed ngram datasets provided as part of  the course.
# 
# The data has been pre-processed to calculate probabilities on the full set of ngrams, and once this is done the data is filtered to only keep the most relevant (i.e. highest frequency) ngrams for realtime processing in the shiny platform.  Although this is not ideal, it is a necessity due to processing speed as well as limited memory available in the platform (1Gb, but this includes some overheads, so in reality a safe data set size seems to be closer to 0.5Gb)

# Load packages
loadNoisyPackages <- function() {
        library(shiny)
        library(shinythemes)
        library(data.table)
        library(dplyr)
        library(ggplot2)
}
suppressPackageStartupMessages(loadNoisyPackages())

# Load the prediciton functions
source("prediction.R")

# Only used during development
# cacheDir <- paste0(getwd(), "/../../cache")
# if (file.exists(cacheDir)) {
#         setwd(cacheDir)
# }

# Load the preprocessed datasets (indexed data.table objects)
l2bt <- readRDS("l2bt.rds")
l3bt <- readRDS("l3bt.rds")
l4bt <- readRDS("l4bt.rds")
l2nt <- readRDS("l2nt.rds")
l3nt <- readRDS("l3nt.rds")
l4nt <- readRDS("l4nt.rds")
l2tt <- readRDS("l2tt.rds")
l3tt <- readRDS("l3tt.rds")
l4tt <- readRDS("l4tt.rds")

# Main UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("JHU/Coursera Data Science Capstone Text Prediction Application by Mark Smith"),
                sidebarLayout(
                        sidebarPanel(
                                # Select Corpora
                                selectInput(inputId = "corpora", label = strong("Prediction Corpora"),
                                            choices = c("Blogs", "News", "Twitter"),
                                            selected = "Blogs"),
                                # Probability Discounts
                                sliderInput("trigramDiscount", 
                                            label = "Trigram discount/multiplier",
                                            min = 0, max = 3, value = 1, step = 0.01, round = FALSE),
                                
                                sliderInput("bigramDiscount", 
                                            label = "Bigram discount/multiplier",
                                            min = 0, max = 3, value = 1, step = 0.01, round = FALSE),
                                
                                sliderInput("uniDiscount", 
                                            label = "Unigram discount/multiplier",
                                            min = 0, max = 3, value = 1, step = 0.01, round = FALSE),
                                
                                sliderInput("maxResults", 
                                            label = "Maxiumum results displayed",
                                            min = 1, max = 20, value = 3, step = 1),

                                # Enter the prediction base
                                textInput("query", "Query String", "what is the"),

                                # Trigger
                                actionButton("predict", "Submit"),
                                
                                # Help text :)
                                p("Select the Corpora and the scaling factors (if you wish), maximum number of results, enter your Query String and press Submit.  Your results for the predicted next word with probabilities will be displayed in the right hand panel.")
                        ),
                        
                        # Output
                        mainPanel(
                                plotOutput(outputId = "plot", width = "100%", height = "400px"),
                                textOutput(outputId = "result"),
                                fluidRow(
                                        column(12,
                                               tableOutput('table')
                                        )
                                )
                        )
                )
)

# Main server function
server <- function(input, output) {
        getData <- eventReactive(input$predict, {
                # Ensure data is available
                req(input$query)
                q <- input$query
                # validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
                maxResults <- as.integer(input$maxResults)
                unigramDiscount <- as.numeric(input$bigramDiscount)
                bigramDiscount <- as.numeric(input$bigramDiscount)
                trigramDiscount <- as.numeric(input$trigramDiscount)
                
                # Call the prediction function with the correct corpora
                if (input$corpora == "Blogs") {
                        prediction <- getTrigramPrediction(q, maxResults, l2bt, l3bt, l4bt, unigramDiscount, bigramDiscount, trigramDiscount)
                } else if (input$corpora == "News") {
                        prediction <- getTrigramPrediction(q, maxResults, l2nt, l3nt, l4nt, unigramDiscount, bigramDiscount, trigramDiscount)
                } else if (input$corpora == "Twitter") {
                        prediction <- getTrigramPrediction(q, maxResults, l2tt, l3tt, l4tt, unigramDiscount, bigramDiscount, trigramDiscount)
                }
                prediction 
        })
        
        # Display a string of text showing the highest probability result
        output$result <- renderText({
                d <- getData()
                if (d[[1]][3] != "NotFound") {
                        paste0("The top prediction was \"", d[[1]][1], "\".")
                } else {
                        paste("No result was found.")
                }
        })

        # Display a table of the results
        output$table <- renderTable({
                getData()
        }, digits = 10)
        
        # Display a plot of the results sorted by probability
        output$plot <- renderPlot({
                d <- getData()
                d$Prediction <- factor(d$Prediction, levels = d$Prediction[order(d$Probability, decreasing = TRUE)])
                ggplot(data = d, aes(x=Prediction, y=Probability, fill=Type)) + geom_col()
                
        })
}

# Make the shiny happen!
shinyApp(ui = ui, server = server)
