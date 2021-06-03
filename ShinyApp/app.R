# app.R
# Author: Maroje Raguž
# Date: 01-Jul-2021
# Description: Shiny UI, Coursera Data Science Capstone Final Project
# GitHub: https://github.com/MRHub13/DSS-Capstone-Project

# ## Shiny App 
# This script creates a Shiny App that predicts next word based on the input of a word or phrase 


library(shiny)
library(shinythemes)
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

# ... sourcing ngram matching function
source("ngram.R")

# ... defining UI for app ... theme & the front page 

ui <- fluidPage(theme = shinytheme("superhero"),

  # ... app title
  titlePanel("'Next Word' ... Prediction Application"),
  tags$b(p("This app predicts the next word based on a word or phrase you have entered in a text box.")),
  
  # ... sidebar 
  sidebarLayout(
    sidebarPanel(
      tags$em(h2("Instructions:")), 
      tags$em(h5("1. Enter one word or phrase in the text box - no need to hit enter to submit.")),
      tags$em(h5("2. The predicted next word will be shown in green letters @ row beneath.")),
      tags$em(h5("3. A question mark means no prediction - due to limits of sample dictionary (size vs. speed trade-off) used by app or misspelling.")),
      tags$em(h5("4. Additional tabs show plots of the top 10 'n-grams' (from 'bi' to 'six' grams) generated from the sample dataset.")),
      tags$em(h2("")),
      tags$em(h2("")),
      tags$em(h2("")),
      tags$em(h2("")),
      tags$b(tags$em("Note:", style="color:lightgreen")),
      tags$em(("")),
      tags$em(("")),
      tags$em("The app 'Next Word' can predict up to six consecutive words - the app may start to repeat previous prediction pattern when entering more than five consecutive words.", style="color:lightgreen"),
      br(),
      tags$em(h2("")),
      a("Source Code", href = "https://github.com/MRHub13/DSS-Capstone-Project")
    ),
    
    # ... mainpanel & tabpanels
    mainPanel(
      tabsetPanel(
        tabPanel(tags$b(tags$em("PREDICTING")),
                 textInput("user_input",
                           tags$em(h3("Enter a word or phrase:")), 
                           value = "",
                           placeholder = "Enter text here"),
                 tags$em(h4("Predicted Next Word:")),
                 h4(em(span(textOutput("ngram_output"), style="color:lightgreen")))),
        
        tabPanel(tags$b(tags$em("top 10 bigrams")),
                 br(),
                 img(src = "bigrams.png", height = 400, width = 560)),
        
        tabPanel(tags$b(tags$em("top 10 trigrams")),
                 br(),       
                 img(src = "trigrams.png", height = 400, width = 560)),
        
        tabPanel(tags$b(tags$em("top 10 quadgrams")),
                 br(),
                 img(src = "quadgrams.png", height = 400, width = 560)),
        
        tabPanel(tags$b(tags$em("top 10 quintgrams")),
                 br(),
                 img(src = "quintgrams.png", height = 400, width = 560)),
        
        tabPanel(tags$b(tags$em("top 10 sextgrams")),
                 br(),
                 img(src = "sextgrams.png", height = 400, width = 560))
      )   
    )
  )
)
# ... defining a server logic required for interaction with user
server <- function(input, output) {
  
  output$ngram_output <- renderText({
    ngrams(input$user_input)
  })
  
}

# ... running the application 
shinyApp(ui = ui, server = server)
