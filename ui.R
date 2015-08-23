# ui.R for Coursera Data Science Capstone Final Project

library(shiny)
library(markdown)

shinyUI( fluidPage(
  
  headerPanel ("Coursera Data Science Capstone Final Project"),
  titlePanel("Word Prediction by Ngram Model with Katz Backoff Smoothing Based on Swiftkey Dataset"),
  br(),
  br(),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Please enter an incomplete phrase/sentence (English only) for the next word prediction."),
      textInput(inputId="text",label="",value="How are you"), 
      
      submitButton("submit"),      

      sliderInput(inputId="numPredicted",label="Number of possible words to show in the histogram:", 
                  min=5,max=20,value=5),    
          
      radioButtons("ngram", 
                   h5("maximum ngram used for prediction (Default is quadrigram)"),
                   choices=list("bigram"=2, "trigram"=3, "quadrigram"=4),
                   selected=4),
      
      sliderInput(inputId="discount",label=h5("discount in Katz backoff model:"), 
                  min=0.05,max=0.75,value=0.5)      
      
    ), 
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
        
        tabPanel("Prediction",
                  h3("The predicted word is:"),
                  h3(textOutput("predictedWord"),style="color:red", align='center'),                 
                  plotOutput('hist')
        ),        
              
        tabPanel("About",                  
                 includeMarkdown("about.Rmd")
        )
        
      )
    )
    
  )  
))