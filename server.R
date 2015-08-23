# server.R

library(shiny)
library(tm)
library(stringr)
library(R.utils)
#options(shiny.trace=TRUE)

source("predictWord.R")

# read counts of n-grams
uniCount<-readRDS(file="uniCount.rds")
biCount<-readRDS(file="biCount.rds")
triCount<-readRDS(file="triCount.rds")
quadriCount<-readRDS(file="quadriCount.rds")
vacab<-readRDS(file="vacab.rds")


shinyServer(
  
  function(input,output) { 
      
      wordFreq<-reactive({
        
        text<-input$text
        ngram<-input$ngram
        discount<-input$discount
        hs<-predictWord(text,uniCount,biCount,triCount,quadriCount,vacab,
                        ngram,discount)  
        
      })

      
      output$predictedWord<-renderText({
        maxcol<-ncol(wordFreq())
        if ( maxcol>1 ) wordFreq()$word[1]
      })
      
      output$hist <- renderPlot({
        nword<-input$numPredicted
        maxcol<-ncol(wordFreq())
        if ( maxcol>1 ){
          barplot(wordFreq()[1:nword,maxcol],
                  names.arg=wordFreq()[1:nword,]$word,col="green",
                  ylab='Probability of word in Katz backoff model',
                  las=2,
                  main='Histogram of Possible Words')
        }
      })      
    
  }

)