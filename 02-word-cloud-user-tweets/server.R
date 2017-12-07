library(dplyr)
library(lubridate)
library(BH)
library(shiny) 
library(tm)
library(wordcloud)
library(twitteR)
getWordCloud <- function(userName, tweetCount) {
  oauth <- setup_twitter_oauth(consumer_key = "QfiOGnODt8pYMn3z1IrTAs5MO", consumer_secret = "DtBFlRF01Isx9KujtJZsQ9xRJxDuE2EPP07rUIg5brHX8coXfM",access_token = "125590512-0JbNkV9v2ssLCDgU4k7KrVxnjLJTBBuCoo4lRYDv", access_secret = "cJ9GONkaF5ysWSaR1qi7HZwEyeN6bAYfXtyY2vFYJtN7M")
  myTweets<-userTimeline(userName, n=1000)
  set.seed(1234) # to always get the same wordcloud and for better reproducibility
  tweetTexts<-unlist(lapply(myTweets, function(t) { t$text})) # to extract only the text of each status object
  words<-unlist(strsplit(tweetTexts, " "))
  words<-tolower(words)
  toMatch<-c("http","@","#")
  clean_words <- words[-grep(paste(toMatch,collapse="|"), words)] # remove urls, usernames, hashtags and umlauts (the latter can not be displayed by all fonts)
  print(clean_words)
  wrdCloud <- wordcloud(paste(clean_words, collapse=" "), min.freq = 0, random.color=TRUE , max.words=100 ,colors=brewer.pal(8, "Dark2"))
  return(wrdCloud)  
}

shinyServer(
  function(input, output) {
    output$userName <- renderPrint({input$userName})
    tweets <- reactive({
      req(input$update)
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getWordCloud(input$userName, input$tweetCount)
        })
      })
    })
    output$Clean_words <- renderPlot({
      tweets()
      })
  }
)

