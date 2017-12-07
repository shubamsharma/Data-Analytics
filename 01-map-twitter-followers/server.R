library(dplyr)
library(lubridate)
library(BH)
library(shiny) 
library(tm)
library(wordcloud)
library(twitteR)
library(rjson)
library(leaflet)

if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}
geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyBIO6BjdMiVwfLI13LziyxPK9CDV9ITRgM")
}
getTwitterFollowers <- function(userName, tweetCount) {
  oauth <- setup_twitter_oauth(consumer_key = "QfiOGnODt8pYMn3z1IrTAs5MO", consumer_secret = "DtBFlRF01Isx9KujtJZsQ9xRJxDuE2EPP07rUIg5brHX8coXfM",access_token = "125590512-0JbNkV9v2ssLCDgU4k7KrVxnjLJTBBuCoo4lRYDv", access_secret = "cJ9GONkaF5ysWSaR1qi7HZwEyeN6bAYfXtyY2vFYJtN7M")
  myUser <- getUser(userName)
  location(myUser)
  myUser_follower_IDs <- myUser$getFollowers(retryOnRateLimit=180)
  length(myUser_follower_IDs)
  myUser_followers_df = rbindlist(lapply(myUser_follower_IDs,as.data.frame))
  head(myUser_followers_df$location, 10)
  myUser_followers_df<-subset(myUser_followers_df, location!="")
  head(myUser_followers_df$location, 10)
  myUser_followers_df$location<-gsub("%", " ",myUser_followers_df$location)

  geocode_results<-sapply(myUser_followers_df$location, geocode_apply, simplify = F)
  length(geocode_results)
  
  
  condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
  geocode_results<-geocode_results[condition_a]
  
  condition_b <- lapply(geocode_results, lapply, length)
  condition_b2<-sapply(condition_b, function(x) x["results"]=="1")
  geocode_results<-geocode_results[condition_b2]
  length(geocode_results)
  
  for(i in 1:length(geocode_results)){
    dynamic_j<-length(geocode_results[[i]]$results[[1]]$address_components)
    for(j in 1:dynamic_j){
      if(length(geocode_results[[i]]$results[[1]]$address_components[[j]]$types)>2){
        geocode_results[[i]]$results[[1]]$address_components[[j]]$types<-geocode_results[[i]]$results[[1]]$address_components[[j]]$types[(length(geocode_results[[i]]$results[[1]]$address_components[[j]]$types)-1):length(geocode_results[[i]]$results[[1]]$address_components[[j]]$types)]
      }
    }
    if(length(geocode_results[[i]]$results[[1]]$types)>2){
      geocode_results[[i]]$results[[1]]$types<-geocode_results[[i]]$results[[1]]$types[(length(geocode_results[[i]]$results[[1]]$types)-1):length(geocode_results[[i]]$results[[1]]$types)]
    }
    if(length(geocode_results[[i]]$results[[1]]$types)<1){
      geocode_results[[i]]$results[[1]]$types<-"Unknown"
    }
    dynamic_k<-length(geocode_results[[i]]$results[[1]]$address_components)
    for(k in 1:dynamic_k){
      if(length(geocode_results[[i]]$results[[1]]$address_components[[k]]$types)<1){
        geocode_results[[i]]$results[[1]]$address_components[[k]]$types<-"Unknown"
      }
    }
    if(length(geocode_results[[i]]$results[[1]]$postcode_localities)>2){
      geocode_results[[i]]$results[[1]]$postcode_localities<-geocode_results[[i]]$results[[1]]$postcode_localities[(length(geocode_results[[i]]$results[[1]]$postcode_localities)-1):length(geocode_results[[i]]$results[[1]]$postcode_localities)]
    }
  }
  
  results_b<-lapply(geocode_results, as.data.frame)
  
 #results_c<-lapply(results_b,function(x) subset(x, select=c("results.formatted_address",
#                                                             "results.geometry.location")))
  
  results_d<-lapply(results_b,function(x) data.frame(Location=x[1,"results.formatted_address"],
                                                     lat=x[1,"results.geometry.location.lat"],
                                                     lng=x[2,"results.geometry.location.lng"]))
  
  results_e<-rbindlist(results_d)
  
  results_f<-results_e[,Original_Location:=names(results_d)]
  
  india_results<-subset(results_f,
                           grepl(", ", results_f$Location)==TRUE)
  
  india_results$commas<-sapply(india_results$Location, function(x)
    length(as.numeric(gregexpr(",", as.character(x))[[1]])))
  india_results<-subset(india_results, commas==2)
  #Drop the "commas" column:
  india_results<-subset(india_results, select=-commas)
  
  nrow(india_results)
  
  return(india_results)

  map <- leaflet() %>% addTiles() %>%
  addMarkers(india_results$lng, india_results$lat, popup = india_results$loaction) %>%
  setView(india_results$lng, india_results$lat, zoom = 11)
  

#Generate a blank map:
  albers_proj<-map("state", proj="albers", param=c(39, 45), 
                   col="#999999", fill=FALSE, bg=NA, lwd=0.2, add=FALSE, resolution=1)
  #Add points to it:
  points(mapproject(india_results$lng, india_results$lat), col=NA, bg="#00000030", pch=21, cex=1.0)
  #Add a title:
  mtext("The Geography of @LucasPuente's Followers", side = 3, line = -3.5, outer = T, cex=1.5, font=3)  
  
  
    
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
    #getTwitterFollowers(input$userName, input$tweetCount)
    tweets <- reactive({
      req(input$update)
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          
          india_results <- getTwitterFollowers(input$userName, 0)
          
          map <- leaflet() %>% addTiles() %>%
          addMarkers(india_results$lng, india_results$lat, popup = india_results$location) %>%
          setView(india_results$lng, india_results$lat, zoom = 2)
          
        })
      })
    })
    output$myMap <- renderLeaflet({
      tweets()
      })
  }
)

