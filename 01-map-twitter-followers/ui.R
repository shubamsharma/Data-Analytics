library(shiny)
library(leaflet)

shinyUI(pageWithSidebar(
  headerPanel('Plot My Twitter Follower'),
  sidebarPanel(
    textInput("userName", "User Name", "Enter User"),
    actionButton("update", "Submit")
      ),
  mainPanel(
    p('This shiny app will Will plot user twitter follower on wolrd map'),
    h4('Selected user:'),
    verbatimTextOutput('userName'),
    h4('Twitter Followers'),
    leafletOutput("myMap")
  )
))