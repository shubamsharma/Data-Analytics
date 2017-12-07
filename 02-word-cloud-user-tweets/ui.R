shinyUI(pageWithSidebar(
  headerPanel('Recent Tweets WordCloud'),
  sidebarPanel(
    textInput("userName", "User Name", "Enter User"),
    textInput("tweetCount", "Count", "No Of Tweets"),
    actionButton("update", "Change")
      ),
  mainPanel(
    p('This shiny app will show you the word Cloud of the user tweets'),
    h4('Selected user:'),
    verbatimTextOutput('userName'),
    h4('WordCloud'),
    plotOutput("Clean_words") #Cloud for positive words
  )
))