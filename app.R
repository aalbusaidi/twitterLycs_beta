library(shiny)
library(shinyWidgets)
library(rtweet)
library(twitteR)
library(DT)
library(dplyr)
library(lubridate)
library(graphTweets)
library(httr)
library(plotly)
library(networkD3)
library(ggplot2)
library(ggthemes)
library(stringr)
library(tm)
library(igraph)
library(bs4Dash) #reListed
library(xlsx) #reListed
library(plotly) #reListed
library(plyr) #reListed

source("twitter.R")
source("mod/auth_twitter_mod.R")
source("mod/dash_mod.R")

useCss<-function(file="style.css"){
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = file)
  )
}

ui <- div(
  useCss(file = 'style.css'),
  tags$head(HTML('<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Poppins:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap" rel="stylesheet">')),
 uiOutput("main") 
)

server <- function(input, output, session) {
  r<-reactiveValues(q=NULL)
  
  query <- reactive({getQueryString(session)})
  
  observeEvent(query(),{
    if(!is.null(query()$oauth_token)){
      r$q<-query()
    }else{
      r$q<-NULL
    }
  })
  
  user_token <- reactive({
    if (!is.null(r$q$oauth_token)) {

      access_token <-
        get_access_token(app, query()$oauth_token, query()$oauth_verifier)

      # Commenting below to match twitter
      user_token <- create_token(
        app = "",
        keys['consumer_key'],
        keys['consumer_secret'],
        access_token = access_token$oauth_token,
        access_secret = access_token$oauth_token_secret
      )
      user_token
      
      # list(oauth_token=access_token$oauth_token,oauth_token_secret=access_token$oauth_token_secret)
    } else{
      NULL
    }
  })
  
observe({
    if (is.null(user_token())) {
      
      output$main <- renderUI({
        twitAuth_UI('lv0', url = url)
      })
      
      callModule(twitAuth, id = 'lv0')
      
    } else{
      

      
      output$main <- renderUI({
        dashTweet_UI(id='lv1')
      })
      
      callModule(dashTweet,"lv1",user_token=user_token,keys=keys)
    }
    
})
}

shinyApp(ui, server,options=list(port=8080))