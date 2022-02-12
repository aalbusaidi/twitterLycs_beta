
keys<-c(
  consumer_key='', #enter your consumer key here
  consumer_secret='' # enter your consumer secret here
)


app <- oauth_app(
  app = "R_Package",
  key = keys['consumer_key'],
  secret = keys['consumer_secret']
)

oauth_sig <- function(url, method, token = NULL, token_secret = NULL, private_key = NULL, ...){
  httr::oauth_header(
    httr::oauth_signature(url, 
                          method,
                          app,
                          token,
                          token_secret, 
                          private_key,
                          other_params = list(...)))
}


get_authorization_url <- 
  function(app, callback_url, permission=NULL){
    private_key <- NULL
    response <- 
      httr::POST(
        "https://api.twitter.com/oauth/request_token",
        oauth_sig(
          "https://api.twitter.com/oauth/request_token",
          "POST", 
          private_key = NULL,
          oauth_callback = callback_url)
      )
    httr::stop_for_status(response)
    params <- 
      httr::content(
        response, 
        type = "application/x-www-form-urlencoded")
    authorize_url <- 
      httr::modify_url(
        "https://api.twitter.com/oauth/authenticate",
        query = list(oauth_token = params$oauth_token, 
                     permission = permission))
    authorize_url 
  }



get_access_token <- 
  function(app, 
           oauth_token,
           oauth_verifier){
    url <- 
      paste0(
        "https://api.twitter.com/oauth/access_token?oauth_token=",
        oauth_token, "&oauth_verifier=", oauth_verifier)
    response <- 
      httr::POST(url, 
                 oauth_sig(url,
                           "POST",
                           private_key = NULL))
    if(response$status_code == 200L){
      results <- 
        httr::content(
          response,
          type = "application/x-www-form-urlencoded",
          encoding = "UTF-8")
      
      # since storing the username might be creepy
      results[["screen_name"]] <- NULL 
      
      # since storing the user id might be creepy
      results[["user_id"]] <- NULL     
      
      results
    } else {
      NULL
    }
  }

url <- get_authorization_url(app, "http://127.0.0.1:8080") #insert this url into your app's callback url 
