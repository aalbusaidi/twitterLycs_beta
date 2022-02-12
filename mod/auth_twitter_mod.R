

twitAuth_UI <- function(id,url) {
  ns <- NS(id)
  div(style="width:100vw; height:100vh;",
      div(style="height:10vh; width:100vw; display:flex; color:#fff;",
          div(style="flex:1;background-color:#fff;display:flex; justify-content:center; align-items:center;color:#333;",
              h1("Welcome")
              ),
          div(style="flex:1;background-color:#0F9AFB;display:flex; justify-content:center; align-items:center;",
              h1("TwitterLycs")
          )
          ),
      div(style=" width:100vw;display:flex; justify-content:center; align-items:center;",
          div(style="width:50vw;background-color:#333;display:flex; justify-content:center; align-items:center;height:90vh;background: url(compBg.png);background-repeat: no-repeat;
  background-size: auto;",
              img(src="twitter_white.svg",width="80%")
          ),
          div(style="width:50vw;background-color:#fff;height:90vh;color:#333;box-sizing:border-box; padding:50px; display:flex; flex-direction:column; justify-content:center;",
              img(src="twitter.png",width="70px"),
              h1("Authorize with Twitter",style="font-size:2em;"),
              a(href = url, actionBttn(inputId = "xx","Authorize",style = 'pill',color = 'primary'))
          )
          
      ),
      )
}

twitAuth <- function(input, output, session) {
  
}