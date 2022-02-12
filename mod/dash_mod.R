###------------Global-----------------------#
pos.words <- scan('data/pos.wordsAr5.txt', sep = "\n", what='character',encoding = 'UTF-8')
neg.words <- scan('data/neg.wordsAr5.txt', sep = "\n", what='character',encoding = 'UTF-8')
stop.wordsAr <- scan('data/stop.wordsAr.txt', sep = "\n", what='character',encoding = 'UTF-8')


#function for sentiment analysis 
score.sentiment <- function(sentences, pos.words, neg.words)
{
  scores <- lapply(sentences, function(sentence, pos.words, neg.words){
    # remove retweet entities
    sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",  sentence)
    #remove at people
    sentence = gsub("@\\w+", "",   sentence)
    #remove punctuation
    sentence = gsub("[[:punct:]]", "",   sentence)
    #remove numbers
    sentence = gsub("[[:digit:]]", "",   sentence)
    #remove html links
    sentence = gsub("http\\w+", "",   sentence)
    #remove unnecessary spaces
    sentence = gsub("[ \t]{2,}", "",   sentence)
    sentence = gsub("^\\s+|\\s+$", "",   sentence)
    #Lowercase all words for convenience
    sentence <- tolower(  sentence)
    #Remove words with more than 3 numbers in them (they overwhelm the corpus, and are uninformative)
    sentence <- gsub("[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "",   sentence)
    #Remove all newline characters
    sentence <- gsub("[\r\n]", "",   sentence)
    #word.list <- str_split(sentence,'\s+')
    word.list <-lapply(sentence,function(x)strsplit(x[1]," "))
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words)
  
  scores<-do.call(rbind,scores)
  
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}



dashTweet_UI <- function(id) {
  ns <- NS(id)
  source("mod/bs4elem.R",local = T)
  bs4DashPage(header = head,sidebar = side,body = body,controlbar = NULL,title = "TwitterLycs",dark = TRUE,fullscreen = T)
}

dashTweet <- function(input, output, session,user_token,keys) {
  observe(print(keys))
  
  
  #authonticating API, Option A
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #keys from R_Package app 
  
  # consumer_key <- keys['consumer_key']
  # consumer_secret <- keys['consumer_secret']
  # # access_token <- ""
  # # access_secret <- ""
  # access_token <- reactive({user_token()['oauth_token']})
  # access_secret <- reactive({user_token()['oauth_token_secret']})
  # 
  # setup_twitter_oauth(consumer_key, consumer_secret,access_token(), access_secret())
  
  
  data <- isolate({eventReactive(input$update, {
    #$$$$
    withProgress({
      #setProgress(message = paste("Collecting:", input$keyword," "))
      incProgress(amount = 0.1, message = paste("Keep Calm While Collecting:", input$keyword," "), detail = NULL,
                  session = getDefaultReactiveDomain())
      #Stream tweets and create a df
      
      # tweets<-searchTwitter(input$keyword, input$lang, n=input$num)
      
      tweets<-search_tweets(q = input$keyword,n = as.numeric(input$num),token = user_token(),lang = input$lang)
      # tweets1 <- twListToDF(tweets)
      # tweets1<-tweets
      tweets$text<-iconv(enc2utf8(tweets$text),sub = "byte")
      # tweets2 <- data.frame(tweets1)
      tweets
      
    })
  })
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  observe({
    saveRDS(data(),"xx.Rds")
  })
  
  output$dat <- DT::renderDataTable({
    d <- data()
    print(names(d))
    tabl <- data.frame(User=d$screen_name, Tweet=d$text,Retweets=d$retweet_count,Favorites=d$favorite_count,Created=d$created_at)
    DT::datatable(tabl)
  })  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  #download df
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      #write.csv(data(), file)
      write.xlsx(data(), file, sheetName="Sheet1",
                 col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
    }
  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #withProgress({
  #setProgress(message = paste("Collecting:", input$keyword," "))
  #incProgress(amount = 0.1, message = paste("Keep Calm While Analyzing:", input$keyword," "), detail = NULL,
  #           session = getDefaultReactiveDomain())
  output$allExtracted <- renderInfoBox({
    infoBox(
      #count reTweets
      paste("Total Extracted of", input$keyword," "), paste("Counts:",nrow(data()), " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "primary")
    
  })  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #creating infoboxoutput for the retweets
  # Same as above, but with fill=TRUE
  output$original <- renderInfoBox({
    infoBox(
      #count Original
      paste("Original Tweets of", input$keyword," "),
      paste("Counts:",nrow(data()[data()$is_retweet=="FALSE",]), " "),
      icon = icon("twitter", lib = "font-awesome"),fill = TRUE,color = "purple")
  }) 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #creating infoboxoutput for the retweets
  # Same as above, but with fill=TRUE
  output$retweets <- renderInfoBox({
    infoBox(
      paste("Total Retweets of", input$keyword," "),
      paste("Counts:",nrow(data()[data()$is_retweet=="TRUE",]), " "),
      icon = icon("retweet", lib = "font-awesome"),fill = TRUE,color = "success")
  }) 
  #~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #creating infoboxoutput for number of unique users
  output$users <- renderInfoBox({
    infoBox(
      paste("Unique Users of", input$keyword," "),
      paste("Counts:",nrow(unique(data.frame(data()$screen_name))), " "),
      icon = icon("users", lib = "font-awesome"),fill = TRUE,color = "warning")
  }) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$topWords <- DT::renderDataTable({
    some_txt <- data()$text
    
    some_txt <- data()$text
    # remove retweet entities
    some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    # remove at people
    some_txt = gsub("@\\w+", "", some_txt)
    # remove punctuation
    some_txt = gsub("[[:punct:]]", "", some_txt)
    # remove numbers
    some_txt = gsub("[[:digit:]]", "", some_txt)
    # remove html links
    some_txt = gsub("http\\w+", "", some_txt)
    some_txt <- gsub('\\d+', "", some_txt)
    # remove unnecessary spaces
    some_txt = gsub("[ \t]{2,}", "", some_txt)
    some_txt = gsub("^\\s+|\\s+$", "", some_txt) #removing trailling whitespace
    # Lowercase all words for convenience
    some_txt <- tolower(some_txt)
    # Remove words with more than 3 numbers in them (they overwhelm the corpus, and are uninformative)
    #some_txt <- gsub("[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "", some_txt)
    # Remove all newline characters
    some_txt <- gsub("[\r\n]", "", some_txt)
    # Replace whitespace longer than 1 space with a single space
    some_txt <- gsub(" {2,}", " ", some_txt)
    
    
    
    # Regex pattern for removing stop words
    sw <- c(stopwords("english"),stop.wordsAr)
    stop_pattern <- paste0("\\b(", paste0(sw, collapse="|"), ")\\b")
    #tm_map(abs, removeWords, c(stopwords("english"),"my","custom","words")) 
    some_txt <- gsub(stop_pattern, "", some_txt)
    
    # Replace whitespace longer than 1 space with a single space
    some_txt <- gsub(" {2,}", " ", some_txt)
    some_txt = gsub("^\\s+|\\s+$", "", some_txt) #removing trailing whitespace
    some_txt = gsub("^\\s+", "", some_txt) #removing leading whitespace
    some_txt <- str_trim(some_txt)
    
    
    # Split on spaces and return list of character vectors
    some_txt <- str_split(some_txt, '\\s+')
    some_txt <- unlist(some_txt)
    some_txt <- str_trim(some_txt)
    
    corpusfreq <- data.frame(table(some_txt))
    names(corpusfreq) <- c("Word", "Freq")
    corpusfreq$Word <- as.character(corpusfreq$Word)
    corpusfreq$Freq <- as.numeric(corpusfreq$Freq)
    corpusfreq <- corpusfreq[order(-corpusfreq$Freq), ]
    corpusfreq <- corpusfreq[1:20,]
    topW <- data.frame(Word=corpusfreq$Word, Frequncy=corpusfreq$Freq)
    DT::datatable(topW, style = "bootstrap")
  })  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  #------------------------------
  
  output$plotTime <- renderPlot({
    
    # Put in local time from library(lubridate)
    mydata <- data()
    mydata$created_at = with_tz(mydata$created_at, 'Asia/Muscat') # will reflect GMT+4 Oman time
    
    g <- ggplot(mydata, aes(created_at)) +
      geom_density(aes(fill = is_retweet), alpha = .5) +
      ggtitle("When Do People Usually Tweet GMT+4")+
      scale_fill_discrete("Type of Tweet", labels=c("Original", "Retweet"))
    
    g + theme_economist() + scale_colour_economist() 
    
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$device <- renderPlot({
    mydata <- data()$source
    
    # mydata = substr(mydata, 
    #                 regexpr('>', mydata) + 1, #to remove url
    #                 regexpr('</a>', mydata) - 1) #to remove url
    platforms <- mydata
    ## Top Active Users whether they tweet, retweet or reply to others
    platforms <- platforms[!is.na(platforms)]
    platforms <- as.data.frame(table(platforms))
    colnames(platforms) <- c("Platform", "Freq")
    platforms <- platforms[order(platforms$Freq, decreasing=T), ]
    platforms <- platforms[1:10,]
    #platforms <- subset(platforms, user!="Omantel")
    
    
    g <- ggplot(platforms) + geom_bar(aes(Platform, Freq), fill = "lightslategray", stat="identity") + 
      ylab("Frequency") + xlab("Top Used Devices") +
      coord_flip() + theme_bw() + ggtitle("Top 10 Used Platforms")
    g+theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold"))
    
    
  })
  #}) #closing progress
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sna <- renderForceNetwork({
    
    withProgress({
      #setProgress(message = paste("Collecting:", input$keyword," "))
      incProgress(amount = 0.1, message = "Keep Calm, Wait & Smile", detail = NULL,
                  session = getDefaultReactiveDomain())
      mydata <- data()
      sp = split(mydata, mydata$is_retweet)
      rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))
      el = as.data.frame(cbind(sender = tolower(rt$sender), receiver = tolower(rt$screen_name)))
      el = dplyr::count(el, sender, receiver,sort = TRUE)
      #write.csv(el,"edgeList.csv")
      #build a retweet network 
      #rt_graph <- igraph::graph_from_data_frame(d=el, directed=T)
      #wc <- cluster_walktrap(rt_graph)
      #members <- membership(wc)
      #d3_rt <- igraph_to_networkD3(rt_graph, group = members)
      #forceNetwork(Links = d3_rt$links, Nodes = d3_rt$nodes, 
      #             Source = 'source', Target = 'target', 
      #             NodeID = 'name', Group = 'group', Value = "value",
      #             arrows = TRUE, zoom = TRUE, fontSize = 24,
      #             bounded = TRUE)
      ##############################################################
      #http://www.vesnam.com/Rblog/viznets6/
      edgeList1 <- el
      colnames(edgeList1) <- c("SourceName", "TargetName", "Weight")
      
      # Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
      gD1 <- igraph::simplify(igraph::graph.data.frame(edgeList1, directed=TRUE))
      
      # Create a node list object (actually a data frame object) that will contain information about nodes
      # because networkD3 library requires IDs to start at 0
      nodeList1 <- data.frame(ID = c(0:(igraph::vcount(gD1) - 1)),nName = igraph::V(gD1)$name)
      
      # Map node names from the edge list to node IDs
      getNodeID1 <- function(x){
        which(x == igraph::V(gD1)$name) - 1 # to ensure that IDs start at 0
      }
      
      # And add them to the edge list
      edgeList1 <- plyr::ddply(edgeList1, .variables = c("SourceName", "TargetName", "Weight"), 
                               function (x) data.frame(SourceID = getNodeID1(x$SourceName), 
                                                       TargetID = getNodeID1(x$TargetName)))
      
      
      
      # Calculate degree for all nodes
      nodeList1 <- cbind(nodeList1, nodeDegree=igraph::degree(gD1, v = igraph::V(gD1), mode = "all"))
      
      # Calculate betweenness for all nodes
      betAll1 <- igraph::betweenness(gD1, v = igraph::V(gD1), directed = FALSE) / (((igraph::vcount(gD1) - 1) * (igraph::vcount(gD1)-2)) / 2)
      betAll1.norm <- (betAll1 - min(betAll1))/(max(betAll1) - min(betAll1))
      #nodeList1 <- cbind(nodeList1, nodeBetweenness=100*betAll1.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
      
      # Calculate membership for all node
      memb1 <- igraph::membership(cluster_walktrap(gD1))
      
      #add betweeness and membership to the nodeList1
      nodeList1 <- cbind(nodeList1,members= as.numeric(memb1), nodeBetweenness=100*betAll1.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
      rm(betAll1, betAll1.norm, memb1)
      
      #Calculate Dice similarities between all pairs of nodes
      dsAll1 <- igraph::similarity.dice(gD1, vids = igraph::V(gD1), mode = "all")
      
      F1a <- function(x) {data.frame(diceSim = dsAll1[x$SourceID +1, x$TargetID + 1])}
      edgeList1 <- plyr::ddply(edgeList1, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"), 
                               function(x) data.frame(F1a(x)))
      
      rm(dsAll1, F1a, getNodeID1, gD1)
      
      #NA: We will also create a set of colors for each edge, based on their dice similarity values
      #creating edge colors based on membership
      F2a <- colorRampPalette(c("#ff00ff", "#00ff80"), bias = nrow(edgeList1), space = "rgb", interpolate = "linear")
      colCodes1 <- F2a(length(unique(edgeList1$diceSim)))
      edges_col1 <- sapply(edgeList1$diceSim, function(x) colCodes1[which(sort(unique(edgeList1$diceSim)) == x)])
      
      rm(colCodes1, F2a)
      
      # Let's create a network
      
      D3_network_RT <- networkD3::forceNetwork(Links = edgeList1, # data frame that contains info about edges
                                               Nodes = nodeList1, # data frame that contains info about nodes
                                               Source = "SourceID", # ID of source node 
                                               Target = "TargetID", # ID of target node
                                               Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                               NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                               Nodesize = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for a node size
                                               Group = "members",  # value from the node list (data frame) that contains value we want to use for node color
                                               height = 500, # Size of the plot (vertical)
                                               width = 800,  # Size of the plot (horizontal)
                                               fontSize = 18, # Font size
                                               linkDistance = networkD3::JS("function(d) { return 35*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                               linkWidth = networkD3::JS("function(d) { return d.value/2; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                               opacity = 0.85, # opacity
                                               zoom = TRUE, # ability to zoom when click on the node
                                               opacityNoHover = 0.1, # opacity of labels when static
                                               linkColour = edges_col1, # edge colors
                                               bounded=FALSE, #boundedin border
                                               arrows = TRUE) #directed
      # Plot network
      D3_network_RT 
      
    })
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #forced directed network
  output$sna2 <- renderForceNetwork({
    mentionData <- data()
    sp2 = split(mentionData, mentionData$is_retweet)
    orig = sp2[['FALSE']]
    
    mentioned = 
      lapply(orig$text, function(tx) {
        matches = gregexpr('@[^([:blank:]|[:punct:])]+', tx)[[1]]
        sapply(seq_along(matches), function(i) 
          substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
      })
    
    mentionEL = 
      lapply(seq_along(orig$text), function(i) {
        if(mentioned[[i]] == '')  
          return(NULL)
        lapply(mentioned[[i]], function(m)
          c(sender = as.character(orig$screen_name[i]), receiver = m)) %>%
          do.call(rbind, .) %>% as.data.frame()
      }) %>% 
      do.call(rbind, .) %>%
      dplyr::count(tolower(sender), tolower(receiver))
    ###########
    #http://www.vesnam.com/Rblog/viznets6/
    edgeList2 <- mentionEL
    colnames(edgeList2) <- c("SourceName", "TargetName", "Weight")
    
    # Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
    gD2 <- igraph::simplify(igraph::graph.data.frame(edgeList2, directed=TRUE))
    
    # Create a node list object (actually a data frame object) that will contain information about nodes
    # because networkD3 library requires IDs to start at 0
    nodeList2 <- data.frame(ID = c(0:(igraph::vcount(gD2) - 1)),nName = igraph::V(gD2)$name)
    
    # Map node names from the edge list to node IDs
    getNodeID2 <- function(x){
      which(x == igraph::V(gD2)$name) - 1 # to ensure that IDs start at 0
    }
    
    # And add them to the edge list
    edgeList2 <- plyr::ddply(edgeList2, .variables = c("SourceName", "TargetName", "Weight"), 
                             function (x) data.frame(SourceID = getNodeID2(x$SourceName), 
                                                     TargetID = getNodeID2(x$TargetName)))
    
    
    
    # Calculate degree for all nodes
    nodeList2 <- cbind(nodeList2, nodeDegree=igraph::degree(gD2, v = igraph::V(gD2), mode = "all"))
    
    # Calculate betweenness for all nodes
    betAll2 <- igraph::betweenness(gD2, v = igraph::V(gD2), directed = FALSE) / (((igraph::vcount(gD2) - 1) * (igraph::vcount(gD2)-2)) / 2)
    betAll2.norm <- (betAll2 - min(betAll2))/(max(betAll2) - min(betAll2))
    #nodeList2 <- cbind(nodeList2, nodeBetweenness=100*betAll2.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
    
    # Calculate membership for all node
    memb2 <- igraph::membership(cluster_walktrap(gD2))
    
    #add betweeness and membership to the nodeList2
    nodeList2 <- cbind(nodeList2,members= as.numeric(memb2), nodeBetweenness=100*betAll2.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
    rm(betAll2, betAll2.norm, memb2)
    
    #Calculate Dice similarities between all pairs of nodes
    dsAll2 <- igraph::similarity.dice(gD2, vids = igraph::V(gD2), mode = "all")
    
    F1b <- function(x) {data.frame(diceSim = dsAll2[x$SourceID +1, x$TargetID + 1])}
    edgeList2 <- plyr::ddply(edgeList2, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"), 
                             function(x) data.frame(F1b(x)))
    
    rm(dsAll2, F1b, getNodeID2, gD2)
    
    #NA: We will also create a set of colors for each edge, based on their dice similarity values
    #creating edge colors based on membership
    F2b <- colorRampPalette(c("#ff00ff", "#00ff80"), bias = nrow(edgeList2), space = "rgb", interpolate = "linear")
    colCodes2 <- F2b(length(unique(edgeList2$diceSim)))
    edges_col2 <- sapply(edgeList2$diceSim, function(x) colCodes2[which(sort(unique(edgeList2$diceSim)) == x)])
    
    rm(colCodes2, F2b)
    
    # Let's create a network
    
    D3_network_MN <- networkD3::forceNetwork(Links = edgeList2, # data frame that contains info about edges
                                             Nodes = nodeList2, # data frame that contains info about nodes
                                             Source = "SourceID", # ID of source node 
                                             Target = "TargetID", # ID of target node
                                             Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                             NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                             Nodesize = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for a node size
                                             Group = "members",  # value from the node list (data frame) that contains value we want to use for node color
                                             height = 500, # Size of the plot (vertical)
                                             width = 800,  # Size of the plot (horizontal)
                                             fontSize = 18, # Font size
                                             linkDistance = networkD3::JS("function(d) { return 35*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                             linkWidth = networkD3::JS("function(d) { return d.value/2; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                             opacity = 0.85, # opacity
                                             zoom = TRUE, # ability to zoom when click on the node
                                             opacityNoHover = 0.1, # opacity of labels when static
                                             linkColour = edges_col2, # edge colors
                                             bounded=FALSE, #boundedin border
                                             arrows = TRUE) #directed
    # Plot network
    D3_network_MN 
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #download df
  output$downloadGraphml2 <- downloadHandler(
    filename = function() {
      paste("Retweet_Network-", Sys.Date(),".graphml", sep="")
    },
    content = function(file) {
      rtData <- data()
      sp = split(rtData, rtData$isRetweet)
      rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))
      el = as.data.frame(cbind(sender = tolower(rt$sender), receiver = tolower(rt$screenName)))
      el = dplyr::count(el, sender, receiver,sort = TRUE)
      
      
      ###########
      
      colnames(el) <- c("SourceName", "TargetName", "Weight")
      
      g2 <- igraph::graph.data.frame(el, directed = TRUE)
      V(g2)$label <- V(g2)$name
      
      igraph::write_graph(g2, file, format = "graphml")
    }
  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #download df
  output$downloadGraphml <- downloadHandler(
    filename = function() {
      paste("Mention_Network-", Sys.Date(),".graphml", sep="")
    },
    content = function(file) {
      mentionData <- data()
      sp2 = split(mentionData, mentionData$isRetweet)
      orig = sp2[['FALSE']]
      
      mentioned = 
        lapply(orig$text, function(tx) {
          matches = gregexpr('@[^([:blank:]|[:punct:])]+', tx)[[1]]
          sapply(seq_along(matches), function(i) 
            substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
        })
      
      mentionEL = 
        lapply(seq_along(orig$text), function(i) {
          if(mentioned[[i]] == '')  
            return(NULL)
          lapply(mentioned[[i]], function(m)
            c(sender = as.character(orig$screenName[i]), receiver = m)) %>%
            do.call(rbind, .) %>% as.data.frame()
        }) %>% 
        do.call(rbind, .) %>%
        dplyr::count(tolower(sender), tolower(receiver))
      ###########
      edgeList <- mentionEL
      colnames(edgeList) <- c("SourceName", "TargetName", "Weight")
      
      g <- igraph::graph.data.frame(edgeList, directed = TRUE)
      V(g)$label <- V(g)$name
      
      igraph::write_graph(g, file, format = "graphml")
    }
  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #------------------------------
  
  output$arabSentiment <- renderPlot({
    withProgress({
      #setProgress(message = paste("Collecting:", input$keyword," "))
      incProgress(amount = 0.1, message = "Keep Calm and Wait", detail = NULL,
                  session = getDefaultReactiveDomain())
      # Put in local time from library(lubridate)
      mydata <- data()
      mydata$created_at = with_tz(mydata$created_at, 'Asia/Muscat') # will reflect GMT+4 Oman time
      mydata$text <- as.factor(mydata$text)
      scores <- score.sentiment(mydata$text, pos.words, neg.words)
      #write.csv(scores, file='ArTrum.csv', row.names=TRUE) #save evaluation results into the file
      
      #total evaluation: positive / negative / neutral
      stat <- scores
      stat$created <- mydata$created_at
      stat$created <- as.Date(stat$created)
      stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
      by.tweet <- dplyr::group_by(stat, tweet, created)
      by.tweet <- dplyr::summarise(by.tweet, number=n())
      #write.csv(by.tweet, file= "senArTrump.csv", row.names=TRUE)
      
      #create chart
      g <- ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
        geom_point(aes(group=tweet, color=tweet), size=4) +
        theme(text = element_text(size=16), axis.text.x = element_text(angle=90, vjust=1)) +
        xlab("Twitting Date") + ylab("Number of Tweets") +
        ggtitle("Arabic Sentiment Analysis")
      
      g + theme_economist() + scale_colour_economist()
      
      
    })
    
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  output$threeEmo <- DT::renderDataTable({
    # Put in local time from library(lubridate)
    mydata <- data()
    
    
    mydata$text <- as.factor(mydata$text)
    scores <- score.sentiment(mydata$text, pos.words, neg.words)
    #write.csv(scores, file='ArTrum.csv', row.names=TRUE) #save evaluation results into the file
    
    #total evaluation: positive / negative / neutral
    stat <- scores
    stat$created <- mydata$created_at
    stat$created <- as.Date(stat$created)
    stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
    by.tweet <- dplyr::group_by(stat, tweet, created)
    by.tweet <- dplyr::summarise(by.tweet, number=n())
    #write.csv(by.tweet, file= "senArTrump.csv", row.names=TRUE)
    DT::datatable(by.tweet)
  })  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$allEmo <- DT::renderDataTable({
    # Put in local time from library(lubridate)
    mydata <- data()
    mydata$text <- as.factor(mydata$text)
    scores <- score.sentiment(mydata$text, pos.words, neg.words)
    stat <- scores
    stat$created <- mydata$created_at
    stat$created <- as.Date(stat$created)
    stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
    DT::datatable(stat)
  })  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sentPie <- renderPlotly({
    mydata <- data()
    mydata$text <- as.factor(mydata$text)
    scores <- score.sentiment(mydata$text, pos.words, neg.words)
    stat <- scores

    stat$created_at <- mydata$created_at
    stat$created_at <- as.Date(stat$created_at)
    stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
    by.tweet <- dplyr::group_by(stat, tweet, created_at)
    #write.csv(by.tweet, 'scores2.csv', row.names=TRUE) #save evaluation results
    
    by.tweet <- dplyr::summarise(by.tweet, number=n())
    #write.csv(by.tweet, file= "senArTrump.csv", row.names=TRUE)
    
    colors <- c('rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)')
    
    plotly::plot_ly(by.tweet, labels = ~tweet, values = ~number, type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    hoverinfo = 'text',
                    text = ~paste('Tweets =',number),
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 1)),
                    #The 'pull' attribute can also be used to create space between the sectors
                    showlegend = FALSE) %>%
      layout(title = paste("Sentiment Analysis of:", input$keyword," "),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  output$topNeg <- DT::renderDataTable({
    some_txt <- data()$text
    # remove retweet entities
    some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    # remove at people
    some_txt = gsub("@\\w+", "", some_txt)
    # remove punctuation
    some_txt = gsub("[[:punct:]]", "", some_txt)
    # remove numbers
    some_txt = gsub("[[:digit:]]", "", some_txt)
    # remove html links
    some_txt = gsub("http\\w+", "", some_txt)
    some_txt <- gsub('\\d+', "", some_txt)
    # remove unnecessary spaces
    some_txt = gsub("[ \t]{2,}", "", some_txt)
    some_txt = gsub("^\\s+|\\s+$", "", some_txt)
    # Lowercase all words for convenience
    some_txt <- tolower(some_txt)
    # Remove words with more than 3 numbers in them (they overwhelm the corpus, and are uninformative)
    #some_txt <- gsub("[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "", some_txt)
    # Remove all newline characters
    some_txt <- gsub("[\r\n]", "", some_txt)
    # Replace whitespace longer than 1 space with a single space
    some_txt <- gsub(" {2,}", " ", some_txt)
    word.list <- str_split(some_txt, '\\s+')
    words <- unlist(word.list)                
    posW <- words[words %in% neg.words]
    corpusfreq <- data.frame(table(posW))
    names(corpusfreq) <- c("Word", "Freq")
    corpusfreq$Word <- as.character(corpusfreq$Word)
    corpusfreq$Freq <- as.numeric(corpusfreq$Freq)
    corpusfreq <- corpusfreq[order(-corpusfreq$Freq), ]
    topNegW <- data.frame(NegWord=corpusfreq$Word, Frequncy=corpusfreq$Freq)
    DT::datatable(topNegW, style = "bootstrap")
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  output$topPos <- DT::renderDataTable({
    some_txt <- data()$text
    # remove retweet entities
    some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    # remove at people
    some_txt = gsub("@\\w+", "", some_txt)
    # remove punctuation
    some_txt = gsub("[[:punct:]]", "", some_txt)
    # remove numbers
    some_txt = gsub("[[:digit:]]", "", some_txt)
    # remove html links
    some_txt = gsub("http\\w+", "", some_txt)
    some_txt <- gsub('\\d+', "", some_txt)
    # remove unnecessary spaces
    some_txt = gsub("[ \t]{2,}", "", some_txt)
    some_txt = gsub("^\\s+|\\s+$", "", some_txt)
    # Lowercase all words for convenience
    some_txt <- tolower(some_txt)
    # Remove words with more than 3 numbers in them (they overwhelm the corpus, and are uninformative)
    #some_txt <- gsub("[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "", some_txt)
    # Remove all newline characters
    some_txt <- gsub("[\r\n]", "", some_txt)
    # Replace whitespace longer than 1 space with a single space
    some_txt <- gsub(" {2,}", " ", some_txt)
    word.list <- str_split(some_txt, '\\s+')
    words <- unlist(word.list)                
    posW <- words[words %in% pos.words]
    corpusfreq <- data.frame(table(posW))
    names(corpusfreq) <- c("Word", "Freq")
    corpusfreq$Word <- as.character(corpusfreq$Word)
    corpusfreq$Freq <- as.numeric(corpusfreq$Freq)
    corpusfreq <- corpusfreq[order(-corpusfreq$Freq), ]
    topPosW <- data.frame(PosWord=corpusfreq$Word, Frequncy=corpusfreq$Freq)
    DT::datatable(topPosW, style = "bootstrap")
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # output$cloud100 <- renderPlot({
  #   
  #   some_txt <- data()$text
  #   # remove retweet entities
  #   some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  #   # remove at people
  #   some_txt = gsub("@\\w+", "", some_txt)
  #   # remove punctuation
  #   some_txt = gsub("[[:punct:]]", "", some_txt)
  #   # remove numbers
  #   some_txt = gsub("[[:digit:]]", "", some_txt)
  #   # remove html links
  #   some_txt = gsub("http\\w+", "", some_txt)
  #   some_txt <- gsub('\\d+', "", some_txt)
  #   # remove unnecessary spaces
  #   some_txt = gsub("[ \t]{2,}", "", some_txt)
  #   some_txt = gsub("^\\s+|\\s+$", "", some_txt) #removing trailling whitespace
  #   # Lowercase all words for convenience
  #   some_txt <- tolower(some_txt)
  #   # Remove words with more than 3 numbers in them (they overwhelm the corpus, and are uninformative)
  #   #some_txt <- gsub("[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "", some_txt)
  #   # Remove all newline characters
  #   some_txt <- gsub("[\r\n]", "", some_txt)
  #   # Replace whitespace longer than 1 space with a single space
  #   some_txt <- gsub(" {2,}", " ", some_txt)
  #   
  #   
  #   # Regex pattern for removing stop words
  #   sw <- c(stopwords("english"),stop.wordsAr)
  #   stop_pattern <- paste0("\\b(", paste0(sw, collapse="|"), ")\\b")
  #   #tm_map(abs, removeWords, c(stopwords("english"),"my","custom","words")) 
  #   some_txt <- gsub(stop_pattern, "", some_txt)
  #   
  #   # Replace whitespace longer than 1 space with a single space
  #   some_txt <- gsub(" {2,}", " ", some_txt)
  #   some_txt = gsub("^\\s+|\\s+$", "", some_txt) #removing trailing whitespace
  #   some_txt = gsub("^\\s+", "", some_txt) #removing leading whitespace
  #   some_txt <- str_trim(some_txt)
  #   
  #   
  #   # Split on spaces and return list of character vectors
  #   some_txt <- str_split(some_txt, '\\s+')
  #   some_txt <- unlist(some_txt)
  #   some_txt <- str_trim(some_txt)
  #   
  #   docs = Corpus(VectorSource(some_txt))
  #   docs <- tm_map(docs, removePunctuation)  	
  #   docs <- tm_map(docs, removeNumbers)
  #   docs <- tm_map(docs, tolower) 
  #   docs <- tm_map(docs, stemDocument) 
  #   docs <- tm_map(docs, stripWhitespace)   
  #   docs <- tm_map(docs, PlainTextDocument) 
  #   
  #   dtm <- DocumentTermMatrix(docs) 
  #   tdm <- TermDocumentMatrix(docs)
  #   freq <- colSums(as.matrix(dtm))	
  #   dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
  #   #ord <- order(freq) 
  #   
  #   
  #   # plot words occurring at least 20 times
  #   #set.seed(142)   
  #   #wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) 
  #   
  #   #100 most frequently occurring words
  #   
  #   set.seed(142)   
  #   dark2 <- brewer.pal(6, "Dark2")   
  #   wordcloud(names(freq), freq,scale=c(6,.75), max.words=100, rot.per=0.3, random.color=TRUE,colors=dark2) 
  #   
  # })   
  # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  output$topHashtags <- renderPlot({
    hashTags <- data()$text  
    ## Barplot of top 10 hashtags
    hashTags <- hashTags[!is.na(hashTags)]
    hashTags <- strsplit(hashTags, " ") #split characters-tweets
    hashTags <-	unlist(hashTags)
    hashTags <-	hashTags[grep("^#", hashTags)] #get hashtgged words
    #hashTags <-	gsub("[^A-Za-z0-9]", "", hashTags) #remove words start with any char or number
    hashTags <-	as.data.frame(table(hashTags)) 
    colnames(hashTags) <- c("hashTags", "Freq")
    hashTags <-	subset(hashTags, hashTags!="") # remove blanks
    hashTags <- hashTags[sort.list(hashTags$Freq, decreasing=TRUE), ]
    hashTags <- hashTags[1:10,]
    #hashTags <- paste("#", hashTags$hashTags, sep="")
    
    g <- ggplot(hashTags) + geom_bar(aes(hashTags, Freq), fill = "skyblue", stat="identity") + 
      ylab("Frequency") + xlab("Top Hashtags") +
      coord_flip() + theme_bw() + ggtitle("Top 10 hashtags")
    g+theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold"))
  })   
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$topUsers <- renderPlot({
    users <- data()$screen_name
    ## Top Active Users whether they tweet, retweet or reply to others
    users <- users[!is.na(users)]
    users <- as.data.frame(table(users))
    colnames(users) <- c("user", "tweets")
    users <- users[order(users$tweets, decreasing=T), ]
    users <- users[1:10,]
    #users <- subset(users, user!="Omantel")
    
    g <- ggplot(users) + geom_bar(aes(user, tweets), fill = "orange", stat="identity") + 
      ylab("Frequency") + xlab("Top Users") +
      coord_flip() + theme_bw() + ggtitle("Top 10 users")
    g+theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold"))
    #p2 + theme_economist() + scale_colour_economist() 
  })   
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  ## Top Replied to Users
  output$topRepliedTo <- renderPlot({
    repliedTo <- data()$reply_to_screen_name
    repliedTo <- repliedTo[!is.na(repliedTo)]
    repliedTo <- as.data.frame(table(repliedTo))
    names(repliedTo) <- c("user", "replies")
    repliedTo <- repliedTo[order(repliedTo$replies, decreasing=T), ]
    repliedTo <- repliedTo[1:10,]
    #users <- subset(users, user!="Omantel")
    
    g <- ggplot(repliedTo) + geom_bar(aes(user, replies), fill = "seagreen3", stat="identity") + 
      ylab("Frequency") + xlab("Top Replied Users") +
      #theme(text = element_text(size=40))+
      coord_flip() + theme_bw() + ggtitle("Top 10 Replied to Users")
    g+theme(axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold"))
    #p3 + theme_solarized() +
    #scale_colour_solarized("blue")
  })   
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  ## Top retweeted tweet
  #  output$topRetweeted <- DT::renderDataTable({
  #    
  #    topRetweeted <- data.frame(tweet=data()$text,retweetCount=data()$retweetCount)
  #    topRetweeted <- topRetweeted[!is.na(topRetweeted)]
  #    topRetweeted <- unique(topRetweeted, by=tweet)#avoid duplicates of tweets using data.table
  #    topRetweeted <- data.table(topRetweeted)
  #    topRetweeted <- setorder(topRetweeted,-retweetCount)
  #    topRetweeted <- topRetweeted[1:10,]
  #    DT::datatable(topRetweeted, style = "bootstrap")
  #  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$topFavRetw <- DT::renderDataTable({
    topFavorited <- data.frame(tweet=data()$text,retweetCount=data()$retweet_count,favoriteCount=data()$favorite_count)
    #topFavorited <- topFavorited[!is.na(topFavorited)]
    topFavorited <- unique(topFavorited, by=tweet)#avoid duplicates of tweets using data.table
    #topFavorited <- data.frame(topFavorited)
    #topFavorited <- setorder(topFavorited,-favoriteCount)
    #topFavorited <- topFavorited[1:10,]
    DT::datatable(topFavorited, style = "bootstrap")
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
  
  ################################# 
  #download nodes attributes of retweet nework
  #################################
  output$nodesattributes1 <- downloadHandler(
    filename = function() {
      paste("Node_Attributes_MN_Net", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      mydata <- data()
      sp1 = split(mydata, mydata$is_retweet)
      rt = mutate(sp1[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))
      el = as.data.frame(cbind(sender = tolower(rt$sender), receiver = tolower(rt$screen_name)))
      el = dplyr::count(el, sender, receiver,sort = TRUE)
      edgeList1 <- el
      colnames(edgeList1) <- c("SourceName", "TargetName", "Weight")
      
      # Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
      gD1 <- igraph::simplify(igraph::graph.data.frame(edgeList1, directed=TRUE))
      
      # Create a node list object (actually a data frame object) that will contain information about nodes
      # because networkD3 library requires IDs to start at 0
      nodeList1 <- data.frame(ID = c(0:(igraph::vcount(gD1) - 1)),nName = igraph::V(gD1)$name)
      
      # Map node names from the edge list to node IDs
      getNodeID1 <- function(x){
        which(x == igraph::V(gD1)$name) - 1 # to ensure that IDs start at 0
      }
      
      # And add them to the edge list
      edgeList1 <- plyr::ddply(edgeList1, .variables = c("SourceName", "TargetName", "Weight"), 
                               function (x) data.frame(SourceID = getNodeID1(x$SourceName), 
                                                       TargetID = getNodeID1(x$TargetName)))
      
      
      
      # Calculate degree for all nodes
      nodeList1 <- cbind(nodeList1, nodeDegree=igraph::degree(gD1, v = igraph::V(gD1), mode = "all"))
      
      # Calculate betweenness for all nodes
      betAll1 <- igraph::betweenness(gD1, v = igraph::V(gD1), directed = FALSE) / (((igraph::vcount(gD1) - 1) * (igraph::vcount(gD1)-2)) / 2)
      betAll1.norm <- (betAll1 - min(betAll1))/(max(betAll1) - min(betAll1))
      #nodeList1 <- cbind(nodeList1, nodeBetweenness=100*betAll1.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
      
      # Calculate membership for all node
      memb1 <- igraph::membership(cluster_walktrap(gD1))
      
      #add betweeness and membership to the nodeList1
      nodeList1 <- cbind(nodeList1,members= as.numeric(memb1), nodeBetweenness=100*betAll1.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
      rm(betAll1, betAll1.norm, memb1)
      write.xlsx(nodeList1, file, sheetName="Sheet1",
                 col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
    }
  )
  #################################
  ################################# 
  
  
  ################################# 
  #download nodes attributes of mention nework
  #################################
  output$nodesattributes2 <- downloadHandler(
    filename = function() {
      paste("Node_Attributes_RT_Net", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      mentionData <- data()
      sp2 = split(mentionData, mentionData$is_retweet)
      orig = sp2[['FALSE']]
      
      mentioned = 
        lapply(orig$text, function(tx) {
          matches = gregexpr('@[^([:blank:]|[:punct:])]+', tx)[[1]]
          sapply(seq_along(matches), function(i) 
            substr(tx, matches[i] + 1, matches[i] + attr(matches, 'match.length')[i] - 1))
        })
      
      mentionEL = 
        lapply(seq_along(orig$text), function(i) {
          if(mentioned[[i]] == '')  
            return(NULL)
          lapply(mentioned[[i]], function(m)
            c(sender = as.character(orig$screenName[i]), receiver = m)) %>%
            do.call(rbind, .) %>% as.data.frame()
        }) %>% 
        do.call(rbind, .) %>%
        dplyr::count(tolower(sender), tolower(receiver))
      ###########
      #http://www.vesnam.com/Rblog/viznets6/
      edgeList2 <- mentionEL
      
      colnames(edgeList2) <- c("SourceName", "TargetName", "Weight")
      
      # Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
      gD2 <- igraph::simplify(igraph::graph.data.frame(edgeList2, directed=TRUE))
      
      # Create a node list object (actually a data frame object) that will contain information about nodes
      # because networkD3 library requires IDs to start at 0
      nodeList2 <- data.frame(ID = c(0:(igraph::vcount(gD2) - 1)),nName = igraph::V(gD2)$name)
      
      # Map node names from the edge list to node IDs
      getNodeID2 <- function(x){
        which(x == igraph::V(gD2)$name) - 1 # to ensure that IDs start at 0
      }
      
      # And add them to the edge list
      edgeList2 <- plyr::ddply(edgeList2, .variables = c("SourceName", "TargetName", "Weight"), 
                               function (x) data.frame(SourceID = getNodeID2(x$SourceName), 
                                                       TargetID = getNodeID2(x$TargetName)))
      
      
      
      # Calculate degree for all nodes
      nodeList2 <- cbind(nodeList2, nodeDegree=igraph::degree(gD2, v = igraph::V(gD2), mode = "all"))
      
      # Calculate betweenness for all nodes
      betAll2 <- igraph::betweenness(gD2, v = igraph::V(gD2), directed = FALSE) / (((igraph::vcount(gD2) - 1) * (igraph::vcount(gD2)-2)) / 2)
      betAll2.norm <- (betAll2 - min(betAll2))/(max(betAll2) - min(betAll2))
      #nodeList2 <- cbind(nodeList2, nodeBetweenness=100*betAll2.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
      
      # Calculate membership for all node
      memb2 <- igraph::membership(cluster_walktrap(gD2))
      
      #add betweeness and membership to the nodeList2
      nodeList2 <- cbind(nodeList2,members= as.numeric(memb2), nodeBetweenness=100*betAll2.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
      rm(betAll2, betAll2.norm, memb2)
      write.xlsx(nodeList2, file, sheetName="Sheet1",
                 col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
    }
  )
  #################################
  ################################# 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
} 