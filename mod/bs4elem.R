library(bs4Dash)

side<-bs4DashSidebar(skin = 'light',status = 'primary',collapsed = T,
                     bs4SidebarMenu(id = ns('main'),
                                    bs4SidebarMenuItem(text = "Step1:Data-Collection", tabName = "mining",icon = icon("search", lib="glyphicon")),
                                    bs4SidebarMenuItem("Step2:Statistics", tabName = "arab",icon = icon("stats", lib="glyphicon")),
                                    bs4SidebarMenuItem("Step3:Tables", tabName = "tables",icon = icon("equalizer", lib="glyphicon")), 
                                    bs4SidebarMenuItem("Step4:Social-Network", tabName = "sna",icon = icon("connectdevelop", lib="font-awesome")),
                                    bs4SidebarMenuItem("About Me", tabName = "about", icon = icon("user-edit", lib="font-awesome"))
                                    )
                     )

head<-bs4DashNavbar(title = bs4DashBrand(title = "TwitterLycs",color = 'primary',image = 'logo.png'),leftUi = uiOutput(ns('leftnav')),
                    rightUi = uiOutput(ns('rightnav')),skin = 'light',status = 'primary'
                    )

cont<-bs4DashControlbar(disable = T)

body<-bs4DashBody(
  bs4TabItems(
    bs4TabItem(tabName = "mining",
               h4("You Are in Step1: Data-Collection"),
               
               fluidRow(
                 box(title = "Input", status = "primary", solidHeader = TRUE, width=4,
                     textInput(inputId = ns("keyword"), 
                               label = "Search for:",
                               value = "#Oman"),
                     
                     #textInput(inputId = "lang", 
                     #         label = "Language (en,ar,fr,es,ru,de,zh..etc)",
                     #          value = "en"),
                     
                     radioButtons(ns("lang"), inline=TRUE,selected = character(0),label="Choose Language",
                                  c("English"="en","Arabic"="ar")),
                     
                     radioButtons(ns("num"), "Number of Tweets",c(100,500,1000,5000),selected= 100, inline=TRUE),
                     
                     # numericInput(inputId = "num",
                     #              label = "Number of Tweets",
                     #              value = 100),
                     
                     #dateInput(inputId = "dateStart",value= Sys.Date(), format = "yyyy-mm-dd", 
                     #            label = "Since"),
                     
                     # dateInput(inputId = "dateEnd",value= Sys.Date(), format = "yyyy-mm-dd", 
                     #             label = "Until"),
                     
                     actionButton(inputId = ns("update"), 
                                  label = "Search")
                 )),
               fluidRow(
                 box(title = "Download Full Data", status = "primary", solidHeader = TRUE, width = 4,
                     # tags$h4("Click the Below Button to download Data"),
                     downloadButton(ns('downloadData'), 'Download'))),
               
               fluidRow( 
                 box(title = "Tabulated Results", status = "info", solidHeader = TRUE, width = 12,
                     DT::dataTableOutput(ns("dat")))                       
               )
               ),
    bs4TabItem(tabName = "arab",
               h4("You Are in Step2:Statistics"),
               
               box(width = 12, height = NULL,
                   fluidRow(
                     infoBoxOutput(ns("allExtracted"),width = 3),
                     infoBoxOutput(ns("original"),width = 3),
                     infoBoxOutput(ns("retweets"),width = 3), 
                     infoBoxOutput(ns("users"),width = 3) 
                   )),
               fluidRow(
                 
                 
                 box(title = "Top Hashtags", status = "primary", solidHeader = TRUE, width=6,
                     plotOutput(ns("topHashtags"))),
                 box(title = "Top Active Users", status = "warning", solidHeader = TRUE, width=6,
                     plotOutput(ns("topUsers"))),
                 box(title = "Top Replied To", status = "success", solidHeader = TRUE, width=6,
                     plotOutput(ns("topRepliedTo"))),
                 box(title = "Twitting Time", status = "danger", solidHeader = TRUE, width=6,
                     plotOutput(ns("plotTime"))),
                 box(title = "Used Platform", status = "info", solidHeader = TRUE, width=6,
                     plotOutput(ns("device"))),
                 box(title = "Sentiment Analysis", status = "primary", solidHeader = TRUE, width=6,
                     plotlyOutput(ns("sentPie"))),
                 box(title = "Sentiment by Time", status = "warning", solidHeader = TRUE, width=6,
                     plotOutput(ns("arabSentiment")))
                 # box(title = "Cloud of Top 100 Words", status = "success", solidHeader = TRUE, width=6,
                 #     plotOutput("cloud100"))
                 
               )
    ),
    bs4TabItem(tabName = "tables",
               h4("You Are in Step3:Tables"),
               
               fluidRow(
                 
                 
                 box(title = "Top Negative Words", status = "info", solidHeader = TRUE, width=6,
                     DT::dataTableOutput(ns("topNeg"))),
                 box(title = "Top Positive Words", status = "primary", solidHeader = TRUE, width=6,
                     DT::dataTableOutput(ns("topPos"))),
                 box(title = "Top Favorites & Retweets", status = "danger", solidHeader = TRUE, width=6,
                     DT::dataTableOutput(ns("topFavRetw"))),
                 box(title = "Most Frequent Words", status = "warning", solidHeader = TRUE, width=6,
                     DT::dataTableOutput(ns("topWords"))),
                 box(title = "Levels of Sentiment", status = "success", solidHeader = TRUE, width=6,
                     DT::dataTableOutput(ns("threeEmo"))),
                 box(title = "Full Data", status = "info", solidHeader = TRUE, width=6,
                     DT::dataTableOutput(ns("allEmo")))    
               )
    ),
    bs4TabItem(tabName = "sna", 
               h4("You Are in Step4:Social-Network"),
               fluidRow(
                 tabBox(    
                   # The id lets us use input$resuls on the server to find the current tab
                   id = "summaries", height = "1000px", width = 12,
                   tabPanel("Retweet Network",
                            fluidRow(
                              box(title = "Retweet Network", status = "warning", solidHeader = TRUE, width=10, height = 600,
                                  
                                  forceNetworkOutput(ns("sna")))),
                            fluidRow(
                              box(title = "Download Retweet Network Data", status = "info", solidHeader = TRUE, width=10,
                                  tags$h5("Click the Below Button to download .graphml"),
                                  downloadButton(ns('downloadGraphml2'), 'Download'))),
                            fluidRow(
                              box(title = "Download Node Attributes", status = "info", solidHeader = TRUE, width=10,
                                  tags$h5("Download Nodes List of Retweet Network"),
                                  
                                  
                                  downloadButton(ns('nodesattributes1'), 'Download')))
                   ),
                   
                   
                   tabPanel("Mention Network", 
                            fluidRow(
                              box(title = "Mention Network", status = "warning", solidHeader = TRUE, width=10, height = 600,
                                  forceNetworkOutput(ns("sna2")))),
                            fluidRow(
                              box(title = "Download Mention Network Data", status = "info", solidHeader = TRUE, width=10,
                                  tags$h5("Click the Below Button to download .graphml"),
                                  
                                  
                                  downloadButton(ns('downloadGraphml'), 'Download'))),
                            fluidRow(
                              box(title = "Download Node Attributes", status = "info", solidHeader = TRUE, width=10,
                                  tags$h5("Download Nodes List of Mention Network"),
                                  
                                  
                                  downloadButton(ns('nodesattributes2'), 'Download')))
                            
                            
                   )
                   
                   
                   
                 )
               )
               ),
    bs4TabItem(tabName = "about",
               div(style="padding:30px 60px; box-sizing:border-box;border-radius:10px; background-color:#007bff;",
                   div(style="display:flex;",
                       img(src="user.png", width='300px',style="border-radius:10px; margin-right:30px;"),
                       div(style="color:#fff",
                         tags$h2("Dr. Adil Al-Busaidi"),
                         tags$h3("Director of Innovation and Technology Transfer Center - Sultan Qaboos University"),
                         hr(),
                         p(style="color:#fff;",
                           icon("linkedin"),":",tags$a(style="color:#fff;",
                             "https://www.linkedin.com/in/adil-al-busaidi-56739218/",href="https://www.linkedin.com/in/adil-al-busaidi-56739218/")
                         ),
                         p(style="color:#fff;",
                           icon("orcid"),
                           ":",
                           tags$a(style="color:#fff;",
                             "https://orcid.org/0000-0002-0959-7419",href="https://orcid.org/0000-0002-0959-7419")
                         ),
                         hr(),
                         p(style="color:#fff;",
                           icon("envelope"),
                           ":",
                           tags$a(href="mailto:aalbusaidi@gmail.com",style="color:#fff;",
                           "aalbusaidi@gmail.com")
                         )
                       )
                       )
                   ),
               div(style="padding:30px 60px; box-sizing:border-box; background-color:#007bff; color:#fff; font-size:1.2em; text-align:center; margin:10px 0;border-radius:10px 10px 0 0;",
                tags$p(
                  "Dr. Adil Al-Busaidi is the Director of Innovation and Technology Transfer Center at Sultan Qaboos University (SQU). He is a dedicated educator with 17+ years of extensive international experience in teaching & research. Dr. Al-Busaidi worked as a network engineer, Director of Research & Innovation at Oman Smart City Platform, Head of business communication, and currently as the Director of Innovation & Technology Transfer Center at SQU. He helped develop several national strategies, priorities, and strategic directions. He helped organize and execute national Open Innovation Initiatives in mentorship & assessment of national and regional hackathons, datathons, and boot camps. Dr. Adil managed several cross-functional teams and experienced crisis management leadership. He was involved in several think-tank teams, national committees, national transformation initiatives, and an invited speaker. "
                ),
                tags$p(
                  "His interests revolve around Computer-Mediated-Communication and its role in innovating and elevating Education 4.0. He developed R programming-based applications for social media opinion mining, sentiment analysis, social network analysis, and sample size calculation. Dr. Adil received his Ph.D. from Ohio University in 2014; two Masters's degrees from Ohio University in 2012 and New Mexico State University in 2007; and a BSc from Sultan Qaboos University in 2004. Dr. Adil is a recipient of several awards such as Sharjah Award for best doctoral thesis in Administrative Science in the Arab World, The Arab Administrative Development Organization (ARADO) in 2016, and several best teacher awards at the college and the university level."
                )
               )
    )
  )
)