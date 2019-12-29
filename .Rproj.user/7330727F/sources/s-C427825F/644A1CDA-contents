library(shiny)
library(tidyverse)
library(shinydashboard)
library(rvest)
#####################
####### F N S #######
#####################
get.data <- function(x){
    myurl <- read_html("https://coinmarketcap.com/gainers-losers/") # read our webpage as html
    myurl <- html_table(myurl)  # convert to an html table for ease of use
    
    
    to.parse <- myurl[[3]]  # pull the first item in the list
    to.parse$`% 24h` <- gsub("%","",to.parse$`% 24h`) # cleanup - remove non-characters
    to.parse$`% 24h`<- as.numeric(to.parse$`% 24h`) #cleanup - convert percentages column to numeric
    to.parse$Symbol <- as.factor(to.parse$Symbol) # cleanup - convert coin symbol to factor
    
    to.parse$Symbol <- factor(to.parse$Symbol,
                              levels = to.parse$Symbol[order(to.parse$'% 24h')])  # sort by gain value
    to.parse  # return the finished data.frame
}
get.infobox.val <- function(x){
    
    df1 <- get.data() # run the scraping function above and assign that data.frame to a variable
    df1 <- df1$`% 24h`[1]  # assign the first value of the % gain column to same variable
    df1   # return value
    
}
get.infobox.coin <- function(x){
    
    df <- get.data()  # run the scraping function above and assign that data.frame to a variable
    df <- df$Name[1]  # assign the first value of the name column to same variable
    df   # return value
    
}
get.infobox.val1 <- function(x){
    
    df1 <- get.data() # run the scraping function above and assign that data.frame to a variable
    df1 <- df1$`% 24h`[30]  # assign the first value of the % gain column to same variable
    df1   # return value
    
}
get.infobox.coin1 <- function(x){
    
    df <- get.data()  # run the scraping function above and assign that data.frame to a variable
    df <- df$Name[30]  # assign the first value of the name column to same variable
    df   # return value
    
}

#####################
####### U I #########
#####################
ui <- dashboardPage(skin = "purple",
    
    
    # H E A D E R
    
    dashboardHeader(title = "Top Crypto Peformers"),
    
    # S I D E B A R
    
    dashboardSidebar(
        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: italics;
        font-size: 30px;
      }
    '))),
        
        h4("This is an interactive dashboard that pulls the top performing crypto coins from the last 24 hours as reported by 
           coinmarketcap.com. It refreshes every 10 seconds.", ),
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        
        
        h6("Built by Abhishek Dangol"),
        h6("Using R Studio and Shiny"),
        br(),
        a("My github page", href="https://github.com/abhishek-dangol")
        
    ),
    
    # B O D Y
    dashboardBody(
        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: italics;
        font-size: 22px;
      }
    '))),
        
        fluidRow(
            
            # InfoBox
            infoBoxOutput("top.coin",
                          width = 3),
            
            # InfoBox
            infoBoxOutput("top.name",
                          width = 3),
            # InfoBox
            infoBoxOutput("bottom.coin",
                          width = 3),
            
            # InfoBox
            infoBoxOutput("bottom.name",
                          width = 3)
            
        ),
        
        fluidRow(
            column(
                # Datatable
                box(
                    status = "danger",
                    headerPanel("Information Table"),
                    solidHeader = T,
                    br(),
                    DT::dataTableOutput("table", height = "350px"),
                    width = 6,
                    height = "560px"
                ),
                
                # Chart
                box(
                    status = "primary",
                    headerPanel("Histogram"),
                    solidHeader = T,
                    br(),
                    plotOutput("plot", height = "400px"),
                    width = 6,
                    height = "560px"
                ),
                width = 12
            )
            
        )
    )
    
)
#####################
#### S E R V E R ####
#####################
server <- function(input, output) {
    # R E A C T I V E 
    liveish_data <- reactive({
        invalidateLater(10000)    # refresh the report every 10k milliseconds (10 seconds)
        get.data()                # call our function from above
    })
    
    
    live.infobox.val <- reactive({
        invalidateLater(10000)    # refresh the report every 10k milliseconds (10 seconds)
        get.infobox.val()         # call our function from above
    })
    
    live.infobox.val1 <- reactive({
        invalidateLater(10000)    # refresh the report every 10k milliseconds (10 seconds)
        get.infobox.val1()         # call our function from above
    })
    
    live.infobox.coin <- reactive({
        invalidateLater(10000)    # refresh the report every 10k milliseconds (10 seconds)
        get.infobox.coin()        # call our function from above
    })
    
    live.infobox.coin1 <- reactive({
        invalidateLater(10000)    # refresh the report every 10k milliseconds (10 seconds)
        get.infobox.coin1()        # call our function from above
    })
    
    # D A T A   T A B L E   O U T P U T
    output$table <- DT::renderDataTable(DT::datatable({
        data <- liveish_data()}))
    
    
    # P L O T   O U T P U T
    output$plot <- renderPlot({ (ggplot(data=liveish_data(), aes(x=Symbol, y=`% 24h`)) +
                                     geom_bar(stat="identity", fill = "black") +
                                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                     ggtitle("Top gaining coins in the last 24 hours"))
    })
    
    
    
   
    
    # I N F O B O X   O U T P U T - N A M E -- B E S T
    output$top.name <- renderInfoBox({
        infoBox(
            "Top Performing Coin ",
            live.infobox.coin(),
            icon = icon("coins"),
            color = "olive",
            fill = TRUE)
    })
    # I N F O B O X   O U T P U T - V A L  -- B E S T
    output$top.coin <- renderInfoBox({
        infoBox(
            
            "Most Gain in Last Hour",
            paste0(live.infobox.val(), "%"),
            live.infobox.coin(),
            icon = icon("chart-line"),
            color = "olive",
            fill = TRUE)
    })
    
    
    # I N F O B O X   O U T P U T - N A M E -- W O R S T
    output$bottom.name <- renderInfoBox({
        infoBox(
            "Worst Performing Coin ",
            live.infobox.coin1(),
            icon = icon("coins"),
            color = "red",
            fill = TRUE)
    })
    
    # I N F O B O X   O U T P U T - VAL -- W O R S T
    output$bottom.coin <- renderInfoBox({
        infoBox(
            
            "Least Gain in Last Hour",
            paste0(live.infobox.val1(), "%"),
            live.infobox.coin1(),
            icon = icon("chart-line"),
            color = "red",
            fill = TRUE)
    })
    
    
}
#####################
#### D E P L O Y ####
#####################
# Return a Shiny app objectshinyApp(ui = ui, server = server)
shinyApp(ui = ui, server = server)