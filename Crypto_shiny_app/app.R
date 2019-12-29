library(shiny)
library(tidyverse)
library(shinydashboard)
library(rvest)



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


get.infobox.val <- function(x){ ##pulls the highest value from the above data frame
    df1 <- get.data()  ##run the scraping function above and assign that data.frame to variable
    df1 <- df1$`% 24h`[1] ##assign the first value of the %gain column to the same variable
    df1  ##return value
}

get.infobox.coin <- function(x){ ##returns the name of the top coin
    df2 <- get.data()  ##run the scraping function above and assign that data.frame to a variable
    df2 <- df$Name[1] ##assign the first value of the name column to the same variable
    df2  ## return value
    
}

################
######U I#######
################

ui <- dashboardPage(
    
    ##H E A D E R
    
    dashboardHeader(title = "Crypto Currency Gainers"),
    
    ##S I D E B A R
    dashboardSidebar(
    h5("An interactive dashboard that presents the top crypto gainers from the lsat 24 hours according to coinmarketcap.com. It refreshes every 60 seconds"),
    
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
    br(),
    a("Please check out my github page", href="github.com/abhishek-dangol")
),
    

    # B O D Y 
    dashboardBody(
        fluidRow(
            #InfoBox
            infoBoxOutput("top.coin", width = 3),
            
            #InfoBox
            infoBoxOutput("top.name", width = 3)
        ),
        
        fluidRow(
            column(
                # Crypto table
                box(
                    status = "primary",
                    headerPanel("Crypto Table"),
                    solidHeader = T,
                    br(),
                    DT::dataTableOutput("table", height = "350px"),
                    width = 6,
                    height = "560px"
                ),
                
                #Histogram
                box(
                    status = "primary",
                    headerPanel("Histogram"),
                    solidHeader = T,
                    br(),
                    plotOutput("plot", height = "400px"),
                    width = 6,
                    height = "500px"
                ),
                width = 12
            )
        )
    )

)


################
##S E R V E R ##
################

server <- function(input, output){
    # R E A C T I V E
    liveish_data <- reactive({
        invalidateLater(60000) ##refersh the data every 60 seconds or 60,000 milliseconds
        get.data()
    })
    
    live.infobox.val <- reactive({
        invalidateLater(60000) ##refresh the report every 60 seconds or 60,000 milliseconds
        get.infobox.val()
    })
    
    live.infobox.coin <- reactive({
        invalidateLater(60000)  ##refresh the report every 60 seconds or 60,000 milliseconds
        get.infobox.coin()
    })
    
    ##C R Y P T O  T A B L E  O U T P U T
    output$table <- DT::renderDataTable(DT::datatable({
        data <- liveish_data()
    }))
    
    ##H I S T O G R A M  O U T P U T
    output$plot <- renderPlot({
        (ggplot(data=liveish_data(), aes(x=Symbol, y='% 24h')) + geom_bar(stat="identity", fill = "springgreen3") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Gainers from the last 24 hours"))
    })
    
    ##I N F O B O X  O U T P U T - V A L
    output$top.coin <- renderInfoBox({
        infoBox(
            "Gain in the last 24 hours",
            paste0(live.infobox.val(), "%"),
            icon = icon("signal"),
            color = "purple",
            fill = TRUE
        )
    })
    
    ##I N F O B O X  O U T P U T - N A M E
    output$top.name <- renderInfoBox({
        infoBox(
            "Coin Name",
            live.infobox.coin(),
            icon = icon("bitcoin"),
            color = "purple",
            fill = TRUE
        )
    })
}
