library(shiny)
library(tidyverse)
library(shinydashboard)
library(rvest)

get.data <- function(x){
    myurl <- read_html("https://coinmarketcap.com/gainers-losers") ##read the webpage as html
    myurl <- html_table(myurl) ##convert to html table for ease of use
    to.parse <- myurl[[1]]  ##pull the first item in the list
    to.parse$`% 1h` <- gsub("%", "", to.parse$`% 1h`) ##cleanup - remove non-characters
    to.parse$`% 1h` <- as.numeric(to.parse$`% 1h`) ##cleanup - convert percentages column to numeric so that we can sort
    to.parse$Symbol <- as.factor(to.parse$Symbol) ##cleanup - coin symbol to factor
    to.parse$Symbol <- factor(to.parse$Symbol, levels = to.parse$Symbol[order(to.parse$'% 1h')]) ##sort bu gain value
    to.parse  ##return the finished data frame
}

