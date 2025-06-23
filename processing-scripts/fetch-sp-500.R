# The easiest way to get dplyr is sto install the whole tidyverse:
library(tidyverse) # https://www.tidyverse.org/
library(dplyr) # or just dplyr
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(purrr)
library("IRdisplay")

#quantmod::getSymbols(Symbols = "SP500")


# get all S&P500 ----------------------------------------------------------

library(rvest)

# URL of the Wikipedia page containing the S&P 500 list
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"


# read contents of the page
sp500_page <- read_html(url)

#get the list of all S&P 500 & log them
sp500_table <- sp500_page %>% html_node(xpath = '//*[@id="constituents"]') %>% html_table()


#Prepare symbol ticker for looping
tickers <- sp500_table$Symbol


# This is where I'll store the data
stock_data_list <- list()

# Looping through & getting all the data.. 

# Function to download stock data
download_stock_data <- function(ticker) {
  # try catch wiwll log the error but keeps on running
  tryCatch({
    stock_data <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
    Sys.sleep(1)  # Add a delay to avoid hitting the server too frequently
    return(stock_data)
  }, error = function(e) {
    cat("Error downloading data for", ticker, "\n")
    # would be good to open a text file & log errors there
    return(NULL)
  })
}

# Use purrr::map to download data for each ticker symbol
stock_data_list <- map(tickers, download_stock_data,.progress = TRUE)

# Name the list elements with the ticker symbols
names(stock_data_list) <- sp500_table$Security

saveRDS(stock_data_list,file = "~/Kaggle Projects/shiny-stock-monitor/app-files/data/stock_data_list.rds")


