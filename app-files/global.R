# Load necessary libraries
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)
library(quantmod)
library(markdown)
library(gt)
library(gtExtras)
library(tidyr)
library(dplyr)

# Read the content of the text file and convert it to Markdown
# rsi_content <- suppressWarnings(readLines("~/Kaggle Projects/shiny-stock-monitor/app-files/Descriptions/rsi_info.txt"))
# rsi_content <- paste(rsi_content, collapse = "\n")

# Read Data
#stock_data_list <- readRDS("~/Kaggle Projects/shiny-stock-monitor/app-files/data/stock_data_list.rds")
stock_data_list <- readRDS("data/stock_data_list.rds")



#all_dips_data <- data.table::fread("~/Kaggle Projects/shiny-stock-monitor/app-files/data/all_dips_data.csv") %>% as.data.frame()
all_dips_data <- data.table::fread("data/all_dips_data.csv") %>% as.data.frame()

all_dips_data$date <- as.Date(all_dips_data$date)

print("I ran this")
