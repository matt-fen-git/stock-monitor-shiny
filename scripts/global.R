# Load necessary libraries
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(quantmod)
library(markdown)
library(gt)
library(gtExtras)
library(tidyr)
library(dplyr)

# Read the content of the text file and convert it to Markdown
rsi_content <- suppressWarnings(readLines("~/Kaggle Projects/shiny-stock-monitor/Descriptions/rsi_info.txt"))
rsi_content <- paste(rsi_content, collapse = "\n")

# Read Data
stock_data_list <- readRDS("~/Kaggle Projects/shiny-stock-monitor/data/stock_data_list.rds")

all_dips_data <- data.table::fread("~/Kaggle Projects/shiny-stock-monitor/data/all_dips_data.csv") %>% as.data.frame()
all_dips_data$date <- as.Date(all_dips_data$date)

print("I ran this")