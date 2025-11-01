# Load necessary libraries
library(tidyverse)
library(quantmod)
library(rvest)
library(future)
library(furrr)

# --- 1. Get S&P 500 Company List ---
# URL of the Wikipedia page containing the S&P 500 list
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# Read the HTML content of the page
sp500_page <- read_html(url)

# Extract the table of constituent companies [12]
sp500_table <- sp500_page %>%
  html_node(xpath = '//*[@id="constituents"]') %>%
  html_table()

# --- 2. Prepare Ticker and Security Name Vectors ---
# Create a clean vector of ticker symbols.
# This replaces any '.' in symbols from Wikipedia with a '-' to match Yahoo Finance's format
# (e.g., "BRK.B" becomes "BRK-B").
tickers <- str_replace(sp500_table$Symbol, "\\.", "-")

# Create a clean vector of the company security names.
security_names <- sp500_table$Security

# --- 3. Define the Data Download Function ---
# This function now accepts both ticker and name, returning a tibble
# with both identifiers included as columns.
download_stock_data <- function(ticker, security_name) {
  tryCatch({
    # Download the data using the ticker symbol
    stock_xts <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
    
    # Convert the time-series object to a tibble
    stock_tibble <- as_tibble(stock_xts, rownames = "Date") %>%
      mutate(
        Date = as.Date(Date),          # Ensure 'Date' is a Date object
        Symbol = ticker,               # Add the ticker symbol as a column
        Security = security_name       # Add the security name as a column
      ) %>%
      # Reorder columns for better readability: identifiers first
      select(Symbol, Security, Date, everything())
      
    # A brief pause to avoid overwhelming the server
    Sys.sleep(0.5)
    
    return(stock_tibble)
    
  }, error = function(e) {
    # If a download fails, print an informative error and return NULL
    cat("Error downloading data for", ticker, "(", security_name, "):", e$message, "\n")
    return(NULL)
  })
}

# --- 4. Execute Download in Parallel ---
# Set up parallel processing to speed up the downloads significantly.
plan(multisession, workers = availableCores() - 1)

cat("Starting download for all S&P 500 stocks...\n")
# Use future_map2 to iterate over both 'tickers' and 'security_names' simultaneously.
stock_data_list <- future_map2(
  .x = tickers,
  .y = security_names,
  .f = download_stock_data,
  .progress = TRUE # Display a helpful progress bar
)

# --- 5. Clean Up and Save Data ---
# Assign the ticker symbols as the names of the list elements.
# This allows for easy access to a specific stock's data, e.g., stock_data_list[["MSFT"]]
names(stock_data_list) <- tickers

# Filter out any NULL elements from the list, which result from download errors.
stock_data_list <- stock_data_list[!sapply(stock_data_list, is.null)]

# Define the path for the output file.
# Ensure the target directory exists on your system.
output_file <- "~/Kaggle Projects/shiny-stock-monitor/app-files/data/stock_data_list.rds"

# Save the final, cleaned list to an RDS file for later use.
saveRDS(stock_data_list, file = output_file)

cat("Process complete. All data has been saved to:", output_file, "\n")
