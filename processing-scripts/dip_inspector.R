# Load all necessary libraries at the start
library(tidyverse)
library(TTR)
library(quantmod)
library(future)
library(furrr)
library(data.table)
library(lubridate)

# Define the function to process each stock dataset
get_dips <- function(symbol_name, stock_data_list) {
  
  tryCatch({
    
    # --- Data Preparation ---
    data <- stock_data_list[[symbol_name]]
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # --- Tidyverse Filtering ---
    data_2024 <- data %>%
      filter(year(Date) >= 2024)
    
    if(nrow(data_2024) <= 1) return(NULL)
    
    # --- Convert to XTS for Calculation ---
    # Robustly find the Close column name
    close_col_name <- names(data_2024)[grepl("\\.Close$", names(data_2024))]
    if (length(close_col_name) == 0) close_col_name <- "Close"
    
    data_xts_2024 <- xts(
      x = data_2024[[close_col_name]], 
      order.by = data_2024$Date
    )
    
    # --- Financial Calculation ---
    roc_data <- ROC(data_xts_2024, n = 1)
    roc_z <- (roc_data - mean(roc_data, na.rm = TRUE)) / sd(roc_data, na.rm = TRUE)
    names(roc_z) <- "roc_z" # Name the column for merging
    
    # --- Combine and Process Results ---
    
    # Convert z-scores to a tibble for joining
    roc_z_tibble <- as_tibble(roc_z, rownames = "date") %>%
      mutate(date = as.Date(date))
    
    # Join the z-scores back to the full daily data for 2024
    # This ensures we retain all OHLCV columns for the dip days
    all_data_with_z <- left_join(data_2024, roc_z_tibble, by = c("Date" = "date"))
    
    # Define outlier threshold (5th percentile of this stock's z-scores for the year)
    z_threshold <- quantile(all_data_with_z$roc_z, 0.05, na.rm = TRUE)
    
    # Filter for the days that are significant dips
    dips_df <- all_data_with_z %>%
      filter(roc_z <= z_threshold) %>%
      mutate(
        outlier = 1,
        stock = symbol_name
      ) %>%
      # Rename all columns to a consistent, lowercase format
      select(
        date = Date,
        stock,
        Symbol,
        open = ends_with(".Open"),
        high = ends_with(".High"),
        low = ends_with(".Low"),
        close = ends_with(".Close"),
        volume = ends_with(".Volume"),
        adjusted = ends_with(".Adjusted"),
        roc_z,
        outlier
      ) %>%
      arrange(desc(date))
    
    return(dips_df)
    
  }, error = function(e) {
    cat("Error processing", symbol_name, ":", e$message, "\n")
    return(NULL)
  })
}

# --- Main Execution ---

# Load your main data list
stock_data_list <- readRDS("~/Kaggle Projects/shiny-stock-monitor/app-files/data/stock_data_list.rds")

# Get the names of all stocks
tick_names <- names(stock_data_list)

# Set up parallel processing
plan(multisession, workers = availableCores() - 1)

# Run the 'get_dips' function for every stock in parallel
all_dips_list <- future_map(
  tick_names, 
  ~ get_dips(symbol_name = .x, stock_data_list = stock_data_list), 
  .progress = TRUE,
  .options = furrr_options(seed = TRUE)
)

# Combine the list of data frames into a single data table
all_dips_data <- rbindlist(compact(all_dips_list), fill = TRUE)

# Save the final compiled data to a CSV file for your Shiny app
fwrite(all_dips_data, "~/Kaggle Projects/shiny-stock-monitor/app-files/data/all_dips_data.csv")

cat("Process complete. all_dips_data.csv has been saved with all required columns.\n")
