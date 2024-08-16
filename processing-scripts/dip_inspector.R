# Define the function to process each stock dataset
get_dips <- function(symbol_name) {
  require(dplyr)
  require(tidyverse)
  require(TTR)
  require(quantmod)
  tryCatch({
    
    data <- stock_data_list[[symbol_name]]
    
    # Calculate ROC with a period of 30 days
    roc_data <- ROC(Cl(data %>% 
                         subset(year(index(.))>=2024)), # I am looking at 2024 only!
                    n = 1) # <<<< CHANGE LAGGING VALUE HERE..
    
    # Create Z-score
    roc_z <- (roc_data - mean(roc_data, na.rm = TRUE)) / sd(roc_data, na.rm = TRUE)
    
    # Join Data together
    this_data <- merge(data, roc_z)
    
    # Rename the joined column
    colnames(this_data)[ncol(this_data)] <- "roc_z"
    
    # Flag Outlier
    this_data$outlier <- ifelse(this_data$roc_z <= -2, 1, 0) # This needs to be investigated better...
    
    # Convert to a data frame and filter for outliers
    this_data_df <- 
      this_data %>%
      as.data.frame() %>%
      rownames_to_column("date") %>%
      mutate(date = as.Date(date),
             Stock = symbol_name) %>%
      filter(outlier == 1) %>% #, date >= Sys.Date() - 60) %>%
      arrange(desc(date))
    
    names(this_data_df) <-  tolower(sub(".*\\.","",colnames(this_data_df)))
    
    # Add the symbol name column
    #this_data_df$symbol_name <- symbol_name
    gc()
    return(this_data_df)
  }, error = function(e) {
    # Print error message and return NULL
    NULL
    cat("Error processing", symbol_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Initialize a list
all_dips <- list()

# Get tick names
tick_names <- names(stock_data_list)


all_dips_data <- do.call(rbind,purrr::map(tick_names,get_dips,.progress = TRUE))
data.table::fwrite(all_dips_data,"data/all_dips_data.csv")
