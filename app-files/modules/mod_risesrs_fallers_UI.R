# --- UI Module (Unchanged) ---
mod_risers_fallers_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # Add custom styling for value boxes to ensure text is visible on dark themes
    tags$head(
      tags$style(HTML("
        .bslib-value-box .value-box-title,
        .bslib-value-box .value-box-value {
          color: #ffffff !important;
        }
        .bslib-value-box .value-box-showcase {
          color: #ffffff !important;
        }
      "))
    ),
    useShinyalert(force = TRUE),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        dateRangeInput(ns("dateRange"), "Select Date Range:",
                       start = Sys.Date() - 180,
                       end = Sys.Date()
        ),
        radioButtons(ns("range"),
                     "Last Dips range:",
                     choices = c("Last Week" = 7, "Last Month" = 31, "Last Quarter" = 90, "All" = 9999),
                     selected = 31
        ),
        sliderInput(ns("z_threshold"), 
                    "Z-Score Threshold (Buy Signal):",
                    min = -4, max = 0, value = -2, step = 0.1),
        actionButton(ns("update"), "Update Table", class = "btn-primary", style = "width: 100%;")
      ),
      mainPanel(
        width = 10,
        layout_column_wrap(
          width = 1/3,
          value_box(
            title = "Total Dips Found",
            value = textOutput(ns("total_dips")),
            showcase = icon("chart-line"),
            theme = "primary"
          ),
          value_box(
            title = "Strong Buy Signals",
            value = textOutput(ns("strong_buys")),
            showcase = icon("bullseye"),
            theme = "success"
          ),
          value_box(
            title = "Average Z-Score",
            value = textOutput(ns("avg_dip")),
            showcase = icon("thermometer-half"),
            theme = "info"
          )
        ),
        br(),
        # Use a spinner for better user experience during calculation
        shinycssloaders::withSpinner(gt_output(ns("faller_table")))
      )
    )
  )
}


# --- Server Module (Corrected) ---
mod_risers_fallers_Server <- function(id, all_dips_data, stock_data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initial popup to guide the user
    shinyalert(
      title = "Dip Scanner Ready",
      text = "Click 'Update Table' to scan for buy opportunities based on ROC z-scores.",
      type = "info",
      closeOnClickOutside = TRUE
    )
    
    # Reactive expression to filter the main data based on UI inputs
    table_data <- eventReactive(input$update, {
      req(all_dips_data) # Ensure the base data is loaded
      
      # Date filtering logic
      if (input$range == 9999) {
        faller_data <- all_dips_data %>% 
          filter(between(date, as.Date(input$dateRange[1]), as.Date(input$dateRange[2])))
      } else {
        # Handles 7, 31, 90 day ranges relative to the latest date in the dataset
        latest_date <- max(all_dips_data$date)
        faller_data <- all_dips_data %>% 
          filter(date >= (latest_date - as.numeric(input$range)))
      }
      
      # Mutate data to add calculated columns for the table
      faller_data <- faller_data %>%
        mutate(
          price_change_pct = ((adjusted - open) / open) * 100,
          dip_severity = case_when(
            roc_z <= -3 ~ "Extreme",
            roc_z <= -2.5 ~ "Severe",
            roc_z <= -2 ~ "Strong",
            TRUE ~ "Moderate"
          ),
          buy_signal = case_when(
            roc_z <= -3 ~ "⭐⭐⭐⭐⭐",
            roc_z <= -2.5 ~ "⭐⭐⭐⭐",
            roc_z <= -2 ~ "⭐⭐⭐",
            roc_z <= -1.5 ~ "⭐⭐",
            TRUE ~ "⭐"
          ),
          days_since = as.numeric(Sys.Date() - date),
          vol_relative = ifelse(volume > median(volume, na.rm = TRUE), "High", "Normal")
        ) %>%
        arrange(desc(date), roc_z)
      
      # Final filtering based on the z-score threshold slider
      faller_data %>% filter(roc_z <= input$z_threshold)
    })
    
    # Render value box outputs
    output$total_dips <- renderText({ nrow(table_data()) })
    output$strong_buys <- renderText({ sum(table_data()$roc_z <= -2.5, na.rm = TRUE) })
    output$avg_dip <- renderText({ round(mean(table_data()$roc_z, na.rm = TRUE), 2) })
    
    # --- CORRECTED GT Table Rendering ---
    output$faller_table <- render_gt({
      faller_data <- table_data()
      
      if (is.null(faller_data) || nrow(faller_data) == 0) {
        return(gt(data.frame(Message = "No dips found matching your criteria.")) %>%
                 tab_header(title = "No Data Available"))
      }
      
      # --- FIX STARTS HERE ---
      
      # 1. Get the unique stock names (Security names) from the filtered data
      unique_stocks <- unique(faller_data$stock)
      
      # 2. Prepare the sparkline data correctly
      sparkline_data <- purrr::map_df(unique_stocks, function(stock_name) {
        stock_full_data <- stock_data_list[[stock_name]]
        
        prices <- if (!is.null(stock_full_data)) {
          # Dynamically find the close column (e.g., 'MMM.Close') and get the last 30 values
          close_col_name <- names(stock_full_data)[grepl("\\.Close$", names(stock_full_data))]
          tail(stock_full_data[[close_col_name]], 30)
        } else {
          # If data is missing for some reason, create a vector of NAs
          NA_real_
        }
        
        # Ensure the vector has exactly 30 values, padding with NAs if necessary
        if (length(prices) < 30) {
          prices <- c(rep(NA_real_, 30 - length(prices)), prices)
        }
        
        # Return a tibble with the stock name and a list-column containing the prices vector
        tibble(stock = stock_name, price_trend = list(prices))
      })
      
      # 3. Join the sparkline data back to the main data
      faller_with_sparklines <- left_join(faller_data, sparkline_data, by = "stock")
      
      # --- FIX ENDS HERE ---
      
      # Build the final gt table
      faller_with_sparklines %>%
        gt() %>%
        # The sparkline function will now work correctly
        gt_plt_sparkline(
          column = price_trend,
          type = "default",
          fig_dim = c(15, 45),
          palette = c("#00d4ff", "#00d4ff", "#ff4757", "#00ff88", "#b0b3b8")
        ) %>%
        cols_label(
          date = "Dip Date",
          stock = "Company",
          buy_signal = "Signal",
          dip_severity = "Severity",
          roc_z = "Z-Score",
          adjusted = "Adj Close",
          price_change_pct = "% Change",
          volume = "Volume",
          days_since = "Days Ago",
          price_trend = "30-Day Trend"
        ) %>%
        cols_hide(columns = c(open, high, low, close, outlier, vol_relative)) %>%
        fmt_date(date, date_style = "yMd") %>%
        fmt_number(roc_z, decimals = 2) %>%
        fmt_currency(columns = c(adjusted), currency = "USD") %>%
        fmt_number(volume, decimals = 0, scale_by = 1e-6, pattern = "{x}M") %>%
        cols_move_to_start(columns = c(date, stock, price_trend, buy_signal, dip_severity, roc_z)) %>%
        data_color(
          columns = roc_z,
          method = "numeric",
          palette = c("#00FF00", "#FFFF00", "#FF0000"),
          domain = c(-4, 0),
          reverse = TRUE
        ) %>%
        tab_style(
          style = cell_fill(color = "#2a3150"),
          locations = cells_body(rows = roc_z <= -2.5)
        ) %>%
        tab_header(
          title = md("**Stock Dip Opportunities**"),
          subtitle = md("*Scanning for z-score based mean-reversion signals.*")
        ) %>%
        opt_interactive(use_filters = TRUE, use_search = TRUE) %>%
        tab_options(
          table.background.color = "#151b3d",
          table.font.color = "#e8eaed",
          column_labels.background.color = "#1a2142",
          heading.background.color = "#1a2142"
        ) %>%
        tab_footnote(
          footnote = "Z-Score < -2 indicates a statistically significant dip.",
          locations = cells_column_labels(columns = roc_z)
        )
    })
  })
}
