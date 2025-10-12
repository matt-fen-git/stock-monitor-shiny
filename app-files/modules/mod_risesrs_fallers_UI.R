# UI Module
mod_risers_fallers_UI <- function(id) {
  ns <- NS(id)
  tagList(
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
        actionButton(ns("update"), "Update Table", class = "btn-primary")
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
        withSpinner(gt_output(ns("faller_table")))
      )
    )
  )
}

# Server Module
mod_risers_fallers_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shinyalert(
      title = "Dip Scanner Ready",
      text = "Click 'Update Table' to scan for buy opportunities based on ROC z-scores.",
      type = "info",
      closeOnClickOutside = TRUE
    )
    
    table_data <- eventReactive(input$update, {
      if (input$range == 9999) {
        faller_data <- all_dips_data %>% 
          filter(between(date, as.Date(input$dateRange[1]), as.Date(input$dateRange[2])))
      } else if (input$range == 7) {
        faller_data <- all_dips_data %>% 
          filter(date >= max(date) - 7 & date <= max(date))
      } else if (input$range == 31) {
        faller_data <- all_dips_data %>% 
          filter(date >= max(date) - 31 & date <= max(date))
      } else if (input$range == 90) {
        faller_data <- all_dips_data %>% 
          filter(date >= max(date) - 90 & date <= max(date))
      }
      
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
      
      faller_data %>% filter(roc_z <= input$z_threshold)
    })
    
    output$total_dips <- renderText({
      nrow(table_data())
    })
    
    output$strong_buys <- renderText({
      sum(table_data()$roc_z <= -2, na.rm = TRUE)
    })
    
    output$avg_dip <- renderText({
      round(mean(table_data()$roc_z, na.rm = TRUE), 2)
    })
    
    # Enhanced GT table with corrected sparkline
    # Enhanced GT table with WORKING sparkline (fixed list-column preservation)
    output$faller_table <- render_gt({
      faller_data <- table_data()
      
      if (nrow(faller_data) == 0) {
        return(gt(data.frame(Message = "No dips found matching criteria")) %>%
                 tab_style(style = cell_text(color = "red", weight = "bold"),
                           locations = cells_body()))
      }
      
      unique_stocks <- unique(faller_data$stock)
      
      # FIXED: Build sparkline data using tibble instead of data.frame to preserve list-column
      sparkline_list <- lapply(unique_stocks, function(stock_name) {
        stock_full <- stock_data_list[[stock_name]]
        
        if (is.null(stock_full)) {
          prices <- rep(NA_real_, 30)
        } else {
          stock_recent <- tail(stock_full, 30)
          prices <- as.numeric(Cl(stock_recent))
          
          if (length(prices) < 30) {
            prices <- c(rep(NA_real_, 30 - length(prices)), prices)
          }
        }
        
        list(stock = stock_name, price_trend = list(prices))  # Return as named list
      })
      
      # Convert to tibble to preserve list-column structure
      sparkline_data <- tibble(
        stock = sapply(sparkline_list, function(x) x$stock),
        price_trend = lapply(sparkline_list, function(x) x$price_trend[[1]])
      )
      
      # Join with faller data
      faller_with_sparklines <- left_join(faller_data, sparkline_data, by = "stock")
      
      # Verify list-column exists (debug - remove after testing)
      print(paste("Price trend class:", class(faller_with_sparklines$price_trend)))
      print(paste("First element:", length(faller_with_sparklines$price_trend[[1]])))
      
      # Build gt table
      faller_with_sparklines %>% select(-price_trend)%>%
        gt() %>%
        cols_label(
          date = "Dip Date",
          stock = "Ticker",
          buy_signal = "Signal",
          dip_severity = "Severity",
          roc_z = "Z-Score",
          close = "Close",
          adjusted = "Adj Close",
          price_change_pct = "% Change",
          volume = "Volume",
          vol_relative = "Vol Type",
          days_since = "Days Ago"#,
#          price_trend = "30-Day Trend"
        ) %>%
        # gt_plt_sparkline(
        #   column = price_trend,
        #   type = "default",
        #   fig_dim = c(15, 30),
        #   same_limit = FALSE,
        #   label = TRUE
        # ) %>%
        cols_hide(columns = c(outlier)) %>%
        fmt_date(date, date_style = "yMd") %>%
        fmt_number(roc_z, decimals = 2) %>%
        fmt_number(price_change_pct, decimals = 1, pattern = "{x}%") %>%
        fmt_currency(columns = c(open,high,low,close, adjusted), currency = "USD", decimals = 2) %>%
        fmt_number(volume, decimals = 0, scale_by = 1e-6, pattern = "{x}M") %>%
        cols_move_to_start(columns = c(date, stock, buy_signal, dip_severity, roc_z)) %>%
        data_color(
          columns = roc_z,
          method = "numeric",
          palette = c("#00FF00", "#FFFF00", "#FF0000"),
          domain = c(-4, 0),
          reverse = TRUE
        ) %>%
        data_color(
          columns = dip_severity,
          method = "factor",
          palette = c("Extreme" = "#8B0000", "Severe" = "#FF4500", 
                      "Strong" = "#FFA500", "Moderate" = "#FFD700")
        ) %>%
        tab_style(
          style = cell_fill(color = "#E8F5E9"),
          locations = cells_body(rows = roc_z <= -2.5)
        ) %>%
        tab_header(
          title = md("**S&P 500 Dip Opportunities**"),
          subtitle = md("*Ranked by recent dips with strongest buy signals*")
        ) %>%
        tab_options(
          table.font.size = px(12),
          heading.title.font.size = px(16),
          heading.subtitle.font.size = px(12)
        ) %>%
        opt_interactive(
          use_filters = TRUE,
          use_search = TRUE,
          use_sorting = TRUE,
          use_page_size_select = TRUE,
          page_size_default = 25
        ) %>%
        tab_footnote(
          footnote = "Z-Score < -2 indicates oversold condition (strong buy signal)",
          locations = cells_column_labels(columns = roc_z)
        )
    })
    
    
  })  # Close moduleServer
}  # Close mod_risers_fallers_Server
