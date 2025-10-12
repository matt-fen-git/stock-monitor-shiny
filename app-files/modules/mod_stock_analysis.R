# UI Module (unchanged)
mod_stock_analysis_UI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput(ns("stock"), "Select Stock:", choices = names(stock_data_list)),
      dateRangeInput(ns("dateRange"), "Select Date Range:",
                     start = Sys.Date() - 365,
                     end = Sys.Date()
      ),
      actionButton(ns("update"), "Update Chart")
    ),
    mainPanel(
      width = 10,
      highchartOutput(ns("stockChart"), height = "800px")
    )
  )
}

# Server Module with FIXED candlestick colors
mod_stock_analysis_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    filtered_data <- eventReactive(input$update, {
      this_stock <- stock_data_list[[input$stock]]
      
      # Keep original column names for proper xts recognition
      # Don't rename columns - highcharter needs the stock symbol prefix
      
      # Subset data based on the selected date range
      this_stock <- this_stock[paste(input$dateRange[1], input$dateRange[2], sep = "/")]
      
      if (nrow(this_stock) < 30) {
        showNotification("Insufficient data for indicators. Try a wider date range.", type = "warning")
        return(NULL)
      }
      
      this_stock
    })
    
    output$stockChart <- renderHighchart({
      this_stock <- filtered_data()
      
      if (is.null(this_stock)) {
        return(highchart() %>% hc_title(text = "No data available - select wider date range"))
      }
      
      # Remove rows with NA close prices
      valid_close <- !is.na(Cl(this_stock))
      this_stock_clean <- this_stock[valid_close, ]
      
      if (nrow(this_stock_clean) < 30) {
        return(highchart() %>% 
                 hc_title(text = paste("Insufficient valid data:", input$stock)) %>%
                 hc_subtitle(text = "Stock has data gaps - try different date range"))
      }
      
      # Calculate indicators
      bollinger_bands <- TTR::BBands(Cl(this_stock_clean), n = 20, sd = 2)
      rsi.14 <- RSI(Cl(this_stock_clean), n = 14)
      RSI.SellLevel <- xts(rep(70, NROW(this_stock_clean)), index(this_stock_clean))
      RSI.BuyLevel <- xts(rep(30, NROW(this_stock_clean)), index(this_stock_clean))
      macd <- MACD(Cl(this_stock_clean), maType = "SMA")
      
      # Build chart with proper stock type and explicit candlestick colors
      highchart(type = "stock") %>%
        hc_yAxis_multiples(create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)) %>%
        
        # Add candlestick with EXPLICIT color control
        hc_add_series(
          this_stock_clean,
          type = "candlestick",
          yAxis = 0,
          name = input$stock,
          color = "#ef5350",      # Red for bearish (close < open)
          upColor = "#26a69a",    # Green for bullish (close > open)
          lineColor = "#ef5350",  # Border color for bearish
          upLineColor = "#26a69a" # Border color for bullish
        ) %>%
        
        # Add Bollinger Bands
        hc_add_series(
          bollinger_bands[, "up"],
          yAxis = 0,
          name = "BB Upper",
          color = "#f45b5b",
          type = "line",
          lineWidth = 1,
          dashStyle = "ShortDash"
        ) %>%
        hc_add_series(
          bollinger_bands[, "dn"],
          yAxis = 0,
          name = "BB Lower",
          color = "#f45b5b",
          type = "line",
          lineWidth = 1,
          dashStyle = "ShortDash"
        ) %>%
        hc_add_series(
          bollinger_bands[, "mavg"],
          yAxis = 0,
          name = "BB Middle (SMA20)",
          color = "#90ed7d",
          type = "line",
          lineWidth = 1.5
        ) %>%
        
        # Add RSI
        hc_add_series(
          rsi.14,
          yAxis = 1,
          name = "RSI (14)",
          color = "#7cb5ec",
          type = "line"
        ) %>%
        hc_add_series(
          RSI.SellLevel,
          color = hex_to_rgba("red", 0.7),
          yAxis = 1,
          name = "Overbought (70)",
          dashStyle = "Dash"
        ) %>%
        hc_add_series(
          RSI.BuyLevel,
          color = hex_to_rgba("green", 0.7),
          yAxis = 1,
          name = "Oversold (30)",
          dashStyle = "Dash"
        ) %>%
        
        # Add MACD
        hc_add_series(
          macd[, "macd"],
          yAxis = 2,
          name = "MACD Line",
          type = "line",
          color = "#7cb5ec"
        ) %>%
        hc_add_series(
          macd[, "signal"],
          yAxis = 2,
          name = "Signal Line",
          type = "line",
          color = "#f45b5b"
        ) %>%
        hc_add_series(
          macd[, "macd"] - macd[, "signal"],
          yAxis = 2,
          name = "MACD Histogram",
          type = "column",
          color = "#434348"
        ) %>%
        
        # Apply dark theme for consistency
        hc_add_theme(
          hc_theme(
            chart = list(
              backgroundColor = "#151b3d",
              plotBackgroundColor = "#0a0e27"
            ),
            title = list(style = list(color = "#e8eaed", fontWeight = "600")),
            subtitle = list(style = list(color = "#b0b3b8")),
            yAxis = list(
              gridLineColor = "#2a3150",
              labels = list(style = list(color = "#e8eaed")),
              title = list(style = list(color = "#e8eaed"))
            ),
            xAxis = list(
              gridLineColor = "#2a3150",
              labels = list(style = list(color = "#e8eaed")),
              lineColor = "#2a3150"
            ),
            tooltip = list(
              backgroundColor = "#1a2142",
              style = list(color = "#e8eaed")
            ),
            legend = list(
              itemStyle = list(color = "#e8eaed"),
              itemHoverStyle = list(color = "#00d4ff")
            ),
            navigator = list(
              maskFill = "rgba(0, 212, 255, 0.1)",
              outlineColor = "#2a3150",
              series = list(
                color = "#00d4ff",
                lineColor = "#00d4ff"
              )
            ),
            scrollbar = list(
              barBackgroundColor = "#2a3150",
              barBorderColor = "#2a3150",
              buttonBackgroundColor = "#2a3150",
              buttonBorderColor = "#2a3150",
              rifleColor = "#e8eaed",
              trackBackgroundColor = "#151b3d",
              trackBorderColor = "#2a3150"
            )
          )
        ) %>%
        
        hc_title(
          text = paste("Technical Analysis:", input$stock),
          style = list(fontSize = "18px", fontWeight = "600")
        ) %>%
        hc_subtitle(
          text = "Buy Signal: RSI < 30 + BB Lower Touch | Sell Signal: RSI > 70 + MACD Bearish Cross"
        ) %>%
        hc_tooltip(
          valueDecimals = 2,
          shared = FALSE,
          split = FALSE
        ) %>%
        hc_rangeSelector(
          buttons = list(
            list(type = "month", count = 1, text = "1m"),
            list(type = "month", count = 3, text = "3m"),
            list(type = "month", count = 6, text = "6m"),
            list(type = "ytd", text = "YTD"),
            list(type = "year", count = 1, text = "1y"),
            list(type = "all", text = "All")
          ),
          selected = 4
        )
    })
    
  })
}
