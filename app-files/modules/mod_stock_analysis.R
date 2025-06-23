# It would be AMAZING to upgrade to highcharters
# https://jkunst.com/highcharter/articles/stock.html

# UI script
mod_stock_analysis_UI <- function(id){
  
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 2,
                 selectInput(ns("stock"), "Select Stock:", choices = names(stock_data_list)),
                 dateRangeInput(ns("dateRange"), "Select Date Range:", 
                                start = Sys.Date() - 365, 
                                end = Sys.Date()),
                 actionButton(ns("update"), "Update Chart")
    ),
    mainPanel(width = 8,
              #plotOutput(ns("stockChart"))
              highchartOutput(ns("stockChart"),height = "800px")
    )
  )
}


# Server File
mod_stock_analysis_Server <- function(id){
  
  moduleServer(id,function(input,output,session){
    
    ns <- session$ns
    #print(ns)
    
    filtered_data <- eventReactive(input$update, {
      
      this_stock <- stock_data_list[[input$stock]]
      
      
      names(this_stock) <-  tolower(sub(".*\\.","",colnames(this_stock)))
      
      # Subset data based on the selected date range
      this_stock <- this_stock[paste(input$dateRange[1], input$dateRange[2], sep = "/")]
      
      this_stock <- as.xts(this_stock)
      
      this_stock
    } )
    
    output$stockChart <- #renderPlot({
        renderHighchart({
      options(repr.plot.width = 12, repr.plot.height = 10)
      
      this_stock <- filtered_data() 
      
      this_set <-  xts(this_stock)
    
      
      bollinger_bands <- TTR::BBands(this_stock$close, n = 20, sd = 2) %>% as.data.frame() %>% drop_na()
      
      
      # Add Bollinger Bands to your data frame
      df <- bollinger_bands %>% 
        drop_na() %>%
        tibble::rownames_to_column(var = "Date") %>% 
        rename("Low" = "dn" ,"High" = "up")
      
      ts <- xts(df[,-1],order.by = as.Date(df$Date))
      
      # add RSI
      rsi.14 <- RSI(Cl(this_set))
      RSI.SellLevel <- xts(rep(70, NROW(this_set)), index(this_set))
      RSI.BuyLevel <- xts(rep(30, NROW(this_set)), index(this_set))
      
      # add moving average convergance divergance
      macd <- MACD(Cl(this_set),maType = "SMA")
      
      tseries  <-
        hchart(object = this_set, type = "candlestick") %>%
        hc_yAxis_multiples(create_axis(3, height = c(2, 1, 1), turnopposite = TRUE)) |> 
        hc_add_theme(hc_theme_db()) %>%
        hc_add_series(Cl(this_set),yAxis = 0,name = "Close",  color = "#2b908f") %>%
        hc_add_series(data = Hi(ts),yAxis = 0,  name = "Upper Band", color = "#f45b5b") %>%
        hc_add_series(data = Lo(ts),yAxis = 0,  name = "Lower Band", color = "#f45b5b") %>%
        hc_add_series(data = rsi.14,yAxis = 1,name = "RSI-14day") %>%
        hc_add_series(RSI.SellLevel, color = hex_to_rgba("red", 0.7), yAxis = 1, name = "Sell level") |> 
        hc_add_series(RSI.BuyLevel, color = hex_to_rgba("blue", 0.7), yAxis = 1, name = "Buy level") |> 
        hc_add_series(data = macd,type = "column",yAxis = 2,name = "MACD") %>%
        hc_tooltip(valueDecimals = 2)
      
      
      return(tseries)
    })
    
    output$summaryText <- renderPrint({
      summary(this_stock)
    })
    
  }
  )
}