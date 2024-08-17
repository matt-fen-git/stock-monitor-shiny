# UI script
mod_stock_analysis_UI <- function(id){
  
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 2,
                 selectInput(ns("stock"), "Select Stock:", choices = names(stock_data_list)),
                 dateRangeInput(ns("dateRange"), "Select Date Range:", 
                                start = Sys.Date() - 180, 
                                end = Sys.Date()),
                 actionButton(ns("update"), "Update Chart")
    ),
    mainPanel(width = 8,
              plotOutput(ns("stockChart"))
    )
  )
}


# Server File
mod_stock_analysis_Server <- function(id){
  
  moduleServer(id,function(input,output,session){
    
    ns <- session$ns
    print(ns)
    
    filtered_data <- eventReactive(input$update, {
      
      this_stock <- stock_data_list[[input$stock]]
      
      
      names(this_stock) <-  tolower(sub(".*\\.","",colnames(this_stock)))
      
      # Subset data based on the selected date range
      this_stock <- this_stock[paste(input$dateRange[1], input$dateRange[2], sep = "/")]
      
      this_stock <- as.xts(this_stock)
      
      this_stock
    } )
    
    output$stockChart <- renderPlot({
      
      #options(repr.plot.width = 12, repr.plot.height = 6)
      
      this_stock <- filtered_data()
      # print(this_stock)
       #chartSeries(this_stock)
      chartSeries(this_stock,
                  theme = "white",
                  bar.type = "ohlc",
                  TA = "addRSI();
                        addBBands();
                        addMACD();
                        addSMI();")
      addBBands(n = 20,sd =2)
      addROC(n = 7,col = "green")
      
    })
    
    output$summaryText <- renderPrint({
      summary(this_stock)
    })
    
  }
  )
}