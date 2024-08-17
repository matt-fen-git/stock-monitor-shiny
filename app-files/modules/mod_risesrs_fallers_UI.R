mod_risers_fallers_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    useShinyalert(),
    sidebarLayout(
    sidebarPanel(width = 2,
                 selectInput(ns("stock"), "Select Stock:", choices = names(stock_data_list)),
                 dateRangeInput(ns("dateRange"), "Select Date Range:", 
                                start = Sys.Date() - 180, 
                                end = Sys.Date()),
                 actionButton(ns("update"), "Update Chart"),
                 
    ),
    
    mainPanel(width = 8,
              withSpinner(gt_output(ns("faller_table")))
              
    )
  )
  )
}


mod_risers_fallers_Server <- function(id){
  
  moduleServer(id,function(input,output,session){
    
    ns <- session$ns
    
    # Trigger the popup when the user arrives
    shinyalert::shinyalert(
      title = "Reminder",
      text = "Please click the 'Update Chart' button after selecting your options.",
      type = "info",
      closeOnClickOutside = TRUE
    )
        
    table_data <- 
      eventReactive(
        input$update,{
          # Filter table by dates.
          faller_data <- all_dips_data %>% filter(between(as.Date(date),
                                                          as.Date(input$dateRange[1]),
                                                          as.Date(input$dateRange[2])))
        })
    
    output$faller_table <- 
      render_gt({
        
        faller_data <- table_data()
        
        faller_data  %>%
          gt() |> 
          cols_hide(outlier) |> 
          fmt_number(roc_z,decimals = 2) |> 
          fmt_currency(columns = c(2:7),currency = "USD",decimals = 2) |> 
          fmt_number(volume, decimals = 0 ) |> 
          gt::opt_interactive(use_filters = TRUE)
      })
    
  }
  )
}