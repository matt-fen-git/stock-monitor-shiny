# Load necessary libraries
library(shiny)
library(shinycssloaders)
library(quantmod)
library(markdown)
library(shinyjs)
library(gt)
library(gtExtras)
# Read the content of the text file and convert it to Markdown
rsi_content <- suppressWarnings(readLines("~/Kaggle Projects/Stock Monitor/Descriptions/rsi_info.txt"))
rsi_content <- paste(rsi_content, collapse = "\n")

# Define UI for the Shiny app
ui <- navbarPage(
    "Stock Dashboard",
    useShinyjs(),  # Initialize shinyjs
    tabPanel("Stock Analysis",
             sidebarLayout(
                 sidebarPanel(width = 2,
                              selectInput("stock", "Select Stock:", choices = names(stock_data_list)),
                              dateRangeInput("dateRange", "Select Date Range:", 
                                             start = Sys.Date() - 180, 
                                             end = Sys.Date()),
                              actionButton("update", "Update Chart")
                 ),
                 mainPanel(width = 8,
                           plotOutput("stockChart")
                 )
             )
    ),
    tabPanel("Risers & Fallers",
             sidebarLayout(
                 sidebarPanel(width = 2,
                              selectInput("stock", "Select Stock:", choices = names(stock_data_list)),
                              dateRangeInput("dateRange", "Select Date Range:", 
                                             start = Sys.Date() - 180, 
                                             end = Sys.Date()),
                              actionButton("update", "Update Chart"),
                              
             ),
             
             mainPanel(width = 8,
                 withSpinner(gt_output("faller_table")),
                 
             )))
    
    ,
    
    tabPanel("Summary",
             fluidPage(
                 h3("Summary Metrics"),
                 verbatimTextOutput("summaryText")
             )
    ),
    
    tabPanel("About",
             fluidPage(
                 h3("About This Dashboard"),
                 p("This dashboard provides stock price analysis with various technical indicators.")
             )
    ),
    
    tabPanel("Glossary",
             fluidPage(
                 h2("Info about Key Metrics"),
                 actionButton("toggle_rsi", "Relative Strength Index (RSI)"),
                 div(id = "rsi_content", style = "display:none;", 
                     htmlOutput("glossaryText")  # Use htmlOutput for rendering Markdown content
                 )
             ),
             fluidPage(
                 h3("Great Package for Technical Analysis"),
                 p(tags$a(href = "https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/dygraphs-package.html",
                          "Click here"))
                 
             )
    )
)

# Define the server logic for the Shiny app
server <- function(input, output) {
    observeEvent(input$update, {
        data <- stock_data_list[[input$stock]]
        names(data) <-  tolower(sub(".*\\.","",colnames(data)))
        
        # Subset data based on the selected date range
        data <- data[paste(input$dateRange[1], input$dateRange[2], sep = "/")]
        
        output$stockChart <- renderPlot({
            options(repr.plot.width = 12, repr.plot.height = 6)
            chartSeries(data,
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
            summary(data)
        })

# Faller table ------------------------------------------------------------
        
        # Filter table by dates.
        faller_data <- all_dips_data %>% filter(between(date,as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
        
        output$faller_table <- 
            render_gt({
                
                faller_data  %>%
                    gt() |> 
                    cols_hide(outlier) |> 
                    fmt_number(roc_z,decimals = 2) |> 
                    fmt_currency(columns = c(2:7),currency = "USD",decimals = 2) |> 
                    fmt_number(volume, decimals = 0 ) |> 
                    gt::opt_interactive(use_filters = TRUE)
            })
    })
    
    # Render the Markdown content as HTML
    output$glossaryText <- renderUI({
        HTML(markdownToHTML(text = rsi_content, fragment.only = TRUE))
    })
    
    # Toggle the display of the RSI content
    observeEvent(input$toggle_rsi, {
        toggle("rsi_content")
    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
