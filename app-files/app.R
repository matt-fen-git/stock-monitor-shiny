library(shiny)
library(bslib)
library(gridlayout)
# source global & module files

source("global.R")


# Call & Run the modules.
invisible( purrr::map(dir("modules/",full.names = TRUE,recursive = T),
                      source))


# UI
ui <- navbarPage(
  "Stock Dashboard",
  useShinyjs(),  # Initialize shinyjs

  tabPanel(
    "Stock Analysis",
    fluidPage(
      mod_stock_analysis_UI("stockAnalysis"),
            tags$style(HTML("#stockAnalysis-stockPlot {height: 1200px;}"))  # CSS to increase height

    )
  ),

  tabPanel(
    "Risers & Fallers",
    fluidPage(
      mod_risers_fallers_UI("risersFallers")
    )
  )
 )

# New UI ------------------------------------------------------------------
#ui <- grid_


server <- function(input, output, session) {
  
  mod_stock_analysis_Server("stockAnalysis")
  
  mod_risers_fallers_Server("risersFallers")
  # mod_summary_Server("summary")
  #  mod_about_Server("about")
  #  mod_glossary_Server("glossary")
}

shinyApp(ui,server)
