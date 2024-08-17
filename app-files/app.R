library(shiny)

# source global & module files

source("global.R")


# Call & Run the modules.
invisible( purrr::map(dir("modules/",full.names = TRUE,recursive = T),
           source))


# UI
ui <- navbarPage(
  "Stock Dashboard",
  useShinyjs(),  # Initialize shinyjs
  
  tabPanel("Stock Analysis", mod_stock_analysis_UI("stockAnalysis")),
 
  tabPanel("Risers & Fallers", mod_risers_fallers_UI("risersFallers")),
  # tabPanel("Summary", mod_summary_UI("summary")),
  # tabPanel("About", mod_about_UI("about")),
  # tabPanel("Glossary", mod_glossary_UI("glossary"))
)


server <- function(input, output, session) {
  
  mod_stock_analysis_Server("stockAnalysis")
  
  mod_risers_fallers_Server("risersFallers")
 # mod_summary_Server("summary")
#  mod_about_Server("about")
#  mod_glossary_Server("glossary")
}

shinyApp(ui,server)
