# library(shiny)
# library(bslib)
# library(gridlayout)
# 
# # Source global & module files
# source("global.R")
# invisible(purrr::map(dir("modules/", full.names = TRUE, recursive = TRUE), source))
# 
# # UI
# ui <- navbarPage(
#   theme = bs_theme(bootswatch = "flatly"),
#   "Stock Dashboard",
#   useShinyjs(),  # Initialize shinyjs
#   tabPanel(
#     "Stock Analysis",
#     fluidPage(
#       mod_stock_analysis_UI("stockAnalysis"),
#       tags$style(HTML("#stockAnalysis-stockChart {height: 800px;}"))  # Fixed CSS ID to match output
#     )
#   ),
#   tabPanel(
#     "Risers & Fallers",
#     fluidPage(
#       mod_risers_fallers_UI("risersFallers")
#     )
#   )
# )
# 
# # Server
# server <- function(input, output, session) {
#   mod_stock_analysis_Server("stockAnalysis")
#   mod_risers_fallers_Server("risersFallers")
# }
# 
# shinyApp(ui, server)
library(shiny)
library(bslib)
library(gridlayout)

# Source global & module files
source("global.R")
invisible(purrr::map(dir("modules/", full.names = TRUE, recursive = TRUE), source))

# Dark theme configuration
dark_theme <- bs_theme(
  version = 5,
  preset = "darkly",  # Bootstrap dark preset
  bg = "#0a0e27",  # Deep navy background
  fg = "#e8eaed",  # Light text
  primary = "#00d4ff",  # Cyan accent for buttons/highlights
  secondary = "#6c757d",
  success = "#00ff88",  # Bright green for buy signals
  danger = "#ff4757",  # Red for warnings
  warning = "#ffa502",  # Orange for caution
  info = "#3742fa",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat", wght = 600),
  code_font = font_google("Fira Mono")
)

# Additional custom CSS for dark mode refinements
dark_css <- "
  body {
    background-color: #0a0e27;
    color: #e8eaed;
  }
  
  .navbar {
    background: linear-gradient(135deg, #1a1f3a 0%, #0a0e27 100%) !important;
    border-bottom: 2px solid #00d4ff;
  }
  
  .navbar-brand, .nav-link {
    color: #e8eaed !important;
    font-weight: 500;
  }
  
  .nav-link:hover {
    color: #00d4ff !important;
  }
  
  .well, .panel {
    background-color: #151b3d;
    border: 1px solid #2a3150;
    border-radius: 8px;
  }
  
  .form-control, .selectize-input {
    background-color: #1a2142 !important;
    color: #e8eaed !important;
    border: 1px solid #2a3150 !important;
  }
  
  .form-control:focus, .selectize-input:focus {
    background-color: #212844 !important;
    border-color: #00d4ff !important;
    box-shadow: 0 0 0 0.2rem rgba(0, 212, 255, 0.25);
  }
  
  .btn-primary {
    background: linear-gradient(135deg, #00d4ff 0%, #0099cc 100%);
    border: none;
    font-weight: 600;
    box-shadow: 0 4px 12px rgba(0, 212, 255, 0.3);
  }
  
  .btn-primary:hover {
    background: linear-gradient(135deg, #00e5ff 0%, #00aadd 100%);
    box-shadow: 0 6px 16px rgba(0, 212, 255, 0.4);
  }
  
  /* Value box styling for dark mode */
  .bslib-value-box {
    background: linear-gradient(135deg, #1a2142 0%, #151b3d 100%) !important;
    border: 1px solid #2a3150 !important;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
  }
  
  .bslib-value-box .value-box-showcase {
    color: #00d4ff;
  }
  
  /* GT table dark mode adjustments */
  #risersFallers-faller_table table {
    background-color: #151b3d !important;
    color: #e8eaed !important;
  }
  
  #risersFallers-faller_table th {
    background-color: #1a2142 !important;
    color: #00d4ff !important;
    border-bottom: 2px solid #00d4ff !important;
  }
  
  #risersFallers-faller_table td {
    border-color: #2a3150 !important;
  }
  
  #risersFallers-faller_table tr:hover {
    background-color: #212844 !important;
  }
  
  /* Highcharts dark mode */
  #stockAnalysis-stockChart {
    background-color: #151b3d !important;
    border: 1px solid #2a3150;
    border-radius: 8px;
  }
  
  /* Slider styling */
  .irs--shiny .irs-bar {
    background: linear-gradient(to bottom, #00d4ff 0%, #0099cc 100%);
    border-top: 1px solid #00d4ff;
    border-bottom: 1px solid #00d4ff;
  }
  
  .irs--shiny .irs-handle {
    background: #00d4ff;
    border: 2px solid #e8eaed;
  }
  
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    background: #00d4ff;
    color: #0a0e27;
    font-weight: 600;
  }
  
  /* Radio buttons dark mode */
  .radio label, .checkbox label {
    color: #e8eaed;
  }
  
  /* Date range input */
  .input-daterange .input-group-addon {
    background-color: #1a2142;
    color: #e8eaed;
    border-color: #2a3150;
  }
"

# UI
ui <- navbarPage(
  theme = dark_theme,
  title = div(
    icon("chart-line"),
    "Stock Dashboard",
    style = "font-size: 20px; font-weight: 600;"
  ),
  useShinyjs(),
  tags$head(tags$style(HTML(dark_css))),  # Inject custom dark CSS
  
  tabPanel(
    "Stock Analysis",
    icon = icon("magnifying-chart"),
    fluidPage(
      mod_stock_analysis_UI("stockAnalysis"),
      tags$style(HTML("#stockAnalysis-stockChart {height: 800px;}"))
    )
  ),
  tabPanel(
    "Risers & Fallers",
    icon = icon("chart-line-down"),
    fluidPage(
      mod_risers_fallers_UI("risersFallers")
    )
  )
)

# Server
server <- function(input, output, session) {
  mod_stock_analysis_Server("stockAnalysis")
  mod_risers_fallers_Server("risersFallers")
}

shinyApp(ui, server)
