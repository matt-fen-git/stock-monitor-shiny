library(shiny)
library(bslib)
library(gridlayout)

# Source global & module files
source(here::here("app-files", "global.R"), chdir = TRUE)
invisible(purrr::map(dir("modules/", full.names = TRUE, recursive = TRUE), source))

# Dark theme configuration
dark_theme <- bs_theme(
  version = 5,
  preset = "darkly",
  bg = "#0a0e27",
  fg = "#e8eaed",
  primary = "#00d4ff",
  secondary = "#6c757d",
  success = "#00ff88",
  danger = "#ff4757",
  warning = "#ffa502",
  info = "#3742fa",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat", wght = 600),
  code_font = font_google("Fira Mono")
)

# Additional custom CSS
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
  
  .indicator-card {
    background: linear-gradient(135deg, #1a2142 0%, #151b3d 100%);
    border: 1px solid #2a3150;
    border-radius: 8px;
    padding: 20px;
    margin-bottom: 20px;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
  }
  
  .indicator-card h3 {
    color: #00d4ff;
    font-weight: 600;
    margin-bottom: 10px;
  }
  
  .indicator-card h4 {
    color: #90ed7d;
    font-weight: 500;
    margin-top: 15px;
  }
  
  .buy-signal {
    background: rgba(0, 255, 136, 0.1);
    border-left: 4px solid #00ff88;
    padding: 10px;
    margin: 10px 0;
  }
  
  .sell-signal {
    background: rgba(255, 71, 87, 0.1);
    border-left: 4px solid #ff4757;
    padding: 10px;
    margin: 10px 0;
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
  
  .btn-primary {
    background: linear-gradient(135deg, #00d4ff 0%, #0099cc 100%);
    border: none;
    font-weight: 600;
    box-shadow: 0 4px 12px rgba(0, 212, 255, 0.3);
  }
  
  .bslib-value-box {
    background: linear-gradient(135deg, #1a2142 0%, #151b3d 100%) !important;
    border: 1px solid #2a3150 !important;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
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
  tags$head(tags$style(HTML(dark_css))),
  
  tabPanel(
    "Stock Analysis",
    icon = icon("magnifying-chart"),
    fluidPage(
      mod_stock_analysis_UI("stockAnalysis"),
      tags$style(HTML("#stockAnalysis-stockChart {height: 900px;}"))
    )
  ),
  
  tabPanel(
    "Risers & Fallers",
    icon = icon("chart-line-down"),
    fluidPage(
      mod_risers_fallers_UI("risersFallers")
    )
  ),
  
  tabPanel(
    "Indicator Guide",
    icon = icon("book"),
    fluidPage(
      style = "padding: 20px;",
      
      h2("Technical Indicator Reference Guide", style = "color: #00d4ff; text-align: center; margin-bottom: 30px;"),
      
      # Core Indicators
      div(
        class = "indicator-card",
        h3(icon("chart-simple"), " ROC Z-Score (Rate of Change Z-Score)"),
        p("Your primary dip detector. Measures how unusual today's price change is compared to recent history using statistical standard deviations."),
        h4("Interpretation:"),
        tags$ul(
          tags$li(HTML("<strong>< -2:</strong> Statistically significant dip (2 standard deviations) - moderate buy signal")),
          tags$li(HTML("<strong>< -2.5:</strong> Severe dip - strong buy signal")),
          tags$li(HTML("<strong>< -3:</strong> Extreme oversold condition - very strong buy signal"))
        ),
        div(
          class = "buy-signal",
          strong("Best For: "), "Identifying when panic selling creates opportunities. Use this to filter the Risers & Fallers table."
        )
      ),
      
      div(
        class = "indicator-card",
        h3(icon("arrow-trend-up"), " RSI (Relative Strength Index, 14-period)"),
        p("Momentum oscillator ranging 0-100 that shows if a stock is overbought or oversold based purely on price movements."),
        h4("Interpretation:"),
        tags$ul(
          tags$li(HTML("<strong>< 30:</strong> Oversold - potential buy signal")),
          tags$li(HTML("<strong>> 70:</strong> Overbought - potential sell signal")),
          tags$li(HTML("<strong>30-70:</strong> Normal range"))
        ),
        p(style = "color: #ffa502;", icon("triangle-exclamation"), " Note: During strong trends, RSI can stay extreme for extended periods. Always combine with other indicators."),
        div(
          class = "buy-signal",
          strong("Best For: "), "Confirming z-score dips aren't part of sustained downtrends. Buy when RSI < 30 and rising."
        )
      ),
      
      div(
        class = "indicator-card",
        h3(icon("arrows-up-down"), " Bollinger Bands (20-period, 2 std dev)"),
        p("Price envelope showing volatility boundaries. Consists of three lines: Upper Band, Middle Band (20-day SMA), and Lower Band."),
        h4("Interpretation:"),
        tags$ul(
          tags$li(HTML("<strong>Price touches Lower Band:</strong> Oversold, potential bounce")),
          tags$li(HTML("<strong>Price at Middle Band:</strong> Fair value")),
          tags$li(HTML("<strong>Price touches Upper Band:</strong> Overbought, potential pullback")),
          tags$li(HTML("<strong>Narrowing bands:</strong> Low volatility before breakout")),
          tags$li(HTML("<strong>Widening bands:</strong> High volatility during trend"))
        ),
        div(
          class = "buy-signal",
          strong("Best For: "), "Visual confirmation of dip extremity. Buy when price touches lower band during a dip with RSI < 30."
        )
      ),
      
      div(
        class = "indicator-card",
        h3(icon("wave-square"), " MACD (Moving Average Convergence Divergence)"),
        p("Shows relationship between 12-day and 26-day EMAs. Consists of MACD Line, Signal Line, and Histogram."),
        h4("Interpretation:"),
        tags$ul(
          tags$li(HTML("<strong>MACD > Signal:</strong> Bullish momentum")),
          tags$li(HTML("<strong>MACD < Signal:</strong> Bearish momentum")),
          tags$li(HTML("<strong>Histogram bars growing:</strong> Strengthening momentum")),
          tags$li(HTML("<strong>Bullish divergence:</strong> Price makes new low but MACD doesn't - weakening downtrend"))
        ),
        div(
          class = "buy-signal",
          strong("Best For: "), "Timing entries after a dip bottoms. Wait for MACD to cross above Signal Line for confirmation."
        ),
        div(
          class = "sell-signal",
          strong("Sell Signal: "), "MACD crosses below Signal Line with RSI > 70."
        )
      ),
      
      h2("Volume Indicators", style = "color: #00d4ff; text-align: center; margin: 40px 0 20px 0;"),
      
      div(
        class = "indicator-card",
        h3(icon("chart-area"), " OBV (On-Balance Volume)"),
        p("Cumulative volume flow indicator. Adds volume on up days and subtracts volume on down days to show institutional money flow."),
        h4("Interpretation:"),
        tags$ul(
          tags$li(HTML("<strong>Rising OBV + Falling Price:</strong> Bullish divergence - institutions accumulating (STRONG BUY)")),
          tags$li(HTML("<strong>Falling OBV + Falling Price:</strong> Distribution - avoid catching falling knife")),
          tags$li(HTML("<strong>Rising OBV + Rising Price:</strong> Healthy uptrend"))
        ),
        div(
          class = "buy-signal",
          strong("Best For: "), "Confirming smart money is buying the dip, not just retail panic. Only buy dips with rising OBV."
        )
      ),
      
      div(
        class = "indicator-card",
        h3(icon("money-bill-trend-up"), " MFI (Money Flow Index, 14-period)"),
        p("Volume-weighted RSI combining price and volume. More reliable than RSI alone because it measures the strength of money flowing in/out."),
        h4("Interpretation:"),
        tags$ul(
          tags$li(HTML("<strong>< 20:</strong> Oversold with volume confirmation (STRONG BUY)")),
          tags$li(HTML("<strong>> 80:</strong> Overbought with volume")),
          tags$li(HTML("<strong>High volume + MFI < 20:</strong> Capitulation - best buy opportunity"))
        ),
        div(
          class = "buy-signal",
          strong("Best For: "), "Distinguishing high-conviction dips from weak bounces. MFI < 20 with high volume = institutional capitulation."
        )
      ),
      
      h2("Momentum Indicators", style = "color: #00d4ff; text-align: center; margin: 40px 0 20px 0;"),
      
      div(
        class = "indicator-card",
        h3(icon("gauge-high"), " Stochastic Oscillator (14,3,3)"),
        p("Compares closing price to price range over 14 periods. More sensitive than RSI, providing earlier signals."),
        h4("Interpretation:"),
        tags$ul(
          tags$li(HTML("<strong>%K < 20:</strong> Oversold - potential buy")),
          tags$li(HTML("<strong>%K > 80:</strong> Overbought - potential sell")),
          tags$li(HTML("<strong>%K crosses above %D while both < 20:</strong> Strong buy signal"))
        ),
        p(style = "color: #ffa502;", icon("triangle-exclamation"), " Warning: More false signals than RSI due to higher sensitivity."),
        div(
          class = "buy-signal",
          strong("Best For: "), "Getting in early when dip is bottoming. Buy when Stoch < 20 AND RSI < 30 for confirmation."
        )
      ),
      
      div(
        class = "indicator-card",
        h3(icon("chart-line"), " ADX (Average Directional Index, 14-period)"),
        p("Measures trend strength (not direction) from 0-100. Helps determine if conditions are right for mean-reversion trading."),
        h4("Interpretation:"),
        tags$ul(
          tags$li(HTML("<strong>< 25:</strong> Weak/no trend - ideal for dip buying (mean reversion)")),
          tags$li(HTML("<strong>> 25:</strong> Strong trend - avoid dips as they may continue")),
          tags$li(HTML("<strong>+DI > -DI:</strong> Bullish directional movement")),
          tags$li(HTML("<strong>-DI > +DI:</strong> Bearish directional movement"))
        ),
        div(
          class = "buy-signal",
          strong("Best For: "), "Avoiding 'value traps'. Only buy dips when ADX < 25 or when +DI crosses above -DI."
        )
      ),
      
      h2("Trend Context Indicators", style = "color: #00d4ff; text-align: center; margin: 40px 0 20px 0;"),
      
      div(
        class = "indicator-card",
        h3(icon("arrow-trend-up"), " EMA 50 & 200 (Exponential Moving Averages)"),
        p("Long-term trend indicators that provide big-picture context for dip buying decisions."),
        h4("EMA 50 (Short-term trend):"),
        tags$ul(
          tags$li(HTML("<strong>Price > EMA 50:</strong> Bullish structure - buy dips confidently")),
          tags$li(HTML("<strong>Price < EMA 50:</strong> Caution - dips may continue"))
        ),
        h4("EMA 200 (Long-term trend):"),
        tags$ul(
          tags$li(HTML("<strong>Price > EMA 200:</strong> Bull market - buy dips")),
          tags$li(HTML("<strong>Price < EMA 200:</strong> Bear market - dips are riskier")),
          tags$li(HTML("<strong>Golden Cross (EMA 50 > EMA 200):</strong> Major bullish signal"))
        ),
        div(
          class = "buy-signal",
          strong("Best For: "), "Macro context. Only buy aggressive dips if price is above EMA 200."
        )
      ),
      
      h2("Decision Framework", style = "color: #00d4ff; text-align: center; margin: 40px 0 20px 0;"),
      
      fluidRow(
        column(
          6,
          div(
            class = "indicator-card buy-signal",
            h3(icon("circle-check"), " Conservative Buy Signal"),
            tags$ul(
              tags$li("Z-score < -2"),
              tags$li("RSI < 30"),
              tags$li("Price touches BB Lower Band"),
              tags$li("OBV rising (divergence)"),
              tags$li("ADX < 25 (weak trend)"),
              tags$li("Price above EMA 200"),
              tags$li("MFI < 30")
            )
          ),
          div(
            class = "indicator-card buy-signal",
            h3(icon("rocket"), " Aggressive Buy Signal"),
            tags$ul(
              tags$li("Z-score < -2.5"),
              tags$li("RSI < 25 AND Stochastic < 20"),
              tags$li("MFI < 20 (high volume selloff)"),
              tags$li("MACD histogram turning positive"),
              tags$li("Price near EMA 50 support")
            )
          )
        ),
        column(
          6,
          div(
            class = "indicator-card sell-signal",
            h3(icon("circle-xmark"), " Sell/Take Profit Signal"),
            tags$ul(
              tags$li("RSI > 70"),
              tags$li("MACD bearish cross"),
              tags$li("OBV divergence (price up, OBV down)"),
              tags$li("Price reaches BB Upper Band"),
              tags$li("MFI > 80")
            )
          ),
          div(
            class = "indicator-card sell-signal",
            h3(icon("triangle-exclamation"), " Avoid/Exit Signal"),
            tags$ul(
              tags$li("Z-score dip with falling OBV"),
              tags$li("ADX > 40 with -DI > +DI"),
              tags$li("Price below EMA 200"),
              tags$li("RSI < 30 but making lower lows")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  mod_stock_analysis_Server("stockAnalysis")
  mod_risers_fallers_Server(
    id = "risersFallers",
    all_dips_data = all_dips_data,      # Pass the global object here
    stock_data_list = stock_data_list   # Pass the global object here
  )}

shinyApp(ui, server)
