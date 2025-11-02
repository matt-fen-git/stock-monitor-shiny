# Technical indicator calculation functions
# Sourced in global.R or at top of mod_stock_analysis.R

#' Calculate all base technical indicators
#' @param stock_clean xts object with OHLCV data
#' @return Named list of calculated indicators
calculate_all_indicators <- function(stock_clean) {
  list(
    rsi_full = RSI(Cl(stock_clean), n = 14),
    bb = BBands(Cl(stock_clean), n = 20, sd = 2),
    macd = MACD(Cl(stock_clean), maType = "SMA"),
    adx = ADX(HLC(stock_clean), n = 14),
    current_price = as.numeric(tail(Cl(stock_clean), 1))
  )
}

#' Calculate optional indicators based on user selection
calculate_optional_indicators <- function(stock_clean, selected) {
  indicators <- list()
  
  if ("stoch" %in% selected) {
    indicators$stoch <- stoch(HLC(stock_clean), nFastK = 14, nSlowK = 3, nSlowD = 3)
  }
  if ("mfi" %in% selected) {
    indicators$mfi <- MFI(HLC(stock_clean), Vo(stock_clean), n = 14)
  }
  if ("obv" %in% selected) {
    indicators$obv <- OBV(Cl(stock_clean), Vo(stock_clean))
  }
  if ("adx" %in% selected) {
    indicators$adx <- ADX(HLC(stock_clean), n = 14)
  }
  if ("ema" %in% selected) {
    indicators$ema50 <- EMA(Cl(stock_clean), n = 50)
    indicators$ema200 <- EMA(Cl(stock_clean), n = 200)
  }
  
  indicators
}
