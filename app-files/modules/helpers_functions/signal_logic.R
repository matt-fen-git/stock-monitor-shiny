# Signal generation and scoring logic

#' Determine market regime from ADX
get_market_regime <- function(adx_val) {
  if (is.na(adx_val)) adx_val <- 25
  if (adx_val < 25) "ranging" else "trending"
}

#' Get indicator weights based on market regime
get_indicator_weights <- function(regime, selected_indicators) {
  if (regime == "ranging") {
    weights <- list(rsi = 0.30, stoch = 0.25, bb = 0.20, mfi = 0.15,
                   macd = 0.05, obv = 0.05, ema = 0, adx = 0)
  } else {
    weights <- list(rsi = 0.05, stoch = 0.05, bb = 0.05, mfi = 0.05,
                   macd = 0.30, obv = 0.25, ema = 0.25, adx = 0)
  }
  
  # Multicollinearity adjustment
  if ("stoch" %in% selected_indicators) weights$stoch <- weights$stoch * 0.5
  if ("mfi" %in% selected_indicators && "rsi" %in% selected_indicators) {
    weights$mfi <- weights$mfi * 0.5
  }
  
  weights
}

#' Calculate RSI signal using Z-score
calculate_rsi_signal <- function(rsi_full, current_rsi) {
  rolling_mean <- runMean(rsi_full, n = 90)
  rolling_sd <- runSD(rsi_full, n = 90)
  mean_val <- as.numeric(tail(rolling_mean, 1))
  sd_val <- as.numeric(tail(rolling_sd, 1))
  
  zscore <- if (!is.na(sd_val) && sd_val > 0) {
    (current_rsi - mean_val) / sd_val
  } else 0
  
  if (is.na(current_rsi)) current_rsi <- 50
  
  signal <- if (zscore < -2.0) "Strong Buy"
            else if (zscore < -1.25) "Buy"
            else if (zscore > 2.0) "Strong Sell"
            else if (zscore > 1.25) "Sell"
            else "Neutral"
  
  list(signal = signal, 
       score = pmin(100, pmax(0, 50 - (zscore * 25))),
       value = round(current_rsi, 1))
}

# Add similar functions for BB, MACD, etc.
