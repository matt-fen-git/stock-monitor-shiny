# Investigating highcharter
library(quantmod)
library(TTR)
library(dplyr)
library(tidyr)
this_set <- stock_data_list$`Oracle Corporation`


library(highcharter)


# Putting it into practice ------------------------------------------------

#https://api.highcharts.com/highstock/plotOptions
# Investigating highcharter
this_set <- stock_data_list$`Oracle Corporation`

this_set <- getSymbols("NIO", src = "yahoo", auto.assign = FALSE)

names(this_set) <-  tolower(sub(".*\\.","",colnames(this_set)))


bollinger_bands <- TTR::BBands(this_set$close, n = 20, sd = 2) %>% as.data.frame() %>% drop_na()


# Add additional indicators 
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
macd <- MACD(Cl(this_set),maType = "SMA",nSlow = 9,nFast = 12)

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

