stock_data_list <- readRDS("data/stock_data_list.rds")
# test visuals & monitors

symbol_name <- "Intel"

data <- stock_data_list[[symbol_name]]
data <- stock_data_list$Nvidia

names(data) <-  tolower(sub(".*\\.","",colnames(data)))

# data |> autoplot(geom = "line") + theme_classic()

options(repr.plot.width = 6, repr.plot.height = 3)
chartSeries(data,
            #subset = "2024::2024-07",
            theme="white",
            bar.type = "ohlc",
            subset = 'last 6 months',
            TA="addRSI();
            addMACD();
            addSMI();") 
addBBands(n = 20,sd =2)
addROC(n = 7,col = "green")


