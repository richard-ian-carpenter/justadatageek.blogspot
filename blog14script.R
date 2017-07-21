# Blog 14 Script: Revisiting Oil
# Author: Richard I. Carpenter
# Date Created: 30 Jun 2017
# Date Updated: 20 Jul 2017

library(tidyverse)
library(jsonlite)
library(lubridate)
library(sqldf)

# eia_api <- ...

urlOilPrice <- paste0("http://api.eia.gov/series/?api_key=",
              eia_api,
              "&series_id=PET.RWTC.M&out=json")

urlOilProduction <- paste0("http://api.eia.gov/series/?api_key=",
                           eia_api,
                           "&series_id=PET.MCRFPUS1.M&out=json")

urlOilStocks <- paste0("http://api.eia.gov/series/?api_key=",
                       eia_api,
                       "&series_id=PET.MTTSTUS1.M&out=json")

# Reading in via jsonlite:
eiaOilPriceData <- fromJSON(urlOilPrice)
eiaOilProdData <- fromJSON(urlOilProduction)
eiaOilStockData <- fromJSON(urlOilStocks)

# Separating out the date and price data:
dfOilPrice <- data.frame(eiaOilPriceData$series$data)
colnames(dfOilPrice) <- c("date", "price")
dfOilProd <- data.frame(eiaOilProdData$series$data)
colnames(dfOilProd) <- c("date", "prodQty")
dfOilStocks <- data.frame(eiaOilStockData$series$data)
colnames(dfOilStocks) <- c("date", "stockQty")

# Cleaning up the date:
dfOilPrice$date <- as.character(dfOilPrice$date)
dfOilProd$date <- as.character(dfOilProd$date)
dfOilStocks$date <- as.character(dfOilStocks$date)

dfOilPrice$date <- ymd(paste0(dfOilPrice$date,"01"))
dfOilProd$date <- ymd(paste0(dfOilProd$date,"01"))
dfOilStocks$date <- ymd(paste0(dfOilStocks$date,"01"))

# Sorting by date:
dfOilPrice <- dfOilPrice[order(dfOilPrice$date), ]
dfOilProd <- dfOilProd[order(dfOilProd$date), ]
dfOilStocks <- dfOilStocks[order(dfOilStocks$date), ]

# Cleaning up the price:
dfOilPrice$price <- as.numeric(as.character(dfOilPrice$price))
dfOilProd$prodQty <- as.numeric(as.character(dfOilProd$prodQty))
dfOilStocks$stockQty <- as.numeric(as.character(dfOilStocks$stockQty))

# Simple plot:
ggplot(data = dfOilPrice, aes(x = date, y = price)) +
    geom_line() +
    geom_smooth() +
    ggtitle("WTI Crude Oil Spot Price")

ggplot(data = dfOilProd, aes(x = date, y = prodQty)) +
    geom_line() +
    geom_smooth() +
    ggtitle("WTI Crude Oil Production")

ggplot(data = dfOilStocks, aes(x = date, y = stockQty)) +
    geom_line() +
    geom_smooth() +
    ggtitle("WTI Crude Oil Stocks")

# Subsetting Production and Stocks data to have the same start date
# as Price data.

dfOilProdSub <- dfOilProd[dfOilProd$date >= "1986-01-01", ]
dfOilStocksSub <- dfOilStocks[dfOilStocks$date >= "1986-01-01", ]

ggplot(data = dfOilStocksSub, aes(x = date, y = stockQty)) +
    geom_line() +
    geom_smooth() +
    ggtitle("WTI Crude Oil Weekly Stocks Estimates")

ggplot(data = dfOilProdSub, aes(x = date, y = prodQty)) +
    geom_line() +
    geom_smooth() +
    ggtitle("WTI Crude Oil Weekly Production")

# Splitting up date for more indepth exploration:

date <- as.character(dfOilPrice$date)
year <- sapply(date, function(x) {strsplit(x, split = "-")[[1]][1]})
month <- sapply(date, function(x) {strsplit(x, split = "-")[[1]][2]})

dfOilPrice$year <- year
dfOilPrice$month <- month

# Interesting plots with averages:
png(filename = "./blog/bp14/blog14plot1.png",
    width = 720, height = 480, unit = "px")
ggplot(data = dfOilPrice, aes(x = date, y = price)) +
    geom_line() + 
    geom_hline(aes(yintercept = mean(dfOilPrice$price)), color = "red") + 
    scale_x_date(date_minor_breaks = "1 year") +
    scale_y_continuous(labels = scales::dollar,
                       minor_breaks = c(10, 20, 30, 40, 60, 70, 
                                        80, 90, 110, 120, 130, 140))
dev.off()

png(filename = "./blog/bp14/blog14plot2.png",
    width = 720, height = 480, unit = "px")
ggplot(data = dfOilPrice, aes(x = year, y = price)) +
    geom_boxplot() +
    xlab("Year") + ylab("Price") +
    ggtitle("WTI Crude Oil Prices, Jan 1986 to Jul 2017") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    scale_y_continuous(labels = scales::dollar,
                       minor_breaks = c(10, 20, 30, 40, 60, 70, 
                                        80, 90, 110, 120, 130, 140)) +
    geom_hline(aes(yintercept = mean(dfOilPrice$price)), color = "red")
dev.off()

png(filename = "./blog/bp14/blog14plot3.png",
    width = 720, height = 480, unit = "px")
ggplot(data = dfOilPrice, aes(x = month, y = price)) +
    geom_boxplot() +
    scale_y_continuous(labels = scales::dollar,
                       minor_breaks = c(10, 20, 30, 40, 60, 70, 
                                        80, 90, 110, 120, 130, 140)) +
    geom_hline(aes(yintercept = mean(dfOilPrice$price)), color = "red")
dev.off()

