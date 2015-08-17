## Blog: Alcoa vs. Aluminum Price
#
## Author: Richard I. Carpenter
#  Date Created: 15 Aug 2015
#  Date Updated: 16 Aug 2015
#
## 

## Installing packages
#  install.packages("devtools")
#  install.packages("ggplot2")
#  install.packages("zoo")
#  install.packages("dplyr")
#  install.packages("tidyr")
#  install.packages("Quandl")
#  install.packages("lubridate")
#  install.packages("compare")
library(devtools)
library(ggplot2)
library(zoo)
library(dplyr)
library(tidyr)
library(Quandl)
library(lubridate)
library(compare)

## Setting working directory
getwd()
setwd("~/blog/bp4")

## Setting up Quandl authcode
authcode <- "qRga9SqGnuPQvvgDyC_z"

## Alcoa stock price data and aluminum price data from Quandl.com
alcoa <- Quandl("GOOG/NYSE_AA", 
                authcode = authcode, 
                trim_start = "2015-01-01", 
                trim_end = "2015-08-13")
aluminum <- Quandl("LME/PR_AL", 
                   authcode = authcode, 
                   trim_start = "2015-01-01", 
                   trim_end = "2015-08-13")

## Renaming columns to get rid of spaces between names and prep for subset...
colnames(aluminum) <- c("Date", "cashBuyer", "cashSeller", "A1", "A2", "B1", 
                        "B2", "C1", "C2", "D1", "D2", "E1", "E2")
#  ... subsetting data.
alumData <- select(aluminum, Date:cashSeller)

## Looking at the dimensions of the datasets
dim(alumData) # 156  3
dim(alcoa) # 155  6
#  The difference is strange, so I dug into the data a little deeper...
#  ... first, I pulled summary statistics...
summary(alumData)
summary(alcoa)
#  ... then created a "check" for differences in dates due to operational 
#  differences between NYSE and LME...
commonDate <- intersect(alumData$Date, alcoa$Date)
#  ... and compared the differences.
alumData[!alumData$Date %in% commonDate, ]
alcoa[!alcoa$Date %in% commonDate, ]
#  These difference create problems for performing statistical analysis, such 
#  as `cor` and `lm`, and for combining the datasets using the `rbind()` function.

## Creating some quick data visualizations using ggplot2:
#  png("blog4plot1.png", height = 480, width = 720, unit = "px")
p1 <- ggplot(alumData, aes(x = Date, y = cashSeller))
p1 + geom_line() + geom_smooth() +
      ylab("Settlement Price") +
      ggtitle("LME Aluminum Settlement Price, 2015-01-03 to 2015-08-13")
#  dev.off()

#  png("blog4plot2.png", height = 480, width = 720, unit = "px")
p2 <- ggplot(alcoa, aes(x = Date, y = Close))
p2 + geom_line() + geom_smooth() +
      ylab("Stock Closing Price") +
      ggtitle("Alcoa Stock Closing Price, 2015-01-03 to 2015-08-13")
#  dev.off()
#  ... I commented out the .png() function.  You can turn it on if you'd like.

## To better analyze the datasets, I first "trimmed" the original datasets based 
#  on their shared dates:
alcoa2 <- alcoa[alcoa$Date %in% commonDate, ]
alumData2 <- alumData[alumData$Date %in% commonDate, ]
#  This allowed me to run a correlation between Alcoa's closing price and LME's
#  settlement price.
cor(alcoa2$Close, alumData2$cashSeller) # 0.8467066

## Pulling in stock/quantity data from Quandl...
#  ... primary aluminum stocks for LME holdings in all areas...
primeAlumStocks <- Quandl("LME/ST_PA_ALL", authcode = authcode, 
                          trim_start = "2015-01-01", 
                          trim_end = "2015-08-13")
#  ... aluminum alloy stocks for LME holdings in all areas...
alloyAlumStocks <- Quandl("LME/ST_AA_ALL", 
                          authcode = authcode, 
                          trim_start = "2015-01-01", 
                          trim_end = "2015-08-13")
#  ... changing column names to make analysis a little easier...
names(primeAlumStocks)
colnames(primeAlumStocks) <- c("Date", "Open", 
                               "DelIn", "DelOut",
                               "Close", "OpenTon", "CancTon")
names(alloyAlumStocks)
colnames(alloyAlumStocks) <- c("Date", "Open", 
                               "DelIn", "DelOut",
                               "Close", "OpenTon", "CancTon")
#  ... looking at dataset dimensions and summary statistics...
dim(primeAlumStocks)
dim(alloyAlumStocks)
summary(primeAlumStocks)
summary(alloyAlumStocks)
#  ... and running a quick plot on the closing quantities of stocks...
#  png("blog4plot3.png", height = 480, width = 720, unit = "px")
p3 <- ggplot(primeAlumStocks, aes(x = Date, y = Close))
p3 + geom_line() + geom_smooth() +
      ylab("Closing Quantity, tonnes") +
      ggtitle("LME Primary Aluminum Quantities, 2015-01-02 to 2015-08-11")
#  dev.off()

#  png("blog4plot4.png", height = 480, width = 720, unit = "px")
p4 <- ggplot(alloyAlumStocks, aes(x = Date, y = Close))
p4 + geom_line() + geom_smooth() +
      ylab("Closing Quantity, tonnes") +
      ggtitle("LME Aluminum Alloy Quantities, 2015-01-02 to 2015-08-11")
#  dev.off()
