## Blog: First Post
#
## Author: Richard I. Carpenter
#  Date Created: 27 Jun 2015
#  Date Updated: 11 Aug 2015
#
## 

## Installing packages
install.packages("devtools")
install.packages("ggplot2")
install.packages("zoo")
install.packages("dplyr")
install.packages("tidyr")
install.packages("Quandl")
install.packages("lubridate")
library(devtools)
library(ggplot2)
library(zoo)
library(dplyr)
library(tidyr)
library(Quandl)
library(lubridate)

## Setting working directory
getwd()
setwd("~/blog")

## Setting up Quandl authcode
#  authcode <- ...

## Pulling data from Quandl for price...
# gasPriceData <- Quandl("DOE/EMM_EPMRR_PTE_NUS_DPG", authcode = authcode)
#  ... and setting date
# datetime <- dmy_hms(powerData$Date, powerData$Time)

gasPriceData <- read.csv(paste0(getwd(), "/", "gasprice.csv"))
date <- mdy(gasPriceData$Date)
date2 <- as.Date(date)
gasPriceData$Date <- date2
rm("date", "date2")

## Exploratory graph for price
png("blog1plot1.png", bg = "white", width = 720, height = 480, unit = "px")
with(gasPriceData, plot(Date, Value,
                  type = "l",
                  ylab = "Dollar Per Gallon",
                  main = "Weekly U.S. Retail Gasoline Prices",
                  sub = "All Grades, All Formulations"))
dev.off()

## Pulling data from Quandle for quantity... 
gasQuantData <- Quandl("EIA/PET_WGTSTUS1_W", authcode = authcode)

png("blog1plot2.png", bg = "white", width = 720, height = 480, unit = "px")
range <- range(gasQuantData$Value)
with(gasQuantData, plot(Date, Value,
                        type = "l",
                        ylab = "Quantity, in thousands of barrels",
                        main = "U.S. Ending Stocks of Total Gasoline, Weekly",
                        ylim = range))
dev.off()

png("blog1plot3.png", width = 720, height = 480, unit = "px")
with(gasQuantData, qplot(Date, Value)
     + geom_line() + geom_smooth()
     + xlab("Date") + ylab("Quantity, in thousands of barrels")
     + ggtitle("U.S. Ending Stocks of Total Gasoline, Weekly"))
dev.off()
