## Blog: Looking at Greece
#
## Author: Richard I. Carpenter
#  Date Created: 09 Jul 2015
#  Date Updated: 11 Aug 2015
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

## Exploring data on Greece...
#  ... first, historical look at 10-year bond rate...
df1 <- Quandl("YC/GRC10Y", authcode = authcode) # from Quandl
with(df1, plot(Date, Rate, type = "l"))
#  ... making a nicer plot...
png("blog2plot1.png", width = 720, height = 480, unit = "px") # Opening plot device...
plot1 <- ggplot(df1, aes(x = Date, y = Rate))
plot1 + geom_line(aes(colour = Rate)) +
      scale_colour_gradient(high = "red") + # Creates gradient for rate
      ylab("Rate, in percent") +
      ggtitle("Greek 10-year Bond Rates (1998 to 2015)")
dev.off() # ... and closing plot device.

#  ... now zooming in to last 5 years of data...
df2 <- Quandl("YC/GRC10Y", authcode = authcode, trim_start="2010-06-25")
with(df2, plot(Date, Rate, type = "l"))
#  ... making another nicer plot...
png("blog2plot2.png", width = 720, height = 480, unit = "px")
plot2 <- ggplot(df2, aes(x = Date, y = Rate))
plot2 + geom_line(aes(colour = Rate)) +
      scale_colour_gradient(high = "red") + # Creates gradient for rate
      ylab("Rate, in percent") +
      ggtitle("Greek 10-year Bond Rates (June 25, 2010 to June 25, 2015)")
dev.off()

## Pulling in additional data to bring data current...
#  ... this data comes from www.investing.com
df3 <- read.csv(paste0(getwd(), "/", "grc10yrbond.csv"))

## Converting date from "factor" to "Date"
#  ... converting date to character class...
date1 <- as.character(df3$Date)
#  ... splitting the month and year
month <- sapply(date1, FUN = function(x) {strsplit(x, split = "-") [[1]][2]})
day <- sapply(date1, FUN = function(x) {strsplit(x, split = "-") [[1]][3]})
year <- sapply(date1, FUN = function(x) {strsplit(x, split = "-")[[1]][1]})
#  ... joining "month" and "year" and updating "date1"...
date1 <- paste0(year, "-", month, "-", day)
class(date1) # [1] "character"
#  ... loading lubridate to help with the conversion to "Date"...
date2 <- ymd(date1)
#  ... converting from as.POSIXct to as.Date...
date2 <- as.Date(date2)
class(date2) # [1] "Date"
#  ... replacing original date
df3$Date <- date2
class(df3$Date) # [1] "Date"
#  ... date conversion completed!
rm("date1", "date2", "month", "year", "day") # Clean up!

## Creating a plot of the Investing.com data...
#  ... first a basic plot...
with(df3, plot(Date, Rate, type = "l"))
#  ... plot looks good, now in ggplot2 for .png
png("blog2plot3.png", width = 720, height = 480, unit = "px") 
plot3 <- ggplot(df3, aes(x = Date, y = Rate))
plot3 + geom_line(aes(colour = Rate)) +
      scale_colour_gradient(high = "red") +
      ylab("Rate, in percent") +
      ggtitle("Greek 10-year Bond Rates (June 25, 2015 to July 10, 2015)")
dev.off()

## Combining data sets...
df4 <- rbind(df2, df3)
attach(df4)
df4Sorted <- df4[order(Date), ]
detach(df4)

#  ... creating a basic plot...
with(df4Sorted, plot(Date, Rate, type = "l"))
#  ... and, finally, a nicer plot in ggplot2.
png("blog2plot4.png", width = 720, height = 480, unit = "px") 
plot4 <- ggplot(df4Sorted, aes(x = Date, y = Rate))
plot4 + geom_line(aes(colour = Rate)) +
      scale_colour_gradient(high = "red") +
      ylab("Rate, in percent") +
      ggtitle("Greek 10-year Bond Rates (June 25, 2010 to July 10, 2015)")
dev.off()
