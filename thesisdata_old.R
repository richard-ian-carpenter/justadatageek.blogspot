## R Script for MSAE Thesis
## Author: Richard Ian Carpenter
## Date Created: 14 Jan 2015
## Date Updated: 04 Nov 2015

## This is the data that I have collected, sorted, and cleaned via spreadsheet.
#  The data can be found via the Energy Information Agency's (EIA) website at:
#  http://www.eia.gov/petroleum/data.cfm

## NOTE: Comments for the beginning of a step in my analysis will have a
#        double #.  Sub-steps within my analysis will have a single #.
#
#        Also, at various points I use the head(), tail(), class(), etc. functions
#        in order to check/verify the data and my work. I omit them here for 
#        clarity of the R script.

## Setting working directory and reading the data...
getwd()
setwd("~/Documents/Thesis_Research_Data")
thesisData <- read.table(paste0(getwd(), "/", "thesisdata_old.csv"), 
                  header = TRUE, 
                  sep = ",")
#  ... and reading in the variable name & description .csv file.
varNames <- read.table(paste0(getwd(), "/", "variables_old.csv"),
                  header = TRUE,
                  sep = ",")

## Converting date from "factor" to "Date"
#  ... converting date to character class...
date1 <- as.character(thesisData$date)
#  ... splitting the month and year
month <- sapply(date1, FUN = function(x) {strsplit(x, split = "-") [[1]][1]})
year <- sapply(date1, FUN = function(x) {strsplit(x, split = "-")[[1]][2]})
#  ... adding "-01" to the month...
month <- paste0(month, "-01")
#  ... joining "month" and "year" and updating "date1"...
date1 <- paste0(month, "-", year)
class(date1) # [1] "character"
#  ... loading lubridate to help with the conversion to "Date"...
library(lubridate)
date2 <- mdy(date1)
#  ... converting from as.POSIXct to as.Date...
date2 <- as.Date(date2)
class(date2) # [1] "Date"
#  ... replacing original date
thesisData$date <- date2
class(thesisData$date) # [1] "Date"
#  ... date conversion completed!
rm("date1", "date2", "month", "year") # Clean up!

## For publishing functionality, install the following package:
#  install.packages("knitr")
library(knitr)

## For graphing functionality beyond the base plot system of R, install the
#  following packages:
#  install.packages("ggplot2")
library(ggplot2)

## For time series functionality, install the following packages:
#  install.packages("forecast")
library(forecast)
#  install.packages("vars")
library(vars)
#  install.packages("tseries")
library(tseries)
#  install.packages("lmtest")
library(lmtest)

## For manipulating functionality, install the following packages:
#  install.packages("dplyr")
library(dplyr)
#  install.packages("reshape2")
library(reshape2)

####################
#                  #
#  Blog, Part One  #
#                  #
####################

## Exploratory Data Analysis
#  ... table dimensions: rows, columns...
dim(thesisData) 
#  ... summary stats on all variables...
summary(thesisData) 
#  ... and a set of plots:
#  WTI Crude
pWTI <- ggplot(thesisData, aes(x = date, y = wti))
pWTI + geom_line() + ggtitle("WTI Crude Prices, monthly") + 
      xlab("Date") + 
      ylab("Price, in dollars")
#  Conventional Gas, New York
pGasNY <- ggplot(thesisData, aes(x = date, y = convgas_ny))
pGasNY + geom_line() + ggtitle("Conventional Gas Prices, New York, monthly") +
      xlab("Date") +
      ylab("Price, in dollars")
#  Conventional Gas, Gulf
pGasGulf <- ggplot(thesisData, aes(x = date, y = convgas_gulf))
pGasGulf + geom_line() + ggtitle("Conventional Gas Prices, Gulf, monthly") +
      xlab("Date") +
      ylab("Price, in dollars")
#  Reformulated Gas, Los Angeles
pGasLA <- ggplot(thesisData, aes(x = date, y = rbob_la))
pGasLA + geom_line() + ggtitle("Reformulated Gas Prices, Los Angeles, monthly") +
      xlab("Date") +
      ylab("Prices, in dollars")

## Subsetting the data set to focus on prices:
sub1 <- subset(thesisData, select = c(1:12))
#  I am dividing the wti variablle in order to create a per-gallon equivalent. This
#  will allow for equal scaling of prices.
#  The formula is wti <- (wti / 42), 42 being the number of gallons per barrel.
sub1$wti <- (sub1$wti / 42)
#  ... and doing the same for brent prices.
sub1$brent <- (sub1$brent / 42)
#  ... summary stats of price data.
summary(sub1)
#  ... generating an average of the conventional gas prices.
sub1$avgConvGas <- (sub1$convgas_ny + sub1$convgas_gulf) / 2
attach(sub1)
## Quick plot of the transformed prices:
#  Conventional Gas (NY), Brent, and WTI Crude prices:
# Uncomment the png() and dev.off() lines below to create a .png graph, 
# otherwise it will appear on screen.
# png(filename = "blog5plot1.png", height = 480, width = 720, unit = "px")
with(sub1, 
     plot(date, wti, 
          type = "l", 
          col = "blue", 
          ylab = "Crude Oil Price, $/gal"))
with(sub1, 
     lines(date, brent, 
           type = "l", 
           col = "red"))
with(sub1,
     lines(date, avgConvGas,
           type = "l",
           col = "green"))
legend("bottomright", 
       c("wti", "brent", "avgConvGas"), 
       lty = c(1, 1, 1), 
       col = c("blue", "red", "green"))
# Uncomment dev.off() below if you uncommented the png() above!
# dev.off() # Closes device
## Correlation
cor(sub1[, c(2, 3, 13)])
detach(sub1)

####################
#                  #
#  Blog, Part Two  #
#                  #
####################

attach(sub1)
## Some exploratory analysis
summary(sub1[, c(2, 3, 13)])

## Create a PNG file by uncommenting the code around the boxplot.
#  png(filename = "blog6plot1.png", height = 480, width = 720, unit = "px")
boxplot(wti, brent,
        names = c("WTI", "Brent"),
        xlab = ("Crude Oil Type"),
        ylab = ("Price, Dollars per Gallon"),
        main = ("Comparison of WTI & Brent Prices"))
#  dev.off()

## Are the mean prices statistically significantly different?
#  Using mean WTI and Brent prices:
nWTI <- as.numeric(length(wti))
nBrent <- as.numeric(length(brent))
meanWTI <- mean(wti)
meanBrent <- mean(brent)
#  ... and the variance...
varWTI <- var(wti)
varBrent <- var(brent)
#  ... and the standard deviation...
sdWTI <- sd(wti)
sdBrent <- sd(brent)
#  ... calculating the degrees of freedom for unequal variances...
num <- ((mean(wti)^2 / nWTI) + (mean(brent)^2 / nBrent))^2
denom <- (((mean(wti)^2 / nWTI)^2) / (nWTI - 1)) + (((mean(brent)^2 / nBrent)^2) / (nBrent - 1))
compDF <- (num / denom)
#  ... and the t-stat for 5% C.I., using compDF...
t <- round(qt(0.975, compDF), 3)
#  ... and the standard error...
se <- ((sdWTI^2 / nWTI) + (sdBrent^2 / nBrent))^0.5
#  Calculating confidence interval for hypothesis test:
#  H_0: meanBrent - meanWTI = 0
#  H_a: meanBrent - meanWTI != 0
compTest <- meanBrent - meanWTI + c(-1, 1) * t * se
#  Calculating comparative t statistic...
compT <- ((meanBrent - meanWTI) / se)
#  ... and it's p-value
compP <- pt(compT, compDF - 1, lower.tail = FALSE)
#  Displaying t-stats and p-value
infStats <- as.data.frame(cbind(t, compT, compP))
names(infStats) <- c("t-stat, 189.208df", "comparative t-stat", "comparative p-value")
infStats
#  Displaying the 95% confidence intervals.
names(compTest) <- c("Lower C.I., 95%", "Upper C.I., 95%")
compTest

## Testing for autocorrelation:
#  Autocorrelation:
#  png("blog6plot2.png", width = 720, height = 720, unit = "px")
opar <- par(mfrow = c(2, 3))
acf(wti, type = c("correlation"))
acf(brent, type = c("correlation"))
acf(avgConvGas, type = c("correlation"))
#  Partial autocorrelation:
acf(wti, type = c("partial"))
acf(brent, type = c("partial"))
acf(avgConvGas, type = c("partial"))
par(opar)
#  dev.off()
detach(sub1)

##############################
#                            #
#   Blog, Part Three         #
#                            #
##############################

## Testing for unit root:
attach(sub1)
gasADFtest <- summary(ur.df(avgConvGas, type = "drift", selectlags = "BIC"))
wtiADFtest <- summary(ur.df(wti, type = "drift", selectlags = "BIC"))
brentADFtest <- summary(ur.df(brent, type = "drift", selectlags = "BIC"))

#  Plotting data again, using ggplot2:
png("blog7plot1.png", 
    height = 360,
    width = 720,
    unit = "px")
plotGas <- ggplot(sub1, aes(x = date, y = avgConvGas))
plotGas + geom_line() + geom_smooth()
dev.off()

png("blog7plot2.png", 
    height = 360,
    width = 720,
    unit = "px")
plotWTI <- ggplot(sub1, aes(x = date, y = wti))
plotWTI + geom_line() + geom_smooth()
dev.off()

png("blog7plot3.png", 
    height = 360,
    width = 720,
    unit = "px")
plotBrent <- ggplot(sub1, aes(x = date, y = brent))
plotBrent + geom_line() + geom_smooth()
dev.off()

#  They will be in your working directory, or comment out the png() and dev.off()
#  functions to view them on your screen.

## Creating the differenced variables
diffWTI <- diff(wti)
diffBrent <- diff(brent)
diffGas <- diff(avgConvGas)

png("blog7plot4.png", 
    height = 360,
    width = 720,
    unit = "px")
plot(diffGas, type = "l", 
     xlab = "Number of Observations",
     ylab = "Gas Price",
     main = "First Difference of Average U.S. Gasoline Price")
abline(h = mean(diffGas), col = "red")
dev.off()

png("blog7plot5.png", 
    height = 360,
    width = 720,
    unit = "px")
plot(diffWTI, type = "l",
     xlab = "Number of Observations",
     ylab = "WTI Crude Price",
     main = "First Difference of WTI Crude Oil Spot Price")
abline(h = mean(diffGas), col = "red")
dev.off()

png("blog7plot6.png", 
    height = 360,
    width = 720,
    unit = "px")
plot(diffBrent, type = "l",
     xlab = "Number of Observations",
     ylab = "Brent Crude Price",
     main = "First Difference of Brent Crude Oil Spot Price")
abline(h = mean(diffGas), col = "red")
dev.off()

##############################
#                            #
#   Blog, Part Four          #
#                            #
##############################

## Building linear models on the differences.
model1 <- summary(lm(diffGas ~ diffWTI))
model2 <- summary(lm(diffGas ~ diffBrent))
model3 <- summary(lm(diffGas ~ diffWTI + diffBrent))

## Performing the Granger causality tests.