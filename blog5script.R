## Blog Entry #5
#  Commodity Prices
#  Author: Richard I. Carpenter
#  Date Created: 5 Sep 2015
#  Date Updated: 5 Sep 2015

## Setting up working directory...
getwd()
setwd("~/blog/github")
#  ... and some initial packages.
library(Quandl)
library(ggplot2)
library(lubridate)
library(dplyr)

## Quandl authcode.  Sign up for a free account to get your own!
authcode <- "qRga9SqGnuPQvvgDyC_z"

## Industrial metal prices from the London Metal Exchange (LME)...
#  ... all prices from Quandl.com...
#  ... Steel Billet Prices
steel <- Quandl("LME/PR_FM", 
                authcode = authcode, 
                trim_start = "2015-01-02",
                trim_end = "2015-09-04")
#  ... Molybdenum Prices
#  molybdenum <- Quandl("LME/PR_MO", authcode = authcode)
#  ... Cobalt Prices
#  cobalt <- Quandl("LME/PR_CO", authcode = authcode)
#  ... Copper Prices
copper <- Quandl("LME/PR_CU", 
                 authcode = authcode,
                 trim_start = "2015-01-02",
                 trim_end = "2015-09-04")
#  ... Lead Prices
#  lead <- Quandl("LME/PR_PB", authcode = authcode)
#  ... Aluminum Alloy Prices
#  alumalloy <- Quandl("LME/PR_AA", authcode = authcode)
#  ... Nickel Prices
#  nickel <- Quandl("LME/PR_NI", authcode = authcode)
#  ... Aluminum Prices
aluminum <- Quandl("LME/PR_AL", 
                   authcode = authcode,
                   trim_start = "2015-01-02",
                   trim_end = "2015-09-04")
#  ... Zinc Prices
#  zinc <- Quandl("LME/PR_ZI", authcode = authcode)
#  ... Tin Prices
#  tin <- Quandl("LME/PR_TN", authcode = authcode)

## Renaming variables to be more R "friendly".
#  Check the variable names using the names() function & then remove the spaces...
#  ... to make them R "friendly".
#  colnames(alumalloy) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller", "dec1buyer", "dec1seller", "dec2buyer", "dec2seller", "dec3buyer", "dec3seller")
colnames(aluminum) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller", "dec1buyer", "dec1seller", "dec2buyer", "dec2seller", "dec3buyer", "dec3seller")
colnames(copper) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller", "dec1buyer", "dec1seller", "dec2buyer", "dec2seller", "dec3buyer", "dec3seller")
#  colnames(lead) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller", "dec1buyer", "dec1seller", "dec2buyer", "dec2seller", "dec3buyer", "dec3seller")
#  colnames(nickel) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller", "dec1buyer", "dec1seller", "dec2buyer", "dec2seller", "dec3buyer", "dec3seller")
#  colnames(tin) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller", "dec1buyer", "dec1seller", "dec2buyer", "dec2seller", "dec3buyer", "dec3seller")
#  colnames(zinc) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller", "dec1buyer", "dec1seller", "dec2buyer", "dec2seller", "dec3buyer", "dec3seller")
#  colnames(cobalt) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller")
#  colnames(molybdenum) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller")
colnames(steel) <- c("date", "cashBuyer", "cashSeller", "3moBuyer", "3moSeller", "15moBuyer", "15moSeller")

## Industrial metal stocks from the London Metal Exchange (LME)...
#  ... all quantities from Quandl.com...
#  ... Steel Billet Stocks, All Locations...
steelStocks <- Quandl("LME/ST_SB_ALL", 
                      authcode = authcode,
                      trim_start = "2015-01-02",
                      trim_end = "2015-09-04")
#  ... Copper Stocks, All Locations...
copperStocks <- Quandl("LME/ST_CU_ALL", 
                       authcode = authcode,
                       trim_start = "2015-01-02",
                       trim_end = "2015-09-04")
#  ... Aluminum Stocks, All Locations...
aluminumStocks <- Quandl("LME/ST_PA_ALL", 
                         authcode = authcode,
                         trim_start = "2015-01-02",
                         trim_end = "2015-09-04")

## Adjusting stock variable names to be R "friendly". Use names() function first!
colnames(steelStocks) <- c("date", "openStock", "delIn", "delOut", "closingStock", "openTon", "cancTon")
colnames(copperStocks) <- c("date", "openStock", "delIn", "delOut", "closingStock", "openTon", "cancTon")
colnames(aluminumStocks) <- c("date", "openStock", "delIn", "delOut", "closingStock", "openTon", "cancTon")

## Creating subsets using dplyr() function.
subAlumPrice <- select(aluminum, c(1, 3))
subSteelPrice <- select(steel, c(1, 3))
subCopperPrice <- select(copper, c(1, 3))
subAlumStocks <- select(aluminumStocks, c(1, 5))
subSteelStocks <- select(steelStocks, c(1, 5))
subCopperStocks <- select(copperStocks, c(1, 5))
#  ... you can now clean up the unnecessary datasets.
rm("aluminum", "aluminumStocks", "copper", "copperStocks", "steel", "steelStocks")

## Now to change column names to allow for merging of price and stock datasets...
colnames(subAlumPrice) <- c("date", "alumCashSeller")
colnames(subCopperPrice) <- c("date", "copperCashSeller")
colnames(subSteelPrice) <- c("date", "steelCashSeller")
colnames(subAlumStocks) <- c("date", "alumClosingStock")
colnames(subCopperStocks) <- c("date", "copperClosingStock")
colnames(subSteelStocks) <- c("date", "steelClosingStock")
#  ... and to merge the three price datasets into one...
df1 <- full_join(subAlumPrice, subCopperPrice)
df2 <- full_join(df1, subSteelPrice)
allPrices <- df2
#  ... and, lastly, to merge the three stocks datasets into one.
df3 <- full_join(subAlumStocks, subCopperStocks)
df4 <- full_join(df3, subSteelStocks)
allStocks <- df4
#  Cleaning up...
rm("df1", "df2", "df3", "df4")
