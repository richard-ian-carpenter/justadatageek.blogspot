# Voting Data Blog
# Author: Richard I. Carpenter
# Date Created: 27 SEP 2016
# Date Updated: 01 OCT 2016

# Presidential Election data for voting population and participation, as well
# as U.S. population estimates.  All data from the U.S. Census Bureau website:
# 
# 1. All population estimates are in milliions
# 2. For population estimates 2010 to present: 
#       http://www.census.gov/popest/data/national/totals/2015/index.html
# 3. For population estimate 2000 to 2010: 
#       http://www.census.gov/popest/data/intercensal/national/nat2010.html
# 4. For population estimates prior to 2000: 
#       http://www.census.gov/popest/data/national/totals/pre-1980/tables/popclockest.txt

library(ggplot2)
library(dplyr)

setwd("/home/rich/blog/bp12")

df <- read.csv("voting_stats.csv")
str(df)
summary(df)

# Uncomment png() and dev.off() to create a graph, saved to working
# directory.

png(paste0(getwd(), "/", "blog12plot1.png"), height = 480, width = 720, unit = "px")
p1 <- ggplot(data = df, aes(x = year, y = total_population))
p1 + geom_line() +
    geom_line(aes(y = total_voting_age), colour = "red", linetype = 6) +
    geom_line(aes(y = total_voted), colour = "blue", linetype = 5) +
    xlab("Year") +
    ylab("Population, in millions") + 
    ggtitle("Population & Voting Data, 1964 through 2012")
dev.off()

# Removing percentages...
sorted.df <- df[-c(4, 6, 7)]
sorted.df$voter_pct <- round((sorted.df$total_voted / sorted.df$total_population)*100, 2)
sorted.df$pop_pct <- round((sorted.df$total_voting_age / sorted.df$total_population)*100, 2)
sorted.df$participation <- round((sorted.df$total_voted / sorted.df$total_voting_age) * 100, 2)

sorted.df

png(paste0(getwd(), "/", "blog12plot2.png"), height = 480, width = 720, unit = "px")
p2 <- ggplot(data = sorted.df, aes(x = year, y = pop_pct))
p2 + geom_line() +
    geom_line(aes(y = sorted.df$voter_pct), colour = "red", linetype = 4) +
    xlab("Year") + 
    ylab("Percent of Total Population") +
    ggtitle("Percent of Total Population, 1964 through 2012") +
    ylim(0, 100)
dev.off()

png(paste0(getwd(), "/", "blog12plot3.png"), height = 480, width = 720, unit = "px")
p3 <- ggplot(data = sorted.df, aes(x = year, y = participation))
p3 + geom_line() + 
    xlab("Year") +
    ylab("Participation Rate") +
    ggtitle("Voter Participation Rate, 1964 through 2012") +
    ylim(0, 100)
dev.off()
    

