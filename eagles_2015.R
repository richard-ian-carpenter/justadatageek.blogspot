# Blog Post #9
# Title: Analyzing the Philadelphia Eagles 2015 Season
#
# Author: Richard Ian Carpenter
# Date Created: 08 Feb 2016
# Date Updated: 15 Mar 2016
#
# Data source: http://www.pro-football-reference.com/teams/phi/2015.htm
#

getwd()
setwd("~/blog/bp9")

library(ggplot2)
library(dplyr)

# Reading and transforming the data.
df1 <- read.csv("eagles_2015.csv", skip = 2)
dim(df1) # [1] 17 24
summary(df1)  # There are NA values...

# Cleaning up the column names...
# Named the columns for boxscore hyperlink and the projected scores "remove#"
# to facilitate removing them from my analysis.
colnames(df1) <- c("week", "day", "date", "remove1", "win_loss", "overtime", "record",
                  "home_away", "opponent", "points_for", "points_against", 
                  "off_1st_down", "off_tot_yds", "off_pass_yds", "off_rush_yds", 
                  "off_turnovers", "def_1st_down", "def_tot_yds", "def_pass_yds", 
                  "def_rush_yds", "def_turnovers", "remove2", "remove3", "remove4")
names(df1) # Checking column names...
df2 <- filter(df1, opponent != "Bye Week")
df2 <- select(df2, -remove1, -remove2, -remove3, -remove4) # Removes unwanted columns
names(df2)
dim(df2) # [1] 16 20

# Removing NAs from fields **not** in the Bye Week...
# This occurs in the second part of the Eagles' season.
df2[1:16, 9:20][is.na(df2[1:16, 9:20])] <- 0
# summary(df2)
# str(df2)

# Transforming Home & Away from '@' and blank to '1' and '0'
# Home = '1', Away = '0'
df2$home_away <- gsub("@", "1", df2$home_away)
df2$home_away <- gsub("", "0", df2$home_away)
df2$home_away # '1' are now showing as '010'
df2$home_away <- gsub("010", "1", df2$home_away) # Fixes problem above
df2$home_away <- as.factor(df2$home_away)
df2$home_away
# str(df2) # Checking data

# Tranforming Overtime to '1' for overtime and '0' otherwise
df2$overtime <- gsub("OT", "1", df2$overtime)
df2$overtime <- gsub("", "0", df2$overtime)
# This fixes the same problem that occurred in the Home & Away variable
df2$overtime <- gsub("010", "1", df2$overtime) 
df2$overtime <- as.factor(df2$overtime)
df2$overtime

# Progress check...
str(df2)
summary(df2)

# Some ggplot2 work...
# Bar charts showing weekly points for and points against, along with their 
# respective season average:
# NOTE: To create .png files of plots, remove the "#" before the png() and 
# dev.off() lines to uncomment them.

#png("blog9plot1.png", width = 720, height = 480)
p1 <- ggplot(data = df2, aes(x = week, y = points_for))
p1 + geom_bar(stat = "identity", na.rm = TRUE, aes(fill = win_loss), 
              position = "dodge") + 
    geom_hline(aes(yintercept = mean(df2$points_for, 
                                     na.rm = TRUE))) +
    geom_text(aes(0, mean(df2$points_for), 
                  label = round(mean(df2$points_for), 1),
                  vjust = -0.5,
                  hjust = 0.8)) +
    theme(panel.grid.major = element_line("gray"), 
          panel.grid.minor = element_line("lightgray"),
          panel.background = element_rect("white")) +
    ggtitle("Phila. Eagles 2015 Season") +
    ylab("Points For") +
    xlab("Week")
#dev.off()

#png("blog9plot2.png", width = 720, height = 480)
p2 <- ggplot(data = df2, aes(x = week, y = points_against))
p2 + geom_bar(stat = "identity", na.rm = TRUE, aes(fill = win_loss), 
              position = "dodge") + 
    geom_hline(aes(yintercept = mean(df2$points_against, 
                                     na.rm = TRUE))) +
    geom_text(aes(0, mean(df2$points_against), 
                  label = round(mean(df2$points_against), 1),
                  vjust = 1.3,
                  hjust = 0.8)) +
    theme(panel.grid.major = element_line("gray"), 
          panel.grid.minor = element_line("lightgray"),
          panel.background = element_rect("white")) +
    ggtitle("Phila. Eagles 2015 Season") +
    ylab("Points Against") +
    xlab("Week")
#dev.off()

# Scatter plot with linear regression plotted, along with season averages for 
# yards gained (offense) or allowed (defense) and points for (offense) or 
# against (defense).

# To get W and L (win and loss, repsectively), do not include the geom_point()
# option.  Instead, use geom_text() and set up aes() properly.

#png("blog9plot3.png", width = 720, height = 480)
p3 <- ggplot(data = df2, aes(x = points_for, y = off_tot_yds))
p3 + geom_smooth(method = "lm", se = FALSE, colour = "darkgray", size = 0.5) + 
    geom_hline(aes(yintercept = mean(df2$off_tot_yds, 
                                     na.rm = TRUE))) +
    geom_vline(aes(xintercept = mean(df2$points_for,
                                     na.rm = TRUE))) +
    theme(panel.grid.major = element_line("gray"), 
          panel.grid.minor = element_line("lightgray"),
          panel.background = element_rect("white"))+
    geom_text(aes(label = win_loss, colour = win_loss), position = "jitter")+
    ggtitle("Phila. Eagles 2015 Season") + 
    ylab("Total Yards Gained (Off.)") + 
    xlab("Points For")
#dev.off()

#png("blog9plot4.png", width = 720, height = 480)
p4 <- ggplot(data = df2, aes(x = points_against, y = def_tot_yds))
p4 + geom_smooth(method = "lm", se = FALSE, colour = "darkgray", size = 0.5) + 
    geom_hline(aes(yintercept = mean(df2$def_tot_yds, 
                                     na.rm = TRUE))) +
    geom_vline(aes(xintercept = mean(df2$points_against,
                                     na.rm = TRUE))) +
    theme(panel.grid.major = element_line("gray"), 
          panel.grid.minor = element_line("lightgray"),
          panel.background = element_rect("white")) +
    geom_text(aes(label = win_loss, colour = win_loss), position = "jitter") +
    ggtitle("Phila. Eagles 2015 Season") + 
    ylab("Total Yards Allowed (Def.)") + 
    xlab("Points Against")
#dev.off()

# Scatterplot with wins and losses colored green and red, respectively
#png("blog9plot5.png", width = 720, height = 480)
p5 <- ggplot(data = df2, aes(x = week))
p5 + geom_point(y = df2$points_for, colour = "green") +
        geom_point(y = df2$points_against, colour = "red") +
        scale_y_continuous("Points", limits = c(0, 50))+
        theme(panel.grid.major = element_line("gray"), 
            panel.grid.minor = element_line("lightgray"),
            panel.background = element_rect("white")) +
        ggtitle("Phila. Eagles 2015 Season") 
#dev.off()

