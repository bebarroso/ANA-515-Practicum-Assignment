#This firt block of code simply imports the two sheets and creates one dataframe with all the data
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
nfldata1 <- read_excel("McDaniel/Practicum ANA 515P/nfldata.xlsx", "Sheet 1")
nfldata2 <- read_excel("McDaniel/Practicum ANA 515P/nfldata.xlsx", "Sheet 2")
df = rbind(nfldata1, nfldata2)


#Remove all "NA" values and transforms them into NA
df[df == "NA"] <- NA


#reformats date from excel style to a more readable style
df$date = as.Date(df$date, origin = "1899-12-30")

#fixes missing values for column 2 and transforms playoffs NAs to no
df$season = "2020"
df$playoff = replace_na(df$playoff, "no")

#makes NA in scores equal to 0 and all negative values to NA since there should be no negative values in any columns
df$score1 = replace_na(df$score1, "0")
df$score2 = replace_na(df$score2, "0")
df$score1[df$score1 < 0] <- NA
df$score2[df$score2 < 0] <- NA

#My reasoning here is this: I tried to fix all the NA values that could be fixed (ie. playoffs, scores, etc) 
#but intend to drop all NA values in the filtered DF below so this makes it possible to do so.

#this changes the column values of the playoffs from NA to "no" and then drops all rows with NA values that could complicate analysis
filtered = df
filtered <- na.omit(filtered)

#Now the data is clean of any NAs and wrong 0 values and is ready to be visualized.
#I purposefully did not omit any outliers because I havent (at this point in the data cleaning process) figured if there are outliers


#histogram of total points scored distribution
points_scored = as.numeric(filtered$score1) + as.numeric(filtered$score2)
hist(points_scored, col = "blue", main = "Distribution of Points Scored ", xlab = "Total Ponts")

#Then created a frequency graph of all the total points scored using % frequency
frequency <- table(points_scored)
percent_frequency <- prop.table(frequency) * 100

#using a bar plot
barplot(percent_frequency, col = "red", main = "Frequency Graph of Points Scored",
        xlab = "Points Scored", ylab = "Percent Frequency")

#Now a graph comparin the pre and post value of QB1. I am not a football afficcionado but what I want to understand how often QB1's value increases after each match.
plot(filtered$qb1_value_pre, type = "l", col = "blue", lwd = 2,
     main = "Comparison of qb1_value_pre and qb1_value_post",
     xlab = "Observation", ylab = "Value")
lines(filtered$qb1_value_post, col = "red", lwd = 2)
legend("bottom", legend = c("qb1_value_pre", "qb1_value_post"),
       col = c("blue", "red"), lwd = 2, bty = "n", ncol = 2)

             
#Finally saving the new dataset to be uploaded
write.csv(filtered, file = "NFL Data Cleaned.csv", row.names = FALSE)
