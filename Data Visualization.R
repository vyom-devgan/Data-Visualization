#' ---
#' title: "Data Visualization in R"
#' author: "Vyom Devgan"
#' date: "Mach 16, 2023"
#' ---

library(RCurl)
library(plyr)
library(ggplot2)
library(randomcoloR)
library(lubridate)

dataUrl <- getURL("https://raw.githubusercontent.com/vyom-devgan/Data-Visualization/main/all_games.csv")
games <- read.csv(text = dataUrl)
games <- games[,-4]

games2 <- subset(games, user_review != "tbd")
games2 <- transform(games2, user_review = as.numeric(user_review))

top100 <- games2[1:100,]
bot100 <- tail(games2, n = 100)


ggplot(games2, aes(x = factor(meta_score), y = factor(user_review))) + 
  geom_point(size=2) + ggtitle("All rated games by Metacritic") + xlab("Metacritic Score") + ylab("User Score")

#' ---

ggplot(top100, aes(x = factor(meta_score), y = factor(user_review))) + 
  geom_point(size=2) + ggtitle("100 Highest rated games by Metacritic") + xlab("Metacritic Score") + ylab("User Score")

#' ---

ggplot(bot100, aes(x = factor(meta_score), y = factor(user_review))) + 
  geom_point(size=2) + ggtitle("100 Lowest rated games by Metacritic") + xlab("Metacritic Score") + ylab("User Score")

counts <- count(games2, 'platform')

#' ---

palette <- distinctColorPalette(23)
pie(counts$freq, labels = counts$platform, main = "Platform Distribution", col = palette)

games2$release_date <- year(mdy(games2$release_date))

#' ---

xx <- table(games2$release_date)
barplot(xx, main = "Release Year vs Game Released", xlab = "Year of Release", border = "#32cd32", col = "blue")
