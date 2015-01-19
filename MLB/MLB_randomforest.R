library(ggplot2)
library(randomForest)
library(trees)

teams = read.csv("MLBTeams.csv", header = TRUE)

attach(teams)

names(teams) <- c("year","league", "franchiseID", "rank", "games", "wins", "losses","pennant", "worldSeries", "runs", "atBats", "hits","doubles", "triples", "homeruns","walks", "strikeouts", "stolenBases", "runsAgainst","earnedRuns","era","saves", "hitsAllowed", "homerunsAllowed", "walksAllowed", "Ks","errors", "doublePlays", "fieldingPercentage", "attendance")

rank <- NULL
winningPCT <- teams$wins/teams$games
wins <- NULL
losses <- NULL
franchiseID <- as.character(franchiseID)
year <- as.character(year)
franchiseID <- do.call(paste,c(teams[c("franchiseID","year")]))
league <- NULL
year <- NULL
# there should be no null values in this data frame
# p.new <- ggplot(teams, aes(x=teams$wins, y=teams$attendance, col=teams$worldSeries)) + geom_point(size=3,alpha=0.8)+ xlab("wins") +  ylab("attendance") + scale_y_continuous(labels = comma)


