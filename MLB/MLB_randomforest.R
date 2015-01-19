library(ggplot2)

teams = read.csv("MLBTeams.csv", header = TRUE)

teams$Ghome <- NULL
teams$IPouts <- NULL

names(teams) <- c("year","league", "teamID", "franchiseID", "rank", "games", "wins", "losses","pennant", "worldSeries", "runs", "atBats", "hits","doubles", "triples", "homeruns","walks", "strikeouts", "stolenBases", "runsAgainst","earnedRuns","era", "completeGames", "shutouts","saves", "hitsAllowed", "homerunsAllowed", "walksAllowed", "Ks","errors", "doublePlays", "fieldingPercentage","teamName", "park", "attendance")

# there should be no null values in this data frame
# p.new <- ggplot(teams, aes(x=teams$wins, y=teams$attendance, col=teams$worldSeries)) + geom_point(size=3,alpha=0.8)+ xlab("wins") +  ylab("attendance") + scale_y_continuous(labels = comma)


