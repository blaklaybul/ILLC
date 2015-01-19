teams = read.csv("MLBTeams.csv", header = TRUE)

teams$Ghome <- NULL
teams$IPouts <- NULL

names(teams) <- c("year","league", "teamID", "franchiseID", "rank", "games", "wins", "losses","pennant", "worldSeries", "runs", "atBats", "hits","doubles", "triples", "homeruns","walks", "strikeouts", "stolenBases", "runsAgainst","earnedRuns","era", "completeGames", "shutouts","saves", "hitsAllowed", "homerunsAllowed", "walksAllowed", "Ks","errors", "doublePlays", "fieldingPercentage","teamName", "park", "attendance")

# there should be no null values in this data fram

