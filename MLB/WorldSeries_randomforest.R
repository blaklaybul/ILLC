require(ggplot2)
require(randomForest)
require(tree)

teams = read.csv("MLBTeams.csv", header = TRUE)

names(teams) <- c("year","league", "franchiseID", "rank", "games", "wins", "losses","pennant", "worldSeries", "runs", "atBats", "hits","doubles", "triples", "homeruns","walks", "strikeouts", "stolenBases", "runsAgainst","earnedRuns","era","saves", "hitsAllowed", "homerunsAllowed", "walksAllowed", "Ks","errors", "doublePlays", "fieldingPercentage", "attendance")

teams$franchiseID <- as.character(teams$franchiseID)
teams$year <- as.character(teams$year)
teams$franchiseID <- do.call(paste,c(teams[c("franchiseID","year")]))
teams$winningPCT = teams$wins/teams$games
teams$rank <- NULL
teams$league <- NULL
teams$year <- NULL
teams$wins <- NULL
teams$losses <- NULL
teams$attendance <- teams$attendance/teams$games

# there should be no null values in this data frame
# p.new <- ggplot(teams, aes(x=teams$wins, y=teams$attendance, col=teams$worldSeries)) + geom_point(size=3,alpha=0.8)+ xlab("wins") +  ylab("attendance") + scale_y_continuous(labels = comma)

rf.teams=randomForest(attendance~.-franchiseID, data=teams, subset=train)

rf.teams=randomForest(attendance~.-franchiseID-worldSeries, data=teams, subset=train)

for(mtry in 1:23){
  fit=randomForest(attendance~.-franchiseID-worldSeries, data=teams, subset=train, mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred = predict(fit,teams[-train,])
  test.err[mtry]=with(teams[-train,],mean((attendance-pred)^2))
  cat(mtry," ")                      
}

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b", ylab="mean sq error")
