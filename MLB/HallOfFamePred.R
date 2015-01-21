require(ggplot2)
require(randomForest)
require(tree)
require(XML)
require(plyr)
require(stringr)

batting <- data.frame(read.csv("Batting.csv", header=TRUE))
hof <- data.frame(read.csv("HallOfFame.csv", header=TRUE))
summary(hof)

batting$stint <- NULL
batting$lgID <- NULL
batting$teamID <- NULL
batting$yearID <- NULL

batting[is.na(batting)] <- 0



battingagg <- aggregate(.~playerID, batting, sum)
HallOfFame <-  merge(x=battingagg,y=hof,by=c("playerID"))

summary(HallOfFame)

HallOfFame$votedBy <- NULL
HallOfFame$ballots <- NULL
HallOfFame$needed <- NULL
HallOfFame$votes <- NULL
HallOfFame$category <- NULL
HallOfFame$needed_note <- NULL

HallOfFame$inducted <- ifelse(HallOfFame$inducted == "Y", 1, 0)

HallOfFame <- aggregate(.~playerID,HallOfFame, sum)

HallOfFame$inducted <- as.factor(HallOfFame$inducted)

