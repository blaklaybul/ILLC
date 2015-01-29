require(Lahman)
require(mosaic)
require(randomForest)
require(dplyr)
require(rpart)
require(maptree)
require(ggplot2)
require(tree)


inductees <-
  HallOfFame %>%
  group_by(playerID) %>%
  filter(votedBy %in% c("BBWAA", "Special Election") & category == "Player") %>%
  summarise(yearsOnBallot = n(), inducted = sum(inducted == "Y"), best = max(votes/ballots)) %>%
  arrange(desc(best))

batting <-
  Batting %>%
  group_by(playerID) %>%
  summarise(numSeasons = length(unique(yearID)), lastSeason = max(yearID), tAB = sum(AB), tH = sum(H), tHR = sum(HR), tR = sum(R), tSB = sum(SB), tRBI = sum(RBI), tBA = round(sum(H)/sum(AB), 3)) %>%
  arrange(desc(tH))

pitching <-
  Pitching %>%
  group_by(playerID) %>%
  summarise(numSeasons = length(unique(yearID)), lastSeason = max(yearID), tIP = round(sum(IPouts)/3, 2), tW = sum(W), tSO = sum(SO), tSV = sum(SV), tERA = round((9*sum(ER))/(sum(IPouts)/3), 3), tWHIP = round((sum(BB)+sum(H))/(sum(IPouts)/3), 3)) %>%
  arrange(desc(tW))

awards <-
  AwardsPlayers %>%
  group_by(playerID) %>%
  summarise(mvp = sum(awardID == "Most Valuable Player"), gg = sum(awardID == "Gold Glove"), cy = sum(awardID == "Cy Young Award"))

allstar <-
  AllstarFull %>%
  group_by(playerID) %>%
  summarise(ASgame = sum(GP))

#merges
  #batting
candidatesBat <- merge(batting, awards, by="playerID", all.x=T)
candidatesBat <- merge(candidatesBat, allstar, by="playerID", all.x=T)
candidatesBat <- merge(candidatesBat, inductees, by="playerID")
candidatesBat$inducted <- as.factor(candidatesBat$inducted)
  
  #pitching
candidatesPitch <- merge(pitching, awards, by="playerID", all.x=T)
candidatesPitch <- merge(candidatesPitch, allstar, by="playerID", all.x=T)
candidatesPitch <- merge(candidatesPitch, inductees, by="playerID")
candidatesPitch$inducted <- as.factor(candidatesPitch$inducted)

#replace n/a's
candidatesBat[is.na(candidatesBat)] <- 0
candidatesPitch[is.na(candidatesPitch)] <- 0

##get player names
Pnames <- Master[,c("playerID", "nameLast", "nameFirst")]
candidatesBat <- merge(candidatesBat, Pnames, by="playerID", all.x=T)
candidatesPitch <- merge(candidatesPitch, Pnames, by="playerID", all.x=T)

ggplot(candidatesBat[candidatesBat$tBA<0.45 & candidatesBat$tH >400 & candidatesBat$lastSeason < 2010 ,], aes(x=tBA, y=tH, col = inducted)) +geom_point(size = 4, alpha = 0.9) + scale_x_continuous(name = "Batting Average")+ scale_y_continuous(name="Hits") + theme(text = element_text(size=35))

ggplot(candidatesPitch[candidatesPitch$lastSeason < 2010,], aes(x=tW, y=tSO, col = inducted)) +geom_point(size = 4, alpha = 0.9) + scale_x_continuous(name = "Wins")+ scale_y_continuous(name="Strikeouts") + theme(text = element_text(size=35))

#split our data into a training and a test set.
set.seed(919)
train=sample(1:nrow(candidatesBat), 650)


#make trees
bat_tree=rpart(inducted~ tAB +tH+tHR+tR+tSB+tRBI+tBA+mvp, data=candidatesBat, method = "class" ,subset=train)
draw.tree(bat_tree, cex = 1.2)
summary(bat_tree)
print(bat_tree)

bat.pred=predict(bat_tree,candidatesBat[-train,],type="class")

with(candidatesBat[-train,],table(bat.pred,inducted))


#tree classifier?

pruned_bat = prune(bat_tree, cp = 0.046)
draw.tree(bat_tree)
plotcp(bat_tree)
printcp(bat_tree)
draw.tree(pruned_bat, cex=1.2)

bat.pred.prune=predict(pruned_bat,candidatesBat[-train,],type="class")
with(candidatesBat[-train,],table(bat.pred.prune,inducted))

##random forest time
rf.candidatesBat <- merge(batting, awards, by="playerID", all.x=T)
rf.candidatesBat <- merge(rf.candidatesBat, allstar, by="playerID", all.x=T)
rf.candidatesBat <- merge(rf.candidatesBat, inductees, by="playerID", all.x=T)
rf.candidatesBat <- merge(rf.candidatesBat, Pnames, by="playerID", all.x=T)

rf.candidatesBat[is.na(rf.candidatesBat)] <- 0

rf.bat.train= subset(rf.candidatesBat, rf.candidatesBat$numSeasons >= 10 & rf.candidatesBat$lastSeason < 2009 & rf.candidatesBat$lastSeason > 1949 & rf.candidatesBat$tAB > 2500)
rf.bat.test= subset(rf.candidatesBat, rf.candidatesBat$lastSeason >= 2009 & rf.candidatesBat$tAB > 500)

set.seed(919)
par(mfrow=c(1,1))
mtry.bat <- tuneRF(rf.bat.train[,c("tH", "tHR", "tR", "tSB", "tRBI", "tBA", "mvp", "gg", "ASgame")], factor(rf.bat.train[,c("inducted")]), ntreeTry=1000, data=rf.bat.train, plot=T)
mtry.bat

rf.bat <- randomForest(factor(inducted) ~ tH + tHR + tR + tSB + tRBI + tBA + mvp + gg + ASgame, data=rf.bat.train, ntree=1000, replace=TRUE, strata=factor(rf.bat.train$inducted), keep.forest=TRUE, importance=TRUE, localImp=TRUE, proximity=TRUE, mtry=3)
print(rf.bat)
head(rf.bat,10)

##getpredictions
rf.bat.train$predict <- rf.bat$predicted

par(mfrow=c(1,1))
varImpPlot(rf.bat, type=2, pch=20, main = "Variable Importance")
summary(rf.bat)
ggplot(importance(rf.bat)[,4])

#predict on test set
future.bat <- predict(rf.bat, rf.bat.test, type="response")
future.bat.vote <- predict(rf.bat, rf.bat.test, type="vote", norm.votes=TRUE)

rf.bat.test$HOF <- as.numeric(future.bat) - 1
rf.bat.test$votes <- future.bat.vote[,2]

batterHOF <- subset(rf.bat.test[,c(1,18,19,20,21)], rf.bat.test$votes > 0.1)

batterHOF[order(-batterHOF$votes),]
