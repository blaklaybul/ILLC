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
draw.tree(pruned_bat)

bat.pred.prune=predict(pruned_bat,candidatesBat[-train,],type="class")
with(candidatesBat[-train,],table(bat.pred.prune,inducted))


summary(bat_tree)
printcp(bat_tree)
draw.tree(bttree)

names(Pitching)

pitch_tree = rpart(as.factor(inducted) ~ numSeasons+ tIP+tW+tSO+tSV+tERA+tWHIP+mvp+cy, data=candidatesPitch)
draw.tree(pitch_tree)


names(candidatesBat)
