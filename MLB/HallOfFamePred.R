require(ggplot2)
require(randomForest)
require(tree)
require(XML)
require(plyr)
require(stringr)
require(tree)

batting <- data.frame(read.csv("Batting.csv", header=TRUE))
hof <- data.frame(read.csv("HallOfFame.csv", header=TRUE))

batting$stint <- NULL
batting$lgID <- NULL
batting$teamID <- NULL
batting$yearID <- NULL

batting[is.na(batting)] <- 0

##interesting, if i want to check how many years on ballot
      # hofCounts <- aggregate(.~playerID+inducted,hof,length)
      # hofCounts[order(hofCounts$playerID),]


battingagg <- aggregate(.~playerID, batting, sum)
HallOfFame <-  merge(x=battingagg,y=hof,by=c("playerID"))

summary(HallOfFame)
summary(HallOfFame)
HallOfFame <- HallOfFame[HallOfFame$votedBy =="BBWAA",]
HallOfFame$votedBy <- NULL
HallOfFame$ballots <- NULL
HallOfFame$needed <- NULL
HallOfFame$votes <- NULL
HallOfFame$category <- NULL
HallOfFame$needed_note <- NULL

HallOfFame$inducted <- ifelse(HallOfFame$inducted == "Y", 1, 0)

HallOfFame <- aggregate(.~playerID,HallOfFame, max)

HallOfFame$inducted <- as.factor(HallOfFame$inducted)
HallOfFame$playerID = as.character(HallOfFame$playerID)

##only want batters... this was taken from baseball reference
HallOfFame <- HallOfFame[HallOfFame$H > 700,]

#this should be ok.

summary(HallOfFame)

#create one decision tree based on all the data. this will overfit of course
tree.hof = tree(inducted~.-playerID, data=HallOfFame)
summary(tree.hof)

##looking at the tree, this is very bushy, with 22 terminal nodes
plot(tree.hof)
text(tree.hof)

#lets split to training and test
set.seed(919)

train=sample(1:nrow(HallOfFame), 400)

tree.hof=tree(inducted~.-playerID-yearid, HallOfFame,subset=train)

plot(tree.hof)
text(tree.hof)

tree.pred=predict(tree.hof,HallOfFame[-train,],type="class")

with(HallOfFame[-train,],table(tree.pred,inducted))

#what was predicted correctly
(216+14)/(216+14+12+16)

#lets do cross validation to prune the tree.
cv.hof = cv.tree(tree.hof,FUN=prune.misclass)
cv.hof
plot(cv.hof)

##bottoms out around7, lets prune our tree

prune.hof = prune.misclass(tree.hof, best = 7)

plot(prune.hof);text(prune.hof,pretty=0)

#now lets try this on test data

tree.pred=predict(prune.hof,HallOfFame[-train,],type="class")

with(HallOfFame[-train,],table(tree.pred,inducted))

#we did a little better!


(211+15)/(211+15+11+11)
