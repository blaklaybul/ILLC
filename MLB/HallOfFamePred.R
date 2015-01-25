require(ggplot2)
require(randomForest)
require(tree)
require(XML)
require(plyr)
require(stringr)
require(tree)
require(rpart)
require(rpart.plot)
require(party) 

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

HallOfFame <- HallOfFame[HallOfFame$votedBy =="BBWAA",]
HallOfFame <- HallOfFame[HallOfFame$RBI > 300,] #we only want hitters
HallOfFame$OBP <- as.numeric((HallOfFame$H+HallOfFame$BB+HallOfFame$HBP)/(HallOfFame$AB+HallOfFame$BB+HallOfFame$HBP+HallOfFame$SF))
HallOfFame$votedBy <- NULL
HallOfFame$ballots <- NULL
HallOfFame$needed <- NULL
HallOfFame$votes <- NULL
HallOfFame$category <- NULL
HallOfFame$needed_note <- NULL
HallOfFame$G_old <- NULL
HallOfFame$yearid <- NULL
HallOfFame$CS <- NULL
HallOfFame$HBP <- NULL
HallOfFame$SH <- NULL
HallOfFame$SF <- NULL
HallOfFame$GIDP <- NULL
HallOfFame$playerID = as.character(HallOfFame$playerID)

HallOfFame[is.na(HallOfFame$inducted),c("inducted")] <- "N"

HallOfFame$inducted <- ifelse(HallOfFame$inducted == "Y", 1, 0)
HallOfFame <- aggregate(.~playerID,HallOfFame, max)

HallOfFame$inducted <- as.factor(HallOfFame$inducted)

HallOfFameYes <- HallOfFame[HallOfFame$inducted == 1,]


##only want batters... this was taken from baseball reference
# HallOfFame <- HallOfFame[HallOfFame$H > 700,]

#this should be ok.
ggplot(HallOfFame, aes(x=(H/AB), y=H, col =inducted)) +geom_point(size = 3) + scale_x_continuous(name = "Batting Average")+ scale_y_continuous(name="Hits") + theme(text = element_text(size=20))


ggplot(HallOfFame, aes(x=(SO/AB), y=OBP, col =inducted)) +geom_point(size = 3) + scale_x_continuous(name = "SO")+ scale_y_continuous(name="BB") + theme(text = element_text(size=20))

#create one decision tree based on all the data. this will overfit of course
tree.hof = tree(inducted~.-playerID, data=HallOfFame)
summary(tree.hof)


##looking at the tree, this is very bushy, with 22 terminal nodes
plot(tree.hof)
text(tree.hof)


#lets split to training and test
set.seed(919)
train=sample(1:nrow(HallOfFame), 400)
tree.hof=tree(inducted~.-playerID-inducted, HallOfFame,subset=train)
summary(tree.hof)
plot(tree.hof, main = "Decision Tree")
text(tree.hof, all = FALSE)

tree.pred=predict(tree.hof,HallOfFame[-train,],type="class")
with(HallOfFame[-train,],table(tree.pred,inducted))

#what was predicted correctly
(203+18)/(203+18+19+11)

#lets do cross validation to prune the tree.
cv.hof = cv.tree(tree.hof,FUN=prune.misclass)
par(mfrow=c(1,1))
plot(cv.hof$size, cv.hof$dev, type="b")
plot(cv.hof$k, cv.hof$dev, type="b")
cv.hof
plot(cv.hof)

##bottoms out around7, lets prune our tree

prune.hof = prune.misclass(tree.hof, best = 5)
summary(prune.hof)

plot(prune.hof);text(prune.hof,pretty=0)

#now lets try this on test data

tree.pred=predict(prune.hof,HallOfFame[-train,],type="class")

with(HallOfFame[-train,],table(tree.pred,inducted))

#we did a little better!


(211+15)/(211+15+11+11)


hof.tree=tree(inducted~H+RBI, data=HallOfFame)
plot(hof.tree)
plot(HallOfFame$H,HallOfFame$RBI,col=HallOfFame$inducted)
partition.tree(hof.tree, add = TRUE)


