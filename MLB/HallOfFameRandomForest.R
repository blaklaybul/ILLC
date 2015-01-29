require(Lahman)
require(mosaic)
require(randomForest)
require(dplyr)

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
candidatesBat <- merge(candidatesBat, inductees, by="playerID", all.x=T)
  
  #pitching
candidatesPitch <- merge(pitching, awards, by="playerID", all.x=T)
candidatesPitch <- merge(candidatesPitch, allstar, by="playerID", all.x=T)
candidatesPitch <- merge(candidatesPitch, inductees, by="playerID", all.x=T)

#replace n/a's
candidatesBat[is.na(candidatesBat)] <- 0
candidatesPitch[is.na(candidatesPitch)] <- 0

##get player names
Pnames <- Master[,c("playerID", "nameLast", "nameFirst")]
candidatesBat <- merge(candidatesBat, Pnames, by="playerID", all.x=T)
candidatesPitch <- merge(candidatesPitch, Pnames, by="playerID", all.x=T)
candidatesBat <- merge(candidatesBat, Pnames, by="playerID", all.x=T)
candidatesPitch <- merge(candidatesPitch, Pnames, by="playerID", all.x=T)

head(candidatesBat)
