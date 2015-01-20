require(ggplot2)
require(randomForest)
require(tree)
require(XML)
require(plyr)
require(stringr)


get_years <- function(year) {
  br <- paste0("http://www.baseball-reference.com/leagues/MLB/", year, "-misc.shtml")
  sal <- data.frame(readHTMLTable(br, stringsAsFactors = FALSE))
  sal$year <- year
  sal
}

get_stand <- function(year) {
  br2 <- paste0("http://www.baseball-reference.com/leagues/MLB/", year, "-standings.shtml")
  stand <- readHTMLTable(br2, stringsAsFactors = FALSE)
 #1994 was cancelled because of strike
  if(year==1994){
    stand<-data.frame(stand[[1]])
  }
  else{
    stand <- data.frame(stand[[2]])  
  }
  stand$year <- year
  stand
}

make_numeric <- function(x) {
  x <- str_replace_all(x, ",", "") # remove all commas
  x <- str_replace_all(x, "[$]", "") # remove all $
  as.numeric(x) # cast as a number
}

attendance <- ldply(1950:2013, get_years, .progress="text")
standings <-  ldply(1950:2013, get_stand, .progress="text")

# clean up the column names
names(attendance) <- c("tm", "attendance", "attend_per_game", "batage", "page", "bpf", "ppf", "n_hof", "n_aallstars", "n_a_ta_s", "est_payroll", "gametime","managers", "year")

attendance$managers <- NULL
attendance$gametime <- NULL


standings$SOS <- NULL
standings$SRS <- NULL
standings$pythWL <- NULL
standings$Luck <- NULL
standings$Home <- NULL
standings$Road <- NULL
standings$ExInn <- NULL
standings$X1Run <- NULL
standings$vRHPW <- sapply(strsplit(as.character(standings$vRHP), "\\-"), "[", 1)
standings$vRHPL <- sapply(strsplit(as.character(standings$vRHP), "\\-"), "[", 2)
standings$vLHPW <- sapply(strsplit(as.character(standings$vLHP), "\\-"), "[", 1)
standings$vLHPL <- sapply(strsplit(as.character(standings$vLHP), "\\-"), "[", 2)
standings$vRHP <- NULL
standings$vLHP <- NULL
standings$X..500 <- NULL
standings$X..500.1 <- NULL
standings$Inter <- NULL


##setting up merge
names(standings)[names(standings)=="Tm"] <- "tm"

#get wins and games information from teams table
MLBHist <- merge(x=attendance,y=standings,by=c("year","tm"))

#drop teams
rm(attendance)
rm(standings)

#make information numeric
MLBHist$attendance <- make_numeric(MLBHist$attendance)
MLBHist$attend_per_game <- make_numeric(MLBHist$attend_per_game)
MLBHist$batage <- make_numeric(MLBHist$batage)
MLBHist$page <- make_numeric(MLBHist$page)
MLBHist$bpf <- make_numeric(MLBHist$bpf)
MLBHist$ppf <- make_numeric(MLBHist$ppf)
MLBHist$n_hof <- make_numeric(MLBHist$n_hof)
MLBHist$n_aallstars <- make_numeric(MLBHist$n_aallstars)
MLBHist$n_a_ta_s <- make_numeric(MLBHist$n_a_ta_s)
MLBHist$est_payroll <- make_numeric(MLBHist$est_payroll)
MLBHist$Rk <- make_numeric(MLBHist$Rk)
MLBHist$G <- make_numeric(MLBHist$G)
MLBHist$W <- make_numeric(MLBHist$W)
MLBHist$L <- make_numeric(MLBHist$L)
MLBHist$W.L <- make_numeric(MLBHist$W.L)
MLBHist$R <- make_numeric(MLBHist$R)
MLBHist$RA <- make_numeric(MLBHist$RA)
MLBHist$Rdiff <- make_numeric(MLBHist$Rdiff)
MLBHist$vRHPW <- make_numeric(MLBHist$vRHPW)
MLBHist$vRHPL <- make_numeric(MLBHist$vRHPL)
MLBHist$vLHPW <- make_numeric(MLBHist$vLHPW)
MLBHist$vLHPL <- make_numeric(MLBHist$vLHPL)

##our data frame should be ready


attendance_by_year = data.frame(aggregate(MLBHist$attendance ~ MLBHist$year, MLBHist, sum))
ggplot(attendance_by_year, aes(x=MLBHist.year, y=MLBHist.attendance))+geom_line()



