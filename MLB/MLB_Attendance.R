require(ggplot2)
require(randomForest)
require(tree)
require(XML)
require(plyr)
require(stringr)

br <- "http://www.baseball-reference.com/leagues/MLB/1990-misc.shtml"
sal <- data.frame(readHTMLTable(br, stringsAsFactors = FALSE))

get_years <- function(year) {
  br <- paste0("http://www.baseball-reference.com/leagues/MLB/", year, "-misc.shtml")
  sal <- data.frame(readHTMLTable(br, stringsAsFactors = FALSE))
  sal$year <- year
  sal
}

make_numeric <- function(x) {
  x <- str_replace_all(x, ",", "") # remove all commas
  x <- str_replace_all(x, "[$]", "") # remove all $
  as.numeric(x) # cast as a number
}

attendance <- ldply(1950:2013, get_years, .progress="text")


# clean up the column names
names(attendance) <- c("tm", "attendance", "attend_per_game", "batage", "page", "bpf", "ppf", "n_hof", "n_aallstars", "n_a_ta_s", "est_payroll", "gametime","managers", "year")


#gettings teams data
teams = read.csv("MLBTeams.csv", header = TRUE)

##setting up merge
names(teams)[names(teams)=="franchID"] <- "tm"
names(teams)[names(teams)=="yearID"] <- "year"

#get wins and games information from teams table
newatt <- merge(x=attendance,y=teams[,c("W", "G","L", "WSWin" , "year", "tm")],by=c("year","tm"), all.x = TRUE)

#drop teams
rm(teams)
rm(attendance)

#make information numeric
newatt$attendance <- make_numeric(newatt$attendance)
newatt$attend_per_game <- make_numeric(newatt$attend_per_game)
newatt$est_payroll <- make_numeric(newatt$est_payroll)






