#importing data 
library(tidyverse)
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(dbplyr)
library(tidyr)
goalkeepers <- read_csv("goalkeepers-1920.csv" , col_names = TRUE)
players <- read_csv("players-1920.csv", col_names = TRUE)
scoresfix <- read_csv("scoresfixtures-1920.csv", col_names = TRUE)
standings <- read_csv("standings-1920.csv", col_names = TRUE)
teamgoalkeep <- read_csv("teamgoalkeeping-1920.csv", col_names = TRUE)
teamstats <- read_csv("teamstats-1920.csv", col_names = TRUE)

#1a How many teams are there in the league?
nrow(teamstats)
unique(teamstats$Squad)

#1b How many players are there in the league?
unique(players$Player)
nrow(players)

#1c  What is the total number of goals scored for the season?
sum(players$Gls)
sum(teamstats$Gls)

#1d What is the average attendance (spectators) at the games?
mean(scoresfix$Attendance, na.rm = TRUE)

#2 Identify the primary key for each table. Verify your answers. Note: ignore the “Rk” variable, as it usually just represents the row number.
goalkeepers %>% count(Player) %>% filter(n>1)
players %>% count(Player, Squad) %>% filter(n>1)
scoresfix %>% count(Date, Home) %>% filter (n>1)
standings %>% count(Squad) %>% filter (n>1)
standings %>% count(Squad) %>% filter (n>1)
teamgoalkeep %>% count(Squad) %>% filter(n>1)
teamstats %>% count(Squad) %>% filter (n>1)


#3 Find a table with a foreign key(s), and write down the name of the table, the foreign key(s) and the associated table(s)
# players has foreign keys for squad and player
# standings 

standings %>% count(Squad) %>% filter (n>1)
teamgoalkeep %>% count(Squad) %>% filter(n>1)
teamstats %>% count(Squad) %>% filter (n>1)

#4 For the players and goalkeepers data sets, the “Player” variable has two versions of the player name, 
# and the “Nation” variable also has two abbreviations listed for the nationalities of the players. 
# Create a new players tibble and a new goalkeepers tibble which contain only the first version of the 
# player name (and not the second version), and the two abbreviations for “Nation” separated out into two columns, 
#plus all the other columns unmodified. Assign the resulting tibbles to new names dfferent from the original ones.

goalkeepers1 <- goalkeepers %>% separate(Player, into= c("Name","Name2"), sep="\\\\")
goalkeepers1 <- goalkeepers1[,-3]
goalkeepers1 <- goalkeepers1 %>% separate(Nation, into= c("Nation","Nation2"), sep=" ")
playersep <- players %>% separate(Player, into= c("Name","Name2"), sep="\\\\")
playersep <- playersep[,-3]
playersep <- playersep %>% separate(Nation, into= c("Nation","Nation2"), sep=" ")

print(goalkeepers1, n=5)
print(playersep, n=5)


#5 Find, for each team, the mean player age, the age of the youngest player and the age of the oldest player. 
# Use this info to find the name(s) of the youngest player(s) for each team. Your output for the latter, separate
# from the output of the first part, should only contain the team name, the minimum age, and the name of the player.

players %>% select(Squad, Age) %>% na.omit() %>% group_by(Squad) %>% summarize(SMeanAge = mean(Age), SMaxAge = max(Age), SMinAge = min(Age))
players %>% select(Player, Squad, Age) %>% na.omit() %>% group_by(Squad) %>% summarize(SquadMinAge = min(Age), Playername = Player[which.min(Age)])

# 6 Add the team performance numbers (specifically these ones: Possession, Assists, Penalty Kicks 
# scored, Save percentage, and Clean sheet percentage) to the standings table. Add also the age statistics 
# found in Q5 above to the standings table. Then make the following plots (A vs B means A is on the y axis):

teamstats1 <- teamstats %>% select(Poss, Ast, PK, Squad) %>% left_join(standings, by="Squad")
finaltable <- teamgoalkeep %>% select(`CS%`, `Save%`, Squad) %>% left_join(teamstats1, by="Squad")
players1 <- players %>% select(Player, Squad, Age) %>% na.omit() %>% group_by(Squad) %>% summarize(SMeanAge = mean(Age), SMaxAge = max(Age), SMinAge = min(Age), Playername = Player[which.min(Age)])
finaltable1 <- players1 %>% select(Squad, SMeanAge, SMinAge, SMaxAge, Playername) %>% left_join(finaltable, by="Squad")
print(finaltable1, n= 6) 

# 6a Points attained vs Possession

pavspos <- ggplot(finaltable1, mapping=aes(x=Poss,y=Pts)) + geom_point() + labs(title= "Points attained vs Possession", x = "Possessions", y="Points Attained" )
pavspos

#6b Points attained vs Save percentage
pavasp <- ggplot(finaltable1, mapping=aes(x=`Save%`,y=Pts)) + geom_point() + labs(title= "Points attained vs Save Percentage", x = "Save Percentage", y="Points Attained" )
pavasp

#6c Points attained vs Goal against
pavsga <- ggplot(finaltable1, mapping=aes(x=GA,y=Pts)) + geom_point() + labs(title= "Points attained vs Goal Against", x = "Goal Against", y="Points Attained" )
pavsga

#6d Clean sheet percentage vs Save percentage
cspvssp <- ggplot(finaltable1, mapping=aes(x=`Save%`,y=`CS%`)) + geom_point() + labs(title= "Clean sheet percentage vs Save percentage")
cspvssp

#7 Identify the names of the top 3 referees that refereed the most game. week and date the game was played, 
# name of the home and away teams, and the name of the referee.

refereerank <- scoresfix %>% count(Referee, sort = TRUE)
print(refereerank, n=3)


refrank2 <- filter(scoresfix, Referee == "Anthony Taylor"|Referee == "Martin Atkinson"|Referee == "Michael Oliver")
refrank3 <- refrank2 %>% select(Referee, Date, Time, Home, Away, Venue)
refrank3

#8 Obtain a list of the games played by the top 3 scoring teams (i.e. the 3 teams that scored the most goals), showing only 
# week, date, home and away teams and the result (final score 

bestteam <- teamstats %>% select(Squad, Gls)
bestteam1 <- arrange(bestteam, desc(Gls))
print(bestteam1, n=3)


bestest <- filter(scoresfix, Home == "Manchester City" | Home == "Liverpool"| Home == "Chelsea" | Away == "Manchester City" | Away  == "Liverpool"| Away == "Chelsea")
bestest2 <- bestest %>% select(Wk, Date, Home, Away, Score)
print(bestest2, n=5)
#9  Find the 3 dates with the most games played. Your R output should only show three dates. Then obtain all the games that were played on these days (you can keep all the columns).

topdays <- scoresfix %>% count(Date, sort=TRUE)
print(topdays, n=3)

topdayschart <- filter(scoresfix, Date == as.Date("2020-07-26") | Date == as.Date("2019-12-26") | Date == as.Date("2020-01-01"))
topdayschart

#10 nd the 3 dates with the most goals scored on those days (i.e. adding up all the goals scored in all the games played on that day). Your resulting tibble should only have 3 rows.

scoresfixq <- scoresfix %>% separate(Score, into= c("S1","S2")) %>%  mutate(S1 = as.numeric(S1), S2 = as.numeric(S2))
scoresfixq1 <- mutate(scoresfixq, totalscore = S1 + S2)
scoresfixq2 <- scoresfixq1 %>% group_by(Date) %>% summarize(totalscore, .groups = 'drop')
finalmostdate <- arrange(scoresfixq2, desc(totalscore))
print(finalmostdate, n=3)

