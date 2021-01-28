# 2015313693 통계학과 권남택
# exercise

library(tidyverse)
library(tidyr)
library(Lahman)


# given data
manny <- filter(Batting, playerID == "ramirma02")
mannyBySeason <- Batting %>%
  filter(playerID == "ramirma02") %>%
  inner_join(Master, by = c("playerID" = "playerID")) %>%
  group_by(yearID) %>%
  summarize(
    Age = max(yearID - birthYear), numTeams = n_distinct(teamID),
    BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI),
    OBP = sum(H + BB + HBP) / sum(AB + BB + SF + HBP),
    SLG = sum(H + X2B + 2*X3B + 3*HR) / sum(AB)
  ) %>%
  mutate(OPS = OBP + SLG) %>%
  arrange(desc(OPS))
mannyAllstar <- AllstarFull %>% filter(playerID == "ramirma02")


# 1. How many rows are in the data frame manny?
dim(manny)[1]
# answer is 21.


# 2. Display Manny Ramirez’s records (as done above), grouped by teams he played for.
manny %>% 
  group_by(teamID) %>% 
  summarize(span = paste(min(yearID), max(yearID), sep = "-"),
            numYears = n_distinct(yearID), 
            BA = sum(H)/sum(AB), tH = sum(H), tHR = sum(HR), tRBI = sum(RBI)) %>% 
  arrange(span)


# 3. In the above code chunk, if inner_join() is used in place of left_join(), what will be the number
#   of rows of the resulting table?
mannyBySeason %>%
  inner_join(mannyAllstar, by = c("yearID" = "yearID")) %>%
  select(yearID, Age, OPS, GP, startingPos)
# answer is 12.


# 4. Confirm that Barry Bonds has the record for most home runs (762) hit in a career. 
#    For this, list top 20 players’ names with the most home runs, 
#    and confirm that Manny is in the top 20. Note that you will need to join the Batting and 
#    Master tables together to display the players’ name instead of the player ID.
Batting %>% 
  group_by(playerID) %>% 
  summarise(THR = sum(HR)) %>%
  inner_join(Master, by = ("playerID" = "playerID")) %>% 
  mutate(player_name = paste(nameFirst, nameLast)) %>% 
  select(player_name, THR) %>% 
  arrange(desc(THR)) %>% 
  head(20)
# manny ramirez는 15등이다.


# 5. Name every pitcher in baseball history who has accumulated 
#    at least 300 wins (W) and at least 3,000 strikeouts (SO). Use Pitching table.

Pitching %>% 
  group_by(playerID) %>% 
  summarise(Total_Win = sum(W),
            Total_strikeouts = sum(SO)) %>% 
  inner_join(Master, by = ("playerID" = "playerID")) %>% 
  mutate(player_name = paste(nameFirst, nameLast)) %>% 
  filter(Total_Win >= 300 & Total_strikeouts >= 3000) %>% 
  select(player_name, Total_Win, Total_strikeouts) %>% 
  arrange(desc(Total_Win))



# 6. Display a table with 10 most recent World Series MVP awardees. 
#    Include their names and ages. The following code chunk is a good start.

AwardsPlayers %>% 
  filter(awardID == "World Series MVP") %>% 
  filter(yearID >= 2008) %>% 
  inner_join(Master, by = ("playerID" = "playerID"))  %>% 
  mutate(player_name = paste(nameFirst, nameLast),
         age = yearID - birthYear) %>% 
  select(yearID, player_name, age) %>% 
  arrange(desc(yearID))




