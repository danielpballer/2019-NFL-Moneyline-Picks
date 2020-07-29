knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(formattable)
library(ggpubr)
library(ggrepel)
week_1 = read_csv("NFL Week 1.csv")
week_2 = read_csv("NFL Week 2.csv")
data = read_csv("NFL Week 3.csv")
cadet_wk1 = read_csv("NFL Week 1 Prediction MA206.csv")
cadet_wk2 = read_csv("NFL Week 2 MA206.csv")
cadet_wk3 = read_csv("NFL Week 3 MA206.csv")
```

```{r games, include=FALSE}
#basic info
all_dat = list(week_1, week_2, data)

# n = 16 #number of games this week
# wk1_n = 16 # number of games in week 1
# wk2_n = 16 # number of games in week 2
wk1_wins = 10 #number of wins in week 1
wk2_wins = 10 #number of wins in week 2
wk1_combined_wins = 12 # number of combined wins in week 1
wk2_combined_wins = 10 # number of combined wins in week 2
n = 16 #number of games this week
wk1_n = 16 # number of games in week 1
wk2_n = 16 # number of games in week 2
wk1_wins = 10 #number of wins in week 1
wk2_wins = 10 #number of wins in week 2
wk1_combined_wins = 12 # number of combined wins in week 1
wk2_combined_wins = 10 # number of combined wins in week 2

season_games = n+wk1_n+wk2_n
season_wins  = wk1_wins+wk2_wins
# who actually won
winners = c("Jacksonville Jaguars","Buffalo Bills","Detroit Lions","New England Patriots",
            "Minnesota Vikings","Kansas City Chiefs","Indianapolis Colts","Green Bay Packers",
            "Dallas Cowboys","New York Giants","Carolina Panthers","San Francisco 49ers",
            "New Orleans Saints","Houston Texans","Los Angeles Rams","Chicago Bears") # Who won this week

CBSsports = c("Jacksonville Jaguars","Buffalo Bills","Philidelphia Eagles","New England Patriots",
              "Minnesota Vikings","Baltimore Ravens","Atlanta Falcons","Green Bay Packers",
              "Dallas Cowboys","Tampa Bay Buccaneers","Carolina Panthers","San Francisco 49ers",
              "Seattle Seahawks","Los Angeles Chargers","Los Angeles Rams","Chicago Bears") #CBS picks this week

wk1_winners = c("Green Bay Packers","Minnesota Vikings","Baltimore Ravens","Buffalo Bills",
                "Kansas City Chiefs","Los Angeles Rams","Tennessee Titans","Philadelphia Eagles",
                "Seattle Seahawks","Los Angeles Chargers","TIE","Dallas Cowboys","San Francisco 49ers",
                "New England Patriots","New Orleans Saints","Oakland Raiders") # Week 1 Picks

wk2_winners = c("Tampa Bay Buccaneers","Baltimore Ravens","Dallas Cowboys","Indianapolis Colts",
                "Seattle Seahawks","Buffalo Bills","San Fransisco 49ers","Detroit Lions",
                "Green Bay Packers","Houston Texans","New England Patriots","Kansas City Chiefs",
                "Los Angeles Rams","Chicago Bears","Atlanta Falcons","Cleveland Browns") # Week 2 Picks

all_win = list(wk1_winners, wk2_winners, winners) #Creating list of all winners

weeks = list(1, 2, 3) # List of the weeks so far

#Function that gets the number of games in each week
week_n = function(weeks){
  length(all_win[[weeks]])
}

weekly_games = map_dbl(weeks, week_n) #Creating a list of the number of games in each week

#Function to get the survey results for the week
indiv_wk_fn = function (x) {
  x %>% janitor::clean_names() %>%
  select(name, starts_with("game"))
}

indiv_wks = map(all_dat, indiv_wk_fn) #Creating a list of all picks each week 

indiv_wks %>% select(-name)

indiv_wks[[1]]$name
#### Counting the correct number of pics for each person  

games_wk = indiv_wk1 %>%
  select(-name)  #Creating the set of just games

#Function to get the survey results (just games) for the week
indiv_game_wk_fn = function (x) {
  x %>% janitor::clean_names() %>%
    select(starts_with("game"))
}
games_wks = map(all_dat, indiv_game_wk_fn) #Creating a list of all picks (just games) each week 


function (x){ 
  
  }

a = function (x){ 
indiv_correct = NULL
help = NULL
for (i in 1:length(indiv_wks[[weeks]]$name)){
  for(j in 1:length(games_wks[[weeks]])){
    help[j] = ifelse(games_wks[[weeks]][i,j]==all_win[[weeks]][j],1,0)
    indiv_correct[i] = sum(help)
  }
}
return(indiv_correct)
}

indiv_correct %>% class()
indiv_wks %>% map(a)
map(indiv_wks, games_wks, a)

# Calculating individual win number for week 1
season_wk1 = indiv_wk1 %>%
  select(name) %>%
  add_column(Week_1 = indiv_correct_wk1)

season = season_wk1

# Calculating Week 2 Individual Results
indiv_wk2 = week_2 %>%
  select(`Name`, `Game 1`,`Game 2`, `Game 3`, `Game 4`, `Game 5`, `Game 6`, `Game 7`, `Game 8`, `Game 9`, `Game 10`, `Game 11`, `Game 12`, `Game 13`, `Game 14`, `Game 15`, `Game 16`) %>%  janitor::clean_names()

# Counting the correct number of pics for each person  

games_wk2 = week_2 %>% janitor::clean_names() %>%
  select(starts_with("game"))  #Creating the set of just games

indiv_correct_wk2 = NULL
help_wk2 = NULL
for (i in 1:length(indiv_wk2$name)){
  for(j in 1:length(games_wk2)){
    help_wk2[j] = ifelse(games_wk2[i,j]==wk2_winners[j],1,0)
    indiv_correct_wk2[i] = sum(help_wk2)
  }
}

# Calculating individual win number for week 2
season_wk2 = indiv_wk2 %>%
  select(name) %>%
  add_column(Week_2 = indiv_correct_wk2)

# Printing the individual table 
season = season %>%
  full_join(season_wk2)
```

```{r individual, include=FALSE}
# Creating the set of names and games

indiv = data %>%
  select(`Name`, `Game 1`,`Game 2`, `Game 3`, `Game 4`, `Game 5`, `Game 6`, `Game 7`, `Game 8`, `Game 9`, `Game 10`, `Game 11`, `Game 12`, `Game 13`, `Game 14`, `Game 15`, `Game 16`) %>%  janitor::clean_names()

# Counting the correct number of pics for each person  

indiv_correct = NULL
help = NULL
for (i in 1:length(indiv$name)){
  for(j in 1:length(games)){
    help[j] = ifelse(games[i,j]==winners[j],1,0)
    indiv_correct[i] = sum(help)
  }
}

# Calculating individual win percentage, combining data and orderign by percentage 
indiv = indiv %>% 
  add_column(WK3_Correct = indiv_correct) %>%
  mutate(WK3_Percent = WK3_Correct/n) %>%
  rename("Week_3" = WK3_Correct)%>%
  rename("Percent" = WK3_Percent)%>%
  arrange(-Percent)

# choosing just name # correct pics and correct percentage
indiv_disp = indiv %>%
  select(name, Week_3, Percent) %>%
  full_join(season) %>%
  mutate(w1p = ifelse(is.na(Week_1)==T,0,1)) %>%
  mutate(w2p = ifelse(is.na(Week_2)==T,0,1)) %>%
  mutate(w3p = ifelse(is.na(Week_3)==T,0,1)) %>%
  mutate(Weeks_Picked = w1p+w2p+w3p) %>%
  mutate(games_picked = ((w1p*16)+(w2p*16)+(w3p*16))) %>%
  mutate(Season_Percent = round(((ifelse(is.na(Week_1)==T, 0, Week_1) + 
                                    ifelse(is.na(Week_2)==T,0,Week_2) +
                                    ifelse(is.na(Week_3)==T, 0, Week_3))/games_picked),4)) %>%
  select(name, Week_1, Week_2, Week_3, Percent, Weeks_Picked, Season_Percent) %>%
  arrange(desc(Percent), desc(Season_Percent))
