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

```{r read in winners, clean data}
Scores = read_csv("NFL_Scores_3.csv") %>% janitor::clean_names()

Scores = Scores %>% 
  mutate(home_team = case_when(
    home_team == "Packers" ~ "Green Bay Packers",
    home_team == "Redskins" ~ "Washington Redskins",
    home_team == "Bills" ~ "Buffalo Bills",
    home_team == "Falcons" ~ "Atlanta Falcons",
    home_team == "Ravens" ~ "Baltimore Ravens",
    home_team == "Chiefs" ~ "Kansas City Chiefs",
    home_team == "Titans" ~ "Tenessee Titans",
    home_team == "Colts" ~ "Indianapolis Colts",
    home_team == "Bengals" ~ "Cincinnati Bengals",
    home_team == "49ers" ~ "San Francisco 49ers",
    home_team == "Giants" ~ "New York Giants",
    home_team == "Lions" ~ "Detroit Lions",
    home_team == "Steelers" ~ "Pittsburgh Steelers",
    home_team == "Texans" ~ "Houston Texans",
    home_team == "Broncos" ~ "Denver Broncos",
    home_team == "Buccaneers" ~ "Tampa Bay Buccaneers",
    home_team == "Cardinals" ~ "Arizona Cardinals",
    home_team == "Bears" ~ "Chicago Bears",
    home_team == "Panthers" ~ "Carolina Panthers",
    home_team == "Eagles" ~ "Philadelphia Eagles",
    home_team == "Jets" ~ "New York Jets",
    home_team == "Vikings" ~ "Minnesota Vikings",
    home_team == "Dolphins" ~ "Miami Dolphins",
    home_team == "Jaguars" ~ "Jacksonville Jaguars",
    home_team == "Browns" ~ "Cleveland Browns",
    home_team == "Chargers" ~ "Los Angeles Chargers",
    home_team == "Seahawks" ~ "Seattle Seahawks",
    home_team == "Cowboys" ~ "Dallas Cowboys",
    home_team == "Patriots" ~ "New England Patriots",
    home_team == "Saints" ~ "New Orleans Saints",
    home_team == "Raiders" ~ "Oakland Raiders",
    home_team == "Rams" ~ "Los Angeles Rams")) %>%
  mutate(away_team = case_when(
    away_team == "Packers" ~ "Green Bay Packers",
    away_team == "Redskins" ~ "Washington Redskins",
    away_team == "Bills" ~ "Buffalo Bills",
    away_team == "Falcons" ~ "Atlanta Falcons",
    away_team == "Ravens" ~ "Baltimore Ravens",
    away_team == "Chiefs" ~ "Kansas City Chiefs",
    away_team == "Titans" ~ "Tenessee Titans",
    away_team == "Colts" ~ "Indianapolis Colts",
    away_team == "Bengals" ~ "Cincinnati Bengals",
    away_team == "49ers" ~ "San Francisco 49ers",
    away_team == "Giants" ~ "New York Giants",
    away_team == "Lions" ~ "Detroit Lions",
    away_team == "Steelers" ~ "Pittsburgh Steelers",
    away_team == "Texans" ~ "Houston Texans",
    away_team == "Broncos" ~ "Denver Broncos",
    away_team == "Buccaneers" ~ "Tampa Bay Buccaneers",
    away_team == "Cardinals" ~ "Arizona Cardinals",
    away_team == "Bears" ~ "Chicago Bears",
    away_team == "Panthers" ~ "Carolina Panthers",
    away_team == "Eagles" ~ "Philadelphia Eagles",
    away_team == "Jets" ~ "New York Jets",
    away_team == "Vikings" ~ "Minnesota Vikings",
    away_team == "Dolphins" ~ "Miami Dolphins",
    away_team == "Jaguars" ~ "Jacksonville Jaguars",
    away_team == "Browns" ~ "Cleveland Browns",
    away_team == "Chargers" ~ "Los Angeles Chargers",
    away_team == "Seahawks" ~ "Seattle Seahawks",
    away_team == "Cowboys" ~ "Dallas Cowboys",
    away_team == "Patriots" ~ "New England Patriots",
    away_team == "Saints" ~ "New Orleans Saints",
    away_team == "Raiders" ~ "Oakland Raiders",
    away_team == "Rams" ~ "Los Angeles Rams")) %>%
  mutate(winner = case_when(home_score > away_score ~ home_team,
                            home_score == away_score ~ "TIE",
                            home_score < away_score ~ away_team))  


wins = function(weeks){
  Scores %>% select(week, winner) %>% 
    filter(week == weeks) %>%
    select(winner)
}

winners = map(weeks, wins)
this_week = pull(winners[[length(winners)]]) #creating a vector of this weeks winners  
```

```{r games, include=FALSE}
#How many games in each week
weeks = list(1, 2, 3)
week_n = function(weeks){
  dim(winners[[weeks]])[1]
}

weekly_games = map_dbl(weeks, week_n)

# n = 16 #number of games this week
# wk1_n = 16 # number of games in week 1
# wk2_n = 16 # number of games in week 2
wk1_wins = 10 #number of wins in week 1
wk2_wins = 10 #number of wins in week 2
wk1_combined_wins = 12 # number of combined wins in week 1
wk2_combined_wins = 10 # number of combined wins in week 2

# who actually won
# winners = c("Jacksonville Jaguars","Buffalo Bills","Detroit Lions","New England Patriots","Minnesota Vikings","Kansas City Chiefs","Indianapolis Colts","Green Bay Packers","Dallas Cowboys","New York Giants","Carolina Panthers","San Francisco 49ers","New Orleans Saints","Houston Texans","Los Angeles Rams","Chicago Bears")

CBSsports = c("Jacksonville Jaguars","Buffalo Bills","Philidelphia Eagles","New England Patriots","Minnesota Vikings","Baltimore Ravens","Atlanta Falcons","Green Bay Packers","Dallas Cowboys","Tampa Bay Buccaneers","Carolina Panthers","San Francisco 49ers","Seattle Seahawks","Los Angeles Chargers","Los Angeles Rams","Chicago Bears")

# wk1_winners = c("Green Bay Packers","Minnesota Vikings","Baltimore Ravens","Buffalo Bills","Kansas City Chiefs","Los Angeles Rams","Tennessee Titans","Philadelphia Eagles","Seattle Seahawks","Los Angeles Chargers","TIE","Dallas Cowboys","San Francisco 49ers","New England Patriots","New Orleans Saints","Oakland Raiders")
# 
# wk2_winners = c("Tampa Bay Buccaneers","Baltimore Ravens","Dallas Cowboys","Indianapolis Colts","Seattle Seahawks","Buffalo Bills","San Fransisco 49ers","Detroit Lions","Green Bay Packers","Houston Texans","New England Patriots","Kansas City Chiefs","Los Angeles Rams","Chicago Bears","Atlanta Falcons","Cleveland Browns")

#wk1_CBSsports = c("Green Bay Packers","Atlanta Falcons","Miami Dolphins","New York Jets","Kansas City Chiefs","Carolina Panthers","Cleveland Browns","Philadelphia Eagles","Seattle Seahawks","Indianapolis Colts","Arizona Cardinals","Dallas Cowboys","Tampa Bay Buccaneers","Pittsburgh Steelers","New Orleans Saints","Oakland Raiders")

#wk2_CBSsports = c("Carolina Panthers","Baltimore Ravens","Dallas Cowboys","Indianapolis Colts","Seattle Seahawks","Buffalo Bills","Cincinnati Bengals","Los Angeles Chargers","Green Bay Packers","Houston Texans","New England Patriots","Kansas City Chiefs","Los Angeles Rams","Chicago Bears","Philadelphia Eagles","Cleveland Browns")

experts = ifelse(winners == CBSsports, 1, 0)
CBS = sum(experts)/n
```

## Group Predictions

```{r Group Predictions, include=FALSE}
da = list(week_1, week_2, data)
ga = function(x){
  x %>% janitor::clean_names() %>%
  select(starts_with("game"))
  } 
games = da %>% map(ga)

fn1 = function(x) {
  t_x <- sort(table(x), decreasing=TRUE) # get the frequencies and sort them in decreasing order
  list(Prediction=names(t_x)[1], # name of the value with highest frequency
       votes_for=t_x[1], # highest frequency
       votes_against=t_x[2]) # Lowest Freq
}

# creating a table of predictions and votes for and against the prediction
pred_table = NULL
for (i in 1:length(games)){
  pred_table[[i]] = games[[1]] %>% map_df(fn1)
}

a = function(x){
  x %>% mutate(votes_against = ifelse(is.na(votes_against)==T, 0, as.integer(votes_against)))}  # turning NA in votes against column into 0
pred_table = map(pred_table, a)  
b = function(x){
  x %>% mutate(pred = ifelse(as.numeric(votes_for) == as.numeric(votes_against), "Tie", as.character(Prediction))) %>%
    select(pred, votes_for, votes_against)} #choosing prediction or tie if no prediction}
pred = map(pred_table, b)

# # votes_for = as.integer(pred_table$votes_for)
# votes_against = as.integer(pred_table$votes_against)
dim(pred[[1]])[1]

c = function(x, winners){ 
  x%>% mutate(Game = 1:dim(x)[1]) %>% 
  add_column(Prediction = x$pred) %>%
  add_column(Winner = winners) %>%
  mutate(Correct = ifelse(Prediction == Winner, "Yes", "No")) %>%
  mutate(Correct_Votes = ifelse(Prediction == Winner, votes_for, ifelse(Winner == "TIE", 0, votes_against))) %>%
  mutate(Percentage_For = round(Correct_Votes/dim(x)[1],4)) %>%
  select(Game, Prediction, Winner, Correct, Correct_Votes, Percentage_For)%>%
  rename("Correct Votes" = Correct_Votes) %>%
  rename("Correct Percent" = Percentage_For)}

results = map2(pred,winners, c)
results
