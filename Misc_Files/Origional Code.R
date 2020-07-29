---
  title: "NFL Week 1"
author: "MAJ Daniel Baller"
date: "8/27/2019"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(formattable)
data = read_csv("NFL Week 1 Money Line.csv")
```


```{r games, include=FALSE}
#basic info
season_games = 0
season_wins  = 0
n = 16 #number of games this week
# who actually won
winners = c("Chicago Bears","Atlanta Falcons","Baltimore Ravens","New York Jets","Kansas City Chiefs","Carolina Panthers","Tennessee Titans","Washington Redskins","Seattle Seahawks","Indianapolis Colts","Detroit Lions","New York Giants","San Francisco 49ers","Pittsburgh Steelers","Houston Texans","Denver Broncos")

CBSsports = c("Green Bay Packers","Atlanta Falcons","Miami Dolphins","New York Jets","Kansas City Chiefs","Carolina Panthers","Cleveland Browns","Philadelphia Eagles","Seattle Seahawks","Indianapolis Colts","Arizona Cardinals","Dallas Cowboys","Tampa Bay Buccaneers","Pittsburgh Steelers","New Orleans Saints","Oakland Raiders")
```

```{r plots, eval=FALSE, include=FALSE}
# Plots for each Game 
Plot1 = ggplot(data,aes(`Game 1`)) + geom_bar(aes(fill = `Game 1`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 2`)) + geom_bar(aes(fill = `Game 2`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 3`)) + geom_bar(aes(fill = `Game 3`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 4`)) + geom_bar(aes(fill = `Game 4`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 5`)) + geom_bar(aes(fill = `Game 5`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 6`)) + geom_bar(aes(fill = `Game 6`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 7`)) + geom_bar(aes(fill = `Game 7`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 8`)) + geom_bar(aes(fill = `Game 8`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 9`)) + geom_bar(aes(fill = `Game 9`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 10`)) + geom_bar(aes(fill = `Game 10`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 11`)) + geom_bar(aes(fill = `Game 11`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 12`)) + geom_bar(aes(fill = `Game 12`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 13`)) + geom_bar(aes(fill = `Game 13`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 14`)) + geom_bar(aes(fill = `Game 14`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 15`)) + geom_bar(aes(fill = `Game 15`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)
ggplot(data,aes(`Game 16`)) + geom_bar(aes(fill = `Game 16`))+
  geom_text(aes(label=..count..),stat="count", vjust=1.6, color="black", size=3.5)

```

```{r Prep for Predictions, include=FALSE}
games = data %>% janitor::clean_names() %>%
  select(starts_with("game"))  #Creating the set of just games

# Pulling the prediction for each game
pred = NULL
# Game1
votes1 = count(games, game_1)
votes1
m1 = max(votes1$n) 
pred[1] = votes1$`game_1`[which(votes1$n == m1)]

# Game2
votes2 = count(games, game_2)
votes2
m2 = max(votes2$n) 
pred[2] = votes2$`game_2`[which(votes2$n == m2)]

# Game3
votes3 = count(games, game_3)
votes3
m3 = max(votes3$n) 
pred[3] = votes3$`game_3`[which(votes3$n == m3)]

# Game4
votes4 = count(games, game_4)
votes4
m4 = max(votes4$n) 
pred[4] = votes4$`game_4`[which(votes4$n == m4)]

# Game5
votes5 = count(games, game_5)
votes5
m5 = max(votes5$n) 
pred[5] = votes5$`game_5`[which(votes5$n == m5)]

# Game6
votes6 = count(games, game_6)
votes6
m6 = max(votes6$n) 
pred[6] = votes6$`game_6`[which(votes6$n == m6)]

# Game7
votes7 = count(games, game_7)
votes7
m7 = max(votes7$n) 
pred[7] = votes7$`game_7`[which(votes7$n == m7)]

# Game8
votes8 = count(games, game_8)
votes8
m8 = max(votes8$n) 
pred[8] = votes8$`game_8`[which(votes8$n == m8)]

# Game9
votes9 = count(games, game_9)
votes9
m9 = max(votes9$n) 
pred[9] = votes9$`game_9`[which(votes9$n == m9)]

# Game10
votes10 = count(games, game_10)
votes10
m10 = max(votes10$n) 
pred[10] = votes10$`game_10`[which(votes10$n == m10)]

# Game11
votes11 = count(games, game_11)
votes11
m11 = max(votes11$n) 
pred[11] = votes11$`game_11`[which(votes11$n == m11)]

# Game12
votes12 = count(games, game_12)
votes12
m12 = max(votes12$n) 
pred[12] = votes12$`game_12`[which(votes12$n == m12)]

# Game13
votes13 = count(games, game_13)
votes13
m13 = max(votes13$n) 
pred[13] = votes13$`game_13`[which(votes13$n == m13)]

# Game14
votes14 = count(games, game_14)
votes14
m14 = max(votes14$n) 
pred[14] = votes14$`game_14`[which(votes14$n == m14)]

# Game15
votes15 = count(games, game_15)
votes15
m15 = max(votes15$n) 
pred[15] = votes15$`game_15`[which(votes15$n == m15)]

# Game16
votes16 = count(games, game_16)
votes16
m16 = max(votes16$n) 
pred[16] = votes16$`game_16`[which(votes16$n == m16)]
```

```{r preds, include=FALSE}
votes_for = NULL 
Percentage_For = NULL
votes_for[1] = votes1$n[which(votes1$`game_1` == winners[1])]
Percentage_For[1] = round(votes_for[1]/sum(votes1$n),4)
votes_for[2] = votes2$n[which(votes2$`game_2` == winners[2])]
Percentage_For[2] = round(votes_for[2]/sum(votes2$n),4)
votes_for[3] = votes3$n[which(votes3$`game_3` == winners[3])]
Percentage_For[3] = round(votes_for[3]/sum(votes3$n),4)
votes_for[4] = votes4$n[which(votes4$`game_4` == winners[4])]
Percentage_For[4] = round(votes_for[4]/sum(votes4$n),4)
votes_for[5] = votes5$n[which(votes5$`game_5` == winners[5])]
Percentage_For[5] = round(votes_for[5]/sum(votes5$n),4)
votes_for[6] = votes6$n[which(votes6$`game_6` == winners[6])]
Percentage_For[6] = round(votes_for[6]/sum(votes6$n),4)
votes_for[7] = votes7$n[which(votes7$`game_7` == winners[7])]
Percentage_For[7] = round(votes_for[7]/sum(votes7$n),4)
votes_for[8] = votes8$n[which(votes8$`game_8` == winners[8])]
Percentage_For[8] = round(votes_for[8]/sum(votes8$n),4)
votes_for[9] = votes9$n[which(votes9$`game_9` == winners[9])]
Percentage_For[9] = round(votes_for[9]/sum(votes9$n),4)
votes_for[10] = votes10$n[which(votes10$`game_10` == winners[10])]
Percentage_For[10] = round(votes_for[10]/sum(votes10$n),4)
votes_for[11] = votes11$n[which(votes11$`game_11` == winners[11])]
Percentage_For[11] = round(votes_for[11]/sum(votes11$n),4)
votes_for[12] = votes12$n[which(votes12$`game_12` == winners[12])]
Percentage_For[12] = round(votes_for[12]/sum(votes12$n),4)
votes_for[13] = votes13$n[which(votes13$`game_13` == winners[13])]
Percentage_For[13] = round(votes_for[13]/sum(votes13$n),4)
votes_for[14] = votes14$n[which(votes14$`game_14` == winners[14])]
Percentage_For[14] = round(votes_for[14]/sum(votes14$n),4)
votes_for[15] = votes15$n[which(votes15$`game_15` == winners[15])]
Percentage_For[15] = round(votes_for[15]/sum(votes15$n),4)
votes_for[16] = votes16$n[which(votes16$`game_16` == winners[16])]
Percentage_For[16] = round(votes_for[16]/sum(votes16$n),4)

```

## Group Predictions 

```{r predictions, echo=FALSE}
# Creating the Group results table 
results = tibble(Games = 1:length(games)) %>% 
  add_column(Prediction = pred)


# for (i in 1:length(games)){
#   g = as.name(game_names[i])
#   votes = count(games, `g`)
#   m = max(votes$n) 
#   pred[i] = votes$`game_1`[which(votes$n == m)]
#   }

```


```{r results, echo=FALSE}
# Adding the actual winners and checking if they match predictions
results =  results %>%
  add_column(Winners = winners) %>%
  mutate(Correct = ifelse(Prediction == Winners, "Yes", "No")) %>%
  add_column(votes_for) %>%
  add_column(Percentage_For) %>%
  rename("Correct Votes" = votes_for) %>%
  rename("Correct Percent" = Percentage_For)
results
# Formating chart for color
improvement_formatter2 <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x == "Yes", "green", ifelse(x =="No", "red", "black"))))


# Creatign Group Prediction table            
formattable(results, 
            align =c("c","c","c"), 
            list(
              `Indicator Name` = formatter("span", 
                                           style = ~ style(color = "grey",font.weight = "bold")),
              "Correct" = improvement_formatter2))

# Printing the weekly and season win percentage            
correct_count = count(results, Correct)
weekly_win_percentage = correct_count$n[which(correct_count$Correct == "Yes")]/n
paste("Our win percentage for the week is", weekly_win_percentage, sep = " ")
season_win_percentage = (correct_count$n[which(correct_count$Correct == "Yes")]+season_wins)/n+season_games
paste("Our season win percentage so far is", season_win_percentage, sep = " ")
Total = sum(votes1$n)
paste(Total, "predictions this week", sep = " ")
```

## Individual Predictions

```{r individual, echo=FALSE}
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
  add_column(WK1_Correct_Picks = indiv_correct) %>%
  mutate(WK1_Percentage = WK1_Correct_Picks/n) %>%
  arrange(-WK1_Percentage)

# choosing just name # correct pics and correct percentage
indiv_disp = indiv %>%
  select(name, WK1_Correct_Picks, WK1_Percentage)


#formating chart for stars 
improvement_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > .5, "green", ifelse(x < .5, "red", "black"))),
            x ~ icontext(ifelse(x == max(x), "star", ""), x))

# Creating individual results table
Week_1_indiv = formattable(indiv_disp,
                           align =c("l","c","c"), 
                           list(
                             `Indicator Name` = formatter("span", 
                                                          style = ~ style(color = "grey",font.weight = "bold")),
                             "WK1_Percentage" = improvement_formatter
                             
                           ))

# Pringing the individual table 
Week_1_indiv
# data %>% janitor::clean_names() %>%
#   select(starts_with("game")) %>% apply(,2,tally)
#   ?count()
# 
# 
# ?select()
```