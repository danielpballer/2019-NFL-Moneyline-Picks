library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(formattable)
library(ggpubr)
library(ggrepel)
library(plyr)

week_1 = read_csv("NFL Week 1.csv")
week_2 = read_csv("NFL Week 2.csv")
wk1_winners = c("Green Bay Packers","Minnesota Vikings","Baltimore Ravens","Buffalo Bills","Kansas City Chiefs",
                "Los Angeles Rams","Tennessee Titans","Philadelphia Eagles","Seattle Seahawks","Los Angeles Chargers",
                "TIE","Dallas Cowboys","San Francisco 49ers","New England Patriots","New Orleans Saints","Oakland Raiders")

wk2_winners = c("Tampa Bay Buccaneers","Baltimore Ravens","Dallas Cowboys", "Indianapolis Colts","Seattle Seahawks",
                "Buffalo Bills","San Fransisco 49ers","Detroit Lions","Green Bay Packers","Houston Texans",
                "New England Patriots","Kansas City Chiefs","Los Angeles Rams","Chicago Bears","Atlanta Falcons",
                "Cleveland Browns")
# Calculating Week 1 Individual Results

picks = list(week_1,week_2)
winn = list(wk1_winners,wk2_winners)
week_num = list(1,2)
week_name = list("Week_1", "Week_2")
week_name = list(paste("Week_",week_num, sep = ""))

indiv_weekly_pred = function(x,y){
  indiv = x %>% janitor::clean_names() %>%
    select(name, starts_with("game")) 
  
  games_wk = indiv %>%
    select(-name)
  
  indiv_correct_wk = NULL
  help = NULL
  for (i in 1:length(indiv$name)){
    for(j in 1:length(games_wk)){
      help[j] = ifelse(games_wk[i,j]==y[j],1,0)
      indiv_correct_wk[i] = sum(help)
    }
  }
  season_wk = indiv %>%
    select(name) %>%
    add_column(week = indiv_correct_wk)
  return(season_wk)
}

paste("Week_", week_num, sep ="")

season = map2(picks, winn, indiv_weekly_pred)

season[[1]] %>%
  rename("paste("Week_",1, sep = "")" = Week)

week_name = function(x,y){
  for (i in 1:length(x)){
  x[[i]] %>%
    rename(paste("Week_",y[[i]], sep = "") = Week)
    }
}

for (i in 1:length(season)){
  season %>%
    rename(week_name[[1]][i] = season[[1]][2])
}

season[[1]] %>% rename(Week_1 = Week)

season %>%
  rename(week_name[[1]][1] = season[[1]][2])

length(season)
week_name[[1]][1]
