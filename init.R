install.packages("devtools")
install.packages("remotes")

# https://github.com/statsbomb/StatsBombR
devtools::install_github("statsbomb/SDMTools")

# install StatsBombR  
devtools::install_github("statsbomb/StatsBombR")

# install soccermatics for plotting
devtools::install_github("jogall/soccermatics")

library(soccermatics)
library(StatsBombR)
library(dplyr)
library(tidyr)
library(nnet)
library(ggplot2)
library(GGally)
library(reshape2)
library(scales)
library(car)
library(broom)

# load in data from Statsbomb
all_competitions <- FreeCompetitions()

fawsl_2021 <- 
  FreeCompetitions() %>%
  filter(
    competition_id == 37,
    season_id == 90
  )

fawsl_2021_matches <- FreeMatches(fawsl_2021)
fawsl_2021_data <- free_allevents(MatchesDF = fawsl_2021_matches, Parallel = T)
fawsl_2021_data <- allclean(fawsl_2021_data)

# join fawsl_2021_data with fawsl_2021_matches on match_id

fawsl_2021_data_matches <-
  left_join(fawsl_2021_data, fawsl_2021_matches, by = "match_id")

# we will be using fawsl_training_data to develop each team's home and away profile 
# for all team names both away and home concentenate string "_home" / "_away" after each team name 

fawsl_2021_data_matches$home_team.home_team_id <-
  paste(fawsl_2021_data_matches$home_team.home_team_id , "_home", sep ="")

fawsl_2021_data_matches$away_team.away_team_id <- 
  paste(fawsl_2021_data_matches$away_team.away_team_id, "_away", sep ="")

# need to also create a new column for team.name from fawsl_2021_data
# team.name dictates the name of the team associated with the action
# need to know if the team.name doing the action is a home team or an away team

## create new column team.name_home_away, if team.name=home_team.home_team_name then paste, _home, otherwise, _away
fawsl_2021_data_matches <-
  fawsl_2021_data_matches %>%
  mutate(
    team.name_home_away =
      ifelse(team.name==home_team.home_team_name, paste(team.name, "_home", sep = ""),
             paste(team.name, "_away", sep = ""))
  )

# create a variable called opposition team (separated by home/away)
## to create opposition team variable,
## compare team.name and columns home_team.home_team_name and away_team.away_team_name
## if team.name = home_team.home_team_name then, opposition = away_team.away_team_name 
## else home_team.home_team_name

fawsl_2021_data_matches <-
  fawsl_2021_data_matches %>%
  mutate(
    opposition_team_name =
      ifelse(team.name==home_team.home_team_name, paste(away_team.away_team_name, "_away", sep=""),
             paste(home_team.home_team_name, "_home", sep = ""))
  )