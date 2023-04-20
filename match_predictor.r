match_predictor <-
  function(num_week) {
    fawsl_2021_data_matches <-
      fawsl_2021_data_matches %>%
      filter(match_week <= num_week+1)
    
    fawsl_2021_matches <-
      fawsl_2021_matches %>%
      mutate(
        home_team.home_team_name = paste(home_team.home_team_name, "_home", sep=""),
        away_team.away_team_name = paste(away_team.away_team_name, "_away", sep="")
      ) %>%
      filter(match_week <= num_week+1)
    
    # creating xg, xg Assit for teams for training data
    # xg by team
    xg_by_team =
      fawsl_2021_data_matches %>%
      filter(type.name=="Shot") %>%
      filter(shot.type.name!="Penalty"|is.na(shot.type.name)) %>%
      group_by(team.name_home_away) %>%
      summarise(xG=sum(shot.statsbomb_xg, na.rm = TRUE))
    
    num_match_training_data=
      fawsl_2021_data_matches %>%
      group_by(team.name_home_away)%>%
      summarise(num_match=n_distinct(match_id))
    
    xg_by_team =
      left_join(xg_by_team, num_match_training_data, by = "team.name_home_away") %>%
      mutate(xg_per_game = xG/num_match)
    
    #xg assist by team
    xGA =
      fawsl_2021_data_matches %>%
      filter(type.name=="Shot") %>%
      select(shot.key_pass_id, xGA = shot.statsbomb_xg)
    
    shot_assists =
      left_join(fawsl_2021_data_matches, xGA, by = c("id" = "shot.key_pass_id"))%>%
      select(team.name_home_away, type.name, pass.shot_assist, pass.goal_assist, xGA ) %>%
      filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE)
    
    xGA_by_team =
      shot_assists %>%
      group_by(team.name_home_away) %>%
      summarise(xGA = sum(xGA, na.rm = TRUE))
    
    xg_xga_by_team =
      left_join(xg_by_team, xGA_by_team, by = "team.name_home_away") %>%
      mutate(xga_per_game = xGA/num_match)
        
    # need to create xg and xga Against by team
    # xg against by team
    xg_against_by_team =
      fawsl_2021_data_matches %>%
      filter(type.name=="Shot") %>%
      filter(shot.type.name!="Penalty"|is.na(shot.type.name)) %>%
      group_by(opposition_team_name) %>%
      summarise(xG_against=sum(shot.statsbomb_xg, na.rm = TRUE))
    
    num_match_training_data_opposition=
      fawsl_2021_data_matches %>%
      group_by(opposition_team_name)%>%
      summarise(num_match=n_distinct(match_id))
    
    xg_against_by_team =
      left_join(xg_against_by_team, num_match_training_data_opposition, by = "opposition_team_name") %>%
      mutate(xg_against_per_game = xG_against/num_match)
    
    #xg assist against by team
    xGA_against =
      fawsl_2021_data_matches %>%
      filter(type.name=="Shot") %>%
      select(shot.key_pass_id, xGA_against = shot.statsbomb_xg)
    
    shot_assists_against =
      left_join(fawsl_2021_data_matches, xGA_against, by = c("id" = "shot.key_pass_id"))%>%
      select(opposition_team_name, type.name, pass.shot_assist, pass.goal_assist, xGA_against) %>%
      filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE)
    
    xGA_against_by_team =
      shot_assists_against %>%
      group_by(opposition_team_name) %>%
      summarise(xGA_against = sum(xGA_against, na.rm = TRUE))
    
    xg_xga_against_by_team =
      left_join(xg_against_by_team, xGA_against_by_team, by = "opposition_team_name") %>%
      mutate(xga_against_per_game = xGA_against/num_match)
        
    # combine xg_xga_by_team and xg_xga_against_by_team
    team_profile_tmp <- left_join(xg_xga_by_team, xg_xga_against_by_team,
                                  by=c("team.name_home_away"="opposition_team_name"))
    
    # average number of goals per game to determine conversion rate of xg to goals
    shots_goals <-
      fawsl_2021_data_matches %>%
      group_by(team.name_home_away) %>%
      summarise(
        shots=sum(type.name=="Shot",na.rm=TRUE)/n_distinct(match_id),
        goals=sum(shot.outcome.name=="Goal",na.rm=TRUE)/n_distinct(match_id)
      )
    
    # join shots and goals to team_profile_tmp
    test_tmp <-
      left_join(team_profile_tmp, shots_goals)
    team_profile_tmp <- test_tmp
    
    # also need shots, goals per game given up
    shots_goals_against <-
      fawsl_2021_data_matches %>%
      group_by(opposition_team_name) %>%
      summarise(
        shots_against=sum(type.name=="Shot",na.rm=TRUE)/n_distinct(match_id),
        goals_against=sum(shot.outcome.name=="Goal",na.rm=TRUE)/n_distinct(match_id)
      )
    
    test_tmp <-
      left_join(team_profile_tmp, shots_goals_against,
                by=c("team.name_home_away"="opposition_team_name"))
    
    team_profile_tmp <- test_tmp
    test_tmp <- 
      test_tmp %>%
      mutate(
        xg_goal_conversion = xg_per_game/goals,
        xg_goal_against_conversion = xg_against_per_game/goals_against
      )
    
    # we want to only look at from defensive perspective
    defensive_error_total <-
      fawsl_2021_data_matches %>%
      filter(type.name=="Dispossessed") %>%
      mutate(pitch_part = ifelse(location.x <=40, "Defensive Third",
                                 ifelse(location.x <=80, "Middle", "Final Third")))
    
    defensive_error_by_team <-
      defensive_error_total %>%
      filter(pitch_part=="Defensive Third") %>%
      group_by(team.name_home_away) %>%
      summarise(
        dispossed_count = n_distinct(id),
        dispossed_count_per_match = n_distinct(id)/n_distinct(match_id)
      )
    
    # join defensive error_by_team to team_profile_tmp
    
    test_tmp <-
      left_join(test_tmp, defensive_error_by_team,
                by=c("team.name_home_away"="team.name_home_away"))
    
    team_profile_tmp <- test_tmp
    
    duel_by_team <-
      fawsl_2021_data_matches %>%
      filter(type.name =="Duel" & !is.na(duel.outcome.name)) %>%
      group_by(team.name_home_away) %>%
      summarise(
        total_duel = n_distinct(id),
        num_duel_won = sum(duel.outcome.name=="Won"|
                             duel.outcome.name=="Success In Play"|duel.outcome.name=="Success Out"),
        duel_won_perc = num_duel_won/total_duel
      )
    
    # join duel by team to team_profile_tmp
    test_tmp <- left_join(
      test_tmp, duel_by_team, by=c("team.name_home_away"="team.name_home_away")
    )
    
    team_profile_tmp <- test_tmp
    
    # passes specifically forward passing
    # location.x/location.y is start of the pass
    # pass.end_location.x/pass.end_location.y is the end of the pass
    # for forward passes, measure the difference between end and start location and if > 0 forward
    # is.na(pass.outcome.name) will filter on completed passes
    # rest is incomplete pass
    # also plot forward pass
    
    fawsl_2021_data_matches %>%
      filter(type.name=="Pass")
    
    # first get total num of passes per team
    total_num_pass <-
      fawsl_2021_data_matches %>%
      filter(type.name =="Pass") %>%
      group_by(team.name_home_away) %>%
      summarise(
        total_num_pass = n_distinct(id)
      )
    
    total_compl_pass <-
      fawsl_2021_data_matches %>%
      filter(type.name=="Pass" & is.na(pass.outcome.name)) %>%
      group_by(team.name_home_away) %>%
      summarise(
        total_compl_pass = n_distinct(id)
      )
    
    total_num_pass <- left_join(
      total_num_pass, total_compl_pass, by=c("team.name_home_away"="team.name_home_away")
    ) %>%
      mutate(total_pass_compl_perc = total_compl_pass/total_num_pass)
        
    # combine to test_tmp
    test_tmp <- left_join(
      test_tmp, total_num_pass, by=c("team.name_home_away"="team.name_home_away")
    )
    
    # get forward passing
    # reason to get a better understanding of carrying ball forward. from midfield area
    # location.x>40
    
    # first want to understand the distribution of forward pass distance
    x_movement_pass_by_team_attempt <-
      fawsl_2021_data_matches %>%
      filter(
        type.name=="Pass"
      ) %>%
      mutate(pitch_part = ifelse(location.x <=40, "Defensive Third",
                                 ifelse(location.x <=80, "Middle", "Final Third"))) %>%
      group_by(team.name_home_away, pitch_part) %>%
      summarise(
        total_num_pass = n_distinct(id),
        median_distance_x = median(pass.end_location.x - location.x),
        mean_distance_x = mean(pass.end_location.x-location.x),
        sd_distance_x = sd(pass.end_location.x-location.x)
      ) %>%
      pivot_wider(
        names_from = "pitch_part",
        values_from = c(total_num_pass, median_distance_x, mean_distance_x, sd_distance_x)
      ) %>%
      select(
        -c("total_num_pass_Defensive Third", "total_num_pass_Final Third", "total_num_pass_Middle")
      )
    
    names(x_movement_pass_by_team_attempt)
    
    x_movement_pass_by_team_success <-
      fawsl_2021_data_matches %>%
      filter(
        type.name=="Pass" & is.na(pass.outcome.name)
      ) %>%
      mutate(pitch_part = ifelse(location.x <=40, "Defensive Third",
                                 ifelse(location.x <=80, "Middle", "Final Third"))) %>%
      group_by(team.name_home_away, pitch_part) %>%
      summarise(
        comp_num_pass = n_distinct(id),
        compl_median_distance_x = median(pass.end_location.x - location.x),
        compl_mean_distance_x = mean(pass.end_location.x-location.x),
        compl_sd_distance_x = sd(pass.end_location.x-location.x)
      ) %>%
      pivot_wider(
        names_from = "pitch_part",
        values_from =
          c(comp_num_pass, compl_median_distance_x, compl_mean_distance_x, compl_sd_distance_x)
      )
    
    # then understand actual distance of pass
    real_pass_dist <-
      fawsl_2021_data_matches %>%
      filter(
        type.name=="Pass"
      ) %>%
      mutate(pitch_part = ifelse(location.x <=40, "Defensive Third",
                                 ifelse(location.x <=80, "Middle", "Final Third"))) %>%
      group_by(team.name_home_away, pitch_part) %>%
      summarise(
        total_num_pass = n_distinct(id),
        median_distance = median(sqrt((pass.end_location.x-location.x)^2+(pass.end_location.y-location.y)^2)),
        mean_distance = mean(sqrt((pass.end_location.x-location.x)^2+(pass.end_location.y-location.y)^2)),
        sd_distance = sd(sqrt((pass.end_location.x-location.x)^2+(pass.end_location.y-location.y)^2))
      ) %>%
      pivot_wider(
        names_from = "pitch_part",
        values_from =
          c(total_num_pass, median_distance, mean_distance, sd_distance)
      )
    
    pass_compl_by_team_area <-
      fawsl_2021_data_matches %>%
      filter(
        type.name=="Pass" & is.na(pass.outcome.name)
      ) %>%
      mutate(pitch_part = ifelse(location.x <=40, "Defensive Third",
                                 ifelse(location.x <=80, "Middle", "Final Third"))) %>%
      group_by(team.name_home_away, pitch_part) %>%
      summarise(
        compl_num_pass = n_distinct(id)
      ) %>%
      pivot_wider(
        names_from = "pitch_part",
        values_from = "compl_num_pass"
      ) %>%
      rename(
        "defensive_third_pass_compl" = "Defensive Third",
        "middle_third_pass_compl" = "Middle",
        "final_third_pass_compl" = "Final Third"
      )
    
    # join pass_compl_by_team_area on real distance and also join to x_movement_pass_by_team_attempt
    
    pass_compl_by_team_area <-
      left_join(
        pass_compl_by_team_area, real_pass_dist, by=c("team.name_home_away"="team.name_home_away")
      ) %>%
      mutate(
        defensive_third_pass_compl_perc = defensive_third_pass_compl/`total_num_pass_Defensive Third`,
        middle_third_pass_compl_perc = middle_third_pass_compl/total_num_pass_Middle,
        final_third_pass_compl_perc = final_third_pass_compl/`total_num_pass_Final Third`
      )
    
    # join x_movement_pass_by_team_attempt
    pass_compl_by_team_area <-
      left_join(pass_compl_by_team_area, x_movement_pass_by_team_attempt,
                by=c("team.name_home_away"="team.name_home_away"))
    
    # join pass_compl_by_team_are to test_tmp
    test_tmp <-
      left_join(test_tmp, pass_compl_by_team_area,
                by=c("team.name_home_away"="team.name_home_away"))
    team_profile_tmp <- test_tmp
    
    #### count of total num passes tried from middle of the pitch
    total_forward_pass <-
      fawsl_2021_data_matches %>%
      filter(
        type.name=="Pass",
        location.x > 40,
        pass.end_location.x - location.x > 0 #ball moved forward
      ) %>%
      group_by(team.name_home_away) %>%
      summarise(total_forward_pass = n_distinct(id))
    
    total_forward_pass_plt <-
      fawsl_2021_data_matches %>%
      filter(
        type.name=="Pass",
        location.x > 40,
        pass.end_location.x - location.x >10 #ball moved forward
      )
    
    # team_form
   
    # create a list of vectors to store team results
    home_team <- unique(fawsl_2021_matches$home_team.home_team_name)
    away_team <- unique(fawsl_2021_matches$away_team.away_team_name)
    all_team <- as.list(sort(c(home_team, away_team)))
    
    all_team_result <- list()
    
    # loop through each row (match)
    for (i in 1:nrow(fawsl_2021_matches)) {
      home_team <- fawsl_2021_matches[i, "home_team.home_team_name"]
      away_team <- fawsl_2021_matches[i, "away_team.away_team_name"]
      home_score <- fawsl_2021_matches[i, "home_score"]
      away_score <- fawsl_2021_matches[i, "away_score"]
      
      if (home_score > away_score) {
        all_team_result[[paste0("'", home_team, "'")]] <-
          c(all_team_result[[paste0("'", home_team, "'")]],3)
        all_team_result[[paste0("'", away_team, "'")]] <-
          c(all_team_result[[paste0("'", away_team, "'")]],0)
      } else if (home_score < away_score) {
        all_team_result[[paste0("'", home_team, "'")]] <-
          c(all_team_result[[paste0("'", home_team, "'")]],0)
        all_team_result[[paste0("'", away_team, "'")]] <-
          c(all_team_result[[paste0("'", away_team, "'")]],3)
      } else {
        all_team_result[[paste0("'", home_team, "'")]] <-
          c(all_team_result[[paste0("'", home_team, "'")]],1)
        all_team_result[[paste0("'", away_team, "'")]] <-
          c(all_team_result[[paste0("'", away_team, "'")]],1)
      }
    }
    
    last_five_results <- data.frame(teamname = character(),
                                    last_five_sum = numeric(),
                                    stringsAsFactors = FALSE)
    
    for (i in seq_along(all_team_result)) {
      teamname <- names(all_team_result)[i]
      last_five <- tail(all_team_result[[i]], n = 5)
      last_five_sum <- sum(last_five)
      last_five_results[i, ] <- c(teamname, last_five_sum)
    }
    
    last_five_results$teamname <- gsub("'", "", last_five_results$teamname)
    
    # join to test_tmp
    
    test_tmp <- left_join(
      test_tmp, last_five_results, by=c("team.name_home_away"="teamname")
    )
    
    test_temp <- as.numeric(test_tmp$last_five_sum)

    # create training data frame
    # take home team, away team, home score, away score, result(based on home team)
    # left join team profile on to home and away team name
    # train logistic regression model
    # test on week+1 results
    
    training_data <-
      fawsl_2021_matches %>%
      select(
        home_team.home_team_name,
        away_team.away_team_name,
        home_score,
        away_score,
        match_week
      ) %>%
      mutate(result = ifelse(home_score>away_score, "win",
                             ifelse(home_score<away_score, "loss", "draw"))
      )
    
    # add home behind all column names after join
    
    training_data <- left_join(training_data, test_tmp,
                               by=c("home_team.home_team_name"="team.name_home_away"))
    
    names(training_data)[6:ncol(training_data)] <-
      paste0(names(training_data)[6:ncol(training_data)], "_home", sep="")
    
    training_data <- left_join(training_data, test_tmp,
                               by=c("away_team.away_team_name"="team.name_home_away"))
    
    names(training_data)[59:ncol(training_data)] <-
      paste0(names(training_data)[59:ncol(training_data)], "_away", sep="")
    
    # logistic regression model
    
    set.seed(42)
    
    # select specific columns need for testing only
    training_data <-
      training_data %>%
      select(
        result_home, xg_per_game_home, xg_against_per_game_home, dispossed_count_per_match_home,
        duel_won_perc_home, last_five_sum_home, xg_goal_conversion_home, 
        xg_goal_against_conversion_home, `mean_distance_Defensive Third_home`, 
        `mean_distance_Final Third_home`, mean_distance_Middle_home,
        xg_per_game_away, xg_against_per_game_away, dispossed_count_per_match_away,
        duel_won_perc_away, last_five_sum_away, xg_goal_conversion_away, 
        xg_goal_against_conversion_away, `mean_distance_Defensive Third_away`, 
        `mean_distance_Final Third_away`, mean_distance_Middle_away,
        match_week
      )
    
    training_data <- training_data %>% 
      mutate(last_five_sum_home = as.numeric(last_five_sum_home),
             last_five_sum_away = as.numeric(last_five_sum_away))
    
    train_data <- training_data %>% filter(match_week <= num_week) %>% select(-match_week)
    test_data <- training_data %>% filter(match_week == num_week+1) %>% select(-match_week)
    train_data$last_five_sum_home <- as.numeric(train_data$last_five_sum_home)
    test_data$last_five_sum_home <- as.numeric(test_data$last_five_sum_home)
    
    model <- multinom(result_home ~ ., data=train_data)
    pred <- predict(model, newdata = test_data)
    
    accuracy <- sum(pred == test_data$result_home)/nrow(test_data)
    print(paste0("Accuracy:", round(accuracy*100,2),"%"))
    
    predictions_df <- data.frame(actual_result = test_data$result_home,
                                 predicted_result = pred)
    summary_model <- tidy(model)
    
    return(list(predictions_df, test_tmp, summary_model, training_data))
    
  }
