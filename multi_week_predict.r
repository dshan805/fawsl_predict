prediction_df <- data.frame()
new_df <- data.frame()

multi_week_predict <-
  function(num_weeks) {
    for (i in num_weeks:max(fawsl_2021_matches$match_week)-1){
      new_df <- match_predictor(i)[[1]]
      
      prediction_df <- rbind(prediction_df, new_df)
    }
    prediction_df <-
      prediction_df %>%
      mutate(correct_prediction = actual_result==predicted_result)
    prediction_perc <- mean(prediction_df$correct_prediction)*100
    message <- paste("The model correctly predicted", prediction_perc, "% of the matches.")
    cat(message)
    
    return(prediction_df)
  }