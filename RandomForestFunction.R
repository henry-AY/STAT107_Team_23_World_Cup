
count_match_prediction <- function(year) {
  
  library(randomForest)
  rf_model <- randomForest(count_matches ~ elo_rating + win_ratio + goals_ratio + elo_rank, data = wc_elo)
  
  
  wc_year_prediction <- subset(wc_elo, world_cup_year == year)
  
  wc_year_prediction <- wc_year_prediction[, c("team_name", "win_ratio", "goals_ratio", "elo_rank", "elo_rating", "count_matches")]
  
  
  predicted_elo_year <- predict(rf_model, newdata = wc_year_prediction)
  
  wc_year_prediction$predicted_count_matches <- round(predicted_elo_year)
  
  wc_year_prediction$error <- wc_year_prediction$predicted_count_matches - wc_year_prediction$count_matches
  
  ## calculate the model accuracy in a data frame
  actual <- wc_2018_prediction$count_matches
  pred <- wc_2018_prediction$predicted_count_matches
  
  RMSE <- sqrt(mean((pred - actual)^2))
  
  MAE <- mean(abs(pred - actual))
  
  MAPE <- mean(abs((pred - actual) / actual)) * 100
  
  accuracy_df <- data.frame(
    RMSE = RMSE,
    MAE = MAE,
    MAPE = MAPE
  )
  print(accuracy_df)
  ## end of accuracy calculations 
  
  ## graph the model for better visualization 
  barplot(
    wc_year_prediction$error,
    names.arg = wc_year_prediction$team_name,
    las = 2,
    col = "skyblue",
    main = "Prediction Error per Team",
    ylab = "Error = Predicted - Actual"
  )
  abline(h = 0, col = "red", lwd = 2)
  
  return (head(wc_year_prediction))
}



