
source("00_requirements.R")

# This function is the main function for the Random Forest model. It takes in a year, and then predicts the match count on said year.
# Additionally, it calculates absolute accuracy statistics and also creates a plot of its error(s) in predicting.
count_match_prediction <- function(year) {
  library(randomForest)
  library(dplyr)
  
  # Load datasets
  wc_elo <- read.csv("data/primary_dataset_wc_elo.csv")
  
  # Convert columns to factors and/or numeric
  wc_elo <- wc_elo %>%
    mutate(
      world_cup_year = as.factor(world_cup_year),
      team_name = as.factor(team_name),
      elo_rating = as.numeric(elo_rating),
      elo_rank = as.numeric(elo_rank),
      elo_1yr_change_rating = as.numeric(elo_1yr_change_rating),
      win_ratio = as.numeric(win_ratio),
      goals_ratio = as.numeric(goals_ratio),
      count_matches = as.numeric(count_matches)
    )
  
  # Split into training and test sets (this is to prevent data leakage)
  train_data <- wc_elo %>% filter(world_cup_year != year)
  test_data  <- wc_elo %>% filter(world_cup_year == year)
  
  
  # train Random Forest model
  rf_model <- randomForest(count_matches ~ elo_rating + win_ratio + goals_ratio + elo_rank, data = train_data)
  
  test_data$predicted_count_matches <- round(predict(rf_model, newdata = test_data)) # make predictions on year
  test_data$error <- test_data$predicted_count_matches - test_data$count_matches # Compute error
  
  # compute accuracy metrics
  actual <- test_data$count_matches
  pred   <- test_data$predicted_count_matches
  accuracy_df <- data.frame(
    RMSE = sqrt(mean((pred - actual)^2)),
    MAE  = mean(abs(pred - actual)),
    MAPE = mean(abs((pred - actual) / actual)) * 100
  )
  
  # plot the accuracy for per team
  barplot(
    test_data$error,
    names.arg = test_data$team_name,
    las = 2,
    col = "skyblue",
    main = paste("Prediction Error per Team -", year),
    ylab = "Error = Predicted - Actual"
  )
  abline(h = 0, col = "red", lwd = 2)
  
  # Return predictions and accuracy as a list, so that we have access to both
  return(list(predictions = test_data, accuracy = accuracy_df))
}