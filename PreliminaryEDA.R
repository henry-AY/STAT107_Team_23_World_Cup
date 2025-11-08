### Scatterokit - Goals scored versus Points Earned

plot_goals_vs_points <- function(data) {
  # Verify specific columns are present
  if(!all(c("goals_for", "points") %in% colnames(data))) {
    stop("Data must contain 'goals_for' and 'points' columns.")
  }
  
  # Plot the points with a slight jitter to show overlapping points
  plot(jitter(data$goals_for),
       jitter(data$points),
       xlab = "Goals Scored",
       ylab = "Points Earned",
       main = "Relationship between goals scored and points",
       pch = 1)
  
  model <- lm(points ~ goals_for, data = data)
  abline(model, col="blue")
  
  return(model) # Optional return if we want to use it for anything else
}

### Histogram - Points Distribution: Advanced vs. Not Advanced 
plot_advanced_vs_notadvanced <- function(data) {
  # Verify specific columns are present
  if(!all(c("points", "advanced") %in% colnames(data))) {
    stop("Data must contain 'points' and 'advanced' columns.")
  }
  
  hist(data$points[data$advanced == 1], 
       col = rgb(0, 1, 0, 0.5), # Semi-transparent color
       main = "Points Distribution: Advanced vs Non-Advanced",
       xlab = "Points", 
       xlim = c(0, max(data$points)),
       ylim = c(0, max(table(data$points))) # dynamic x & y axis
  )
  
  hist(data$points[data$advanced == 0], 
       col = rgb(1, 0, 0, 0.5), # semi-transparent color/overlay
       add = TRUE
  )
}


### Scatterplot - Goals For vs. Goals Against 
plot_goals_for_against <- function(data) {
  # Verify specific columns are present
  plot(data$goals_for, data$goals_against,
       col = ifelse(data$advanced == TRUE, "green", "red"), # color by team advancement
       pch = 19,
       main = "Goals For vs Goals Against",
       xlab = "Goals For",
       ylab = "Goals Against")
  abline(lm(goals_against ~ goals_for, data = data), 
         col = "blue")
}

run_logistic_regression <- function(data) {
  # Verify required columns are present
  required_cols <- c("played", "wins", "draws", "losses", 
                     "goals_for", "goals_against", "goal_difference", 
                     "points", "advanced")
  if(!all(required_cols %in% colnames(data))) {
    stop("Data must contain all required columns for logistic regression.")
  }
  
  data <- data %>%
    mutate(across(c(played, wins, draws, losses, goals_for, 
                    goals_against, goal_difference, points, advanced), as.factor))
  
  # Logistic Regression Model 1
  model1 <- glm(advanced ~ played + wins + draws + losses + 
                  goals_for + goal_difference + goals_against + points, 
                data = data, family = binomial)
  
  # Perform backward stepwise selection based on AIC
  reduced_model1 <- step(model1, direction = "backward")
  
  # Logistic Regression Model 2
  model2 <- glm(advanced ~ goals_for + goal_difference + goals_against + points,
                data = data, family = binomial)
  
  # Perform backward stepwise selection based on AIC
  reduced_model2 <- step(model2, direction = "backward")
  
  # Return all models as a list
  return(list(
    full_model1 = model1,
    reduced_model1 = reduced_model1,
    full_model2 = model2,
    reduced_model2 = reduced_model2
  ))
}