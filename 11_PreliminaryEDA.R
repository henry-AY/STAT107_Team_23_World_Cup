
## Preliminary EDA 


### Relationship between goals scored and points
plot(jitter(df_group_standings_clean$goals_for),
     jitter(df_group_standings_clean$points),
     xlab = "Goals Scored",
     ylab = "Points Earned",
     main = "Relationship between goals scored and points",
     pch = 1)

model <- lm(points ~ goals_for, data = df_group_standings_clean)
abline(model, col="blue")



### Histogram - Points Distribution: Advanced vs. Not Advanced 
hist(df_group_standings$points[df_group_standings$advanced == 1], 
     col = rgb(0, 1, 0, 0.5), 
     main = "Points Distribution: Advanced vs Non-Advanced",
     xlab = "Points", 
     xlim = c(0, max(df_group_standings$points)),
     ylim = c(0, max(table(df_group_standings$points)))
)
hist(df_group_standings$points[df_group_standings$advanced == 0], 
     col = rgb(1, 0, 0, 0.5),  # red with transparency
     add = TRUE
)


### Scatterplot - Goals For vs. Goals Against 
goals_for_against <- plot(df_group_standings$goals_for, df_group_standings$goals_against,
                          col = ifelse(df_group_standings$advanced == TRUE, "green", "red"),
                          pch = 19,
                          main = "Goals For vs Goals Against",
                          xlab = "Goals For",
                          ylab = "Goals Against")
abline(lm(goals_against ~ goals_for, data = df_group_standings), 
       col = "blue")


### Logistic Regression (Best prediction factors)
played <- as.factor(df_group_standings$played)
wins <- as.factor(df_group_standings$wins)
draws <- as.factor(df_group_standings$draws)
losses <- as.factor(df_group_standings$losses)
goals_for <- as.factor(df_group_standings$goals_for)
goals_against <- as.factor(df_group_standings$goals_against)
goal_difference <- as.factor(df_group_standings$goal_difference)
points <- as.factor(df_group_standings$points)
advanced <- as.factor(df_group_standings$advanced)

group_standing_model1 <- glm(advanced ~ played + wins + draws + losses + goals_for + goal_difference + goals_against + points, data = df_group_standings)
summary(group_standing_model1)

reduced_group_standing_model1 <- step(group_standing_model1, direction = "backward")



### Logistic Regression 2
group_standing_model2 <- glm(advanced ~ goals_for + goal_difference + goals_against + points, data = df_group_standings)
summary(group_standing_model2)

reduced_group_standing_model2 <- step(group_standing_model2, direction = "backward")





