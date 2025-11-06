## Reading in Dataset
df_group_standings <- read_csv("group_standings.csv", show_col_types = FALSE)
head(df_group_standings, 200)




## Data Cleaning / Processing 
df_group_standings_clean <- df_group_standings[, c("tournament_id", "team_name", "team_code", "played", "wins", "draws", "losses", "goals_for", "goals_against", "goal_difference", "points", "advanced")]

df_group_standings_clean <- mutate(
  df_group_standings_clean,
  across(where(is.character), trimws),
  across(c(played, wins, draws, losses, goals_for, goals_against, goal_difference, points, advanced), as.numeric)
)

df_group_standings_clean$year <- sub(".*(\\d{4}).*", "\\1", df_group_standings_clean$tournament_id)

head(df_group_standings_clean)








