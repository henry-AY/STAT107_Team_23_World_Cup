source("00_requirements.R")

# This function plots the predicted versus actual from the Random Forest model. It is a scatterplot, and plots each prediction
# on a 2D plot where one axis is the predicted value, and the other is the actual value. Additionally, it creates a red line that represents
# the perfect prediction line.
plot_pred_vs_actual <- function(results_df, actual_col = "count_matches", pred_col = "predicted_count_matches") {
  p <- ggplot(results_df, aes(x = .data[[actual_col]], y = .data[[pred_col]])) +
    # Jitter points so that we can see all of them
    geom_jitter(width = 0.2, height = 0.2, alpha = 0.6, color = "#2E8B57") + 
    
    # perfect prediction line (y = x)
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
    
    theme_minimal() +
    labs(
      title = "Random Forest Predicted vs. Actual Match Counts",
      subtitle = "Points closer to the red line represent more accurate model predictions",
      x = "Actual Matches Played",
      y = "Predicted Matches Played"
    ) +
    coord_fixed()
  
  return(p)
}