load_package = function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

calculate_mae = function(actual, predicted) {
  return (mean(abs(actual - predicted)))
}

calculate_k = function(stats) {
  return (sd(stats))
}

calculate_accuracy = function(actual, predicted, k) {
  mae = calculate_mae(actual, predicted)
  return (1 / (1 + mae / k))
}

load_package("dplyr")
load_package("hoopR")

all_data = load_nba_player_box(2024:2025)
def_rating_data = read.csv("defensive_rating.csv")

all_data = all_data %>% filter(minutes > 0)

all_data = all_data %>% filter(team_name != "All-Stars"
                               & team_name != "Team Shaq"
                               & team_name != "Team Chuck"
                               & team_name != "Team Candace"
                               & team_name != "Team Kenny")

# TODO: Use average of 2023-2025
all_data = all_data %>%
  left_join(def_rating_data, by = c("opponent_team_display_name" = "Team")) %>%
  rename(opponent_defensive_rating = DRtg.A)

all_data$home_away = factor(all_data$home_away, levels = c("home", "away"))
all_data$season_type = factor(all_data$season_type, levels = c(2, 3, 5),
                              labels = c("regular", "playoffs", "play_in"))

relevant_cols = c("season_type", "home_away", "points", "assists",
                 "offensive_rebounds", "opponent_defensive_rating")
for (col_name in relevant_cols) {
  cat("NA values in ", col_name, ": ", sum(is.na(all_data[[col_name]])), "\n",
      sep = "")
}

training_indices = 1:(nrow(all_data) * 0.7)
training_data = all_data[training_indices, ]
testing_data = all_data[-training_indices, ]

points_model = lm(points ~ home_away + season_type + opponent_defensive_rating,
               data = training_data)
assists_model = lm(assists ~ home_away + season_type
                   + opponent_defensive_rating,
               data = training_data)
off_rebounds_model = lm(offensive_rebounds ~ home_away + season_type
                        + opponent_defensive_rating,
               data = training_data)

actual_points = testing_data$points
predicted_points = predict(points_model, testing_data)
actual_assists = testing_data$assists
predicted_assists = predict(assists_model, testing_data)
actual_off_rebounds = testing_data$offensive_rebounds
predicted_off_rebounds = predict(off_rebounds_model, testing_data)

points_k = calculate_k(all_data$points)
assists_k = calculate_k(all_data$assists)
off_rebounds_k = calculate_k(all_data$offensive_rebounds)

points_accuracy = calculate_accuracy(actual_points, predicted_points,
                                     points_k)
assists_accuracy = calculate_accuracy(actual_assists, predicted_assists,
                                      assists_k)
off_rebounds_accuracy = calculate_accuracy(actual_off_rebounds,
                                           predicted_off_rebounds,
                                           off_rebounds_k)
global_accuracy = (points_accuracy + assists_accuracy + off_rebounds_accuracy) / 3

cat("Points accuracy: ", points_accuracy, "\n", sep = "")
cat("Assists accuracy: ", assists_accuracy, "\n", sep = "")
cat("Offensive_rebounds accuracy: ", off_rebounds_accuracy, "\n", sep = "")
cat("Global accuracy: ", global_accuracy, "\n", sep = "")