load_package = function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

calculate_mae = function(actual, predicted) {
  return (mean(abs(actual - predicted), na.rm = TRUE))
}

calculate_accuracy = function(actual, predicted, k) {
  mae = calculate_mae(actual, predicted)
  return (1 / (1 + mae / k))
}

load_package("dplyr")
load_package("hoopR")

set.seed(123)

player_data = load_nba_player_box(2025)
team_data = read.csv("defensive_rating.csv")

player_data = player_data %>% filter(minutes > 0)

player_data = player_data %>%
  left_join(team_data, by = c("opponent_team_display_name" = "Team")) %>%
  rename(opponent_defensive_rating = DRtg.A)

player_data$home_away = factor(player_data$home_away, levels = c("home", "away"))
player_data$season_type = factor(player_data$season_type, levels = c(2, 3, 5),
                          labels = c("regular", "playoffs", "play_in"))

training_indices = sample(1:nrow(player_data), size = 0.8 * nrow(player_data))
training_data = player_data[training_indices, ]
testing_data = player_data[-training_indices, ]s

points_model = lm(points ~ home_away + season_type + opponent_defensive_rating,
                  data = training_data)
assists_model = lm(assists ~ home_away + season_type + opponent_defensive_rating,
                   data = training_data)
rebounds_model = lm(offensive_rebounds ~ home_away + season_type
                    + opponent_defensive_rating, data = training_data)

cat("Modelo 1: Predicción de puntos\n")
print(summary(points_model))
cat("Modelo 1: Predicción de asistencias\n")
print(summary(assists_model))
cat("Modelo 1: Predicción de rebotes ofensivos\n")
print(summary(rebounds_model))

actual_points = testing_data$points
predicted_points = predict(points_model, testing_data)
actual_assists = testing_data$assists
predicted_assists = predict(assists_model, testing_data)
actual_rebounds = testing_data$offensive_rebounds
predicted_rebounds = predict(rebounds_model, testing_data)

points_accuracy = calculate_accuracy(actual_points, predicted_points, 5)
assists_accuracy = calculate_accuracy(actual_assists, predicted_assists, 2)
rebounds_accuracy = calculate_accuracy(actual_rebounds, predicted_rebounds, 1)

global_accuracy = (points_accuracy + assists_accuracy + rebounds_accuracy) / 3

cat("Exactitud de puntos: ", points_accuracy, "\n")
cat("Exactitud de asistencias: ", assists_accuracy, "\n")
cat("Exactitud de rebotes: ", rebounds_accuracy, "\n")
cat("Exactitud global: ", global_accuracy, "\n")
 
# team_game_logs = nba_teamgamelogs(last_n_games = 0,
#                                   league_id = "00",
#                                   measure_type = "Base",
#                                   month = 0,
#                                   opponent_team_id = 0,
#                                   po_round = 0,
#                                   pace_adjust = "N",
#                                   per_mode = "Totals",
#                                   period = 0,
#                                   plus_minus = "N",
#                                   rank = "N",
#                                   season = "2025-26",
#                                   season_type = "Pre Season",
#                                   team_id = "")
