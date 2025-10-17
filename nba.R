load_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

predict_for_player <- function(player, data) {
  if (!(player %in% names(points_models))) {
    return(NA)
  }

  points_model <- points_models[[player]]
  assists_model <- assists_models[[player]]
  off_rebounds_model <- off_rebounds_models[[player]]

  predicted_points <- predict(points_model, data)
  predicted_assists <- predict(assists_model, data)
  predicted_off_rebounds <- predict(off_rebounds_model, data)

  return(list(
    points = predicted_points,
    assists = predicted_assists,
    off_rebounds = predicted_off_rebounds
  ))
}

calculate_mae <- function(actual, predicted) {
  return(mean(abs(actual - predicted)))
}

calculate_k <- function(stats) {
  return(sd(stats))
}

calculate_accuracy <- function(actual, predicted, k) {
  mae <- calculate_mae(actual, predicted)
  return(1 / (1 + mae / k))
}

load_package("dplyr")
load_package("hoopR")

all_data <- load_nba_player_box(2024:2025)

team_games_2023_2024 <- as.data.frame(team_game_logs <- nba_teamgamelogs(
  last_n_games = 0,
  league_id = "00",
  measure_type = "Advanced",
  month = 0,
  opponent_team_id = 0,
  po_round = 0,
  pace_adjust = "N",
  per_mode = "Totals",
  period = 0,
  plus_minus = "N",
  rank = "N",
  season = "2023-24",
  season_type = "",
  team_id = ""
)[[1]])

team_games_2024_2025 <- as.data.frame(team_game_logs <- nba_teamgamelogs(
  last_n_games = 0,
  league_id = "00",
  measure_type = "Advanced",
  month = 0,
  opponent_team_id = 0,
  po_round = 0,
  pace_adjust = "N",
  per_mode = "Totals",
  period = 0,
  plus_minus = "N",
  rank = "N",
  season = "2024-25",
  season_type = "",
  team_id = ""
)[[1]])

all_team_games <- rbind(team_games_2023_2024, team_games_2024_2025)

all_team_games <- transform(all_team_games, GAME_DATE = as.Date(GAME_DATE))

players_to_predict <- 60
players_sorted_by_pie <- read.csv("players_sorted_by_pie.csv", nrows = players_to_predict)

all_data <- all_data %>%
  inner_join(players_sorted_by_pie, by = c("athlete_display_name" = "PLAYER"))

all_data <- all_data %>% filter(minutes > 0)

all_data <- all_data %>% filter(team_name != "All-Stars" &
  team_name != "Team Shaq" &
  team_name != "Team Chuck" &
  team_name != "Team Candace" &
  team_name != "Team Kenny")

all_data <- all_data %>%
  inner_join(all_team_games, by = c("opponent_team_abbreviation" = "TEAM_ABBREVIATION", "game_date" = "GAME_DATE")) %>%
  rename("opponent_defensive_rating" = "DEF_RATING")


all_data$home_away <- factor(all_data$home_away, levels = c("home", "away"))
all_data$season_type <- factor(all_data$season_type,
  levels = c(2, 3, 5),
  labels = c("regular", "playoffs", "play_in")
)

relevant_cols <- c(
  "season_type", "home_away", "points", "assists",
  "offensive_rebounds", "opponent_defensive_rating"
)

for (col_name in relevant_cols) {
  cat("NA values in ", col_name, ": ", sum(is.na(all_data[[col_name]])), "\n",
    sep = ""
  )
}

all_data$opponent_defensive_rating <- as.numeric(as.character(all_data$opponent_defensive_rating))


training_percentage <- 0.7
players <- unique(all_data$athlete_display_name)
points_models <- list()
assists_models <- list()
off_rebounds_models <- list()

for (player in players) {
  cat("Training model for ", player, "\n", sep = "")
  player_data <- all_data %>% filter(athlete_display_name == player)
  training_indices <- 1:(nrow(player_data) * training_percentage)
  training_data <- player_data[training_indices, ]
  testing_data <- player_data[-training_indices, ]

  if (length(unique(training_data$home_away)) < 2 |
    length(unique(training_data$season_type)) < 2 |
    !all(testing_data$season_type %in% training_data$season_type)) {
    next
  }

  points_model <- lm(points ~ home_away + season_type + opponent_defensive_rating,
    data = training_data
  )
  assists_model <- lm(
    assists ~ home_away + season_type
      + opponent_defensive_rating,
    data = training_data
  )
  off_rebounds_model <- lm(
    offensive_rebounds ~ home_away + season_type
      + opponent_defensive_rating,
    data = training_data
  )

  points_models[[player]] <- points_model
  assists_models[[player]] <- assists_model
  off_rebounds_models[[player]] <- off_rebounds_model
}

points_accuracy <- 0
assists_accuracy <- 0
off_rebounds_accuracy <- 0

for (player in players) {
  player_data <- all_data %>% filter(athlete_display_name == player)
  training_indices <- 1:(nrow(player_data) * training_percentage)
  testing_data <- player_data[-training_indices, ]

  prediction <- predict_for_player(player, testing_data)
  if (!is.list(prediction)) {
    next
  }

  cat("Predicted stats for ", player, "\n", sep = "")

  points_k <- calculate_k(all_data$points)
  assists_k <- calculate_k(all_data$assists)
  off_rebounds_k <- calculate_k(all_data$offensive_rebounds)

  actual_points <- testing_data$points
  actual_assists <- testing_data$assists
  actual_off_rebounds <- testing_data$offensive_rebounds

  points_accuracy <- points_accuracy + calculate_accuracy(
    actual_points,
    prediction$points,
    points_k
  )
  assists_accuracy <- assists_accuracy + calculate_accuracy(
    actual_assists,
    prediction$assists,
    assists_k
  )
  off_rebounds_accuracy <- off_rebounds_accuracy + calculate_accuracy(
    actual_off_rebounds,
    prediction$off_rebounds,
    off_rebounds_k
  )
}

points_accuracy <- points_accuracy / length(players)
assists_accuracy <- assists_accuracy / length(players)
off_rebounds_accuracy <- off_rebounds_accuracy / length(players)

global_accuracy <- (points_accuracy + assists_accuracy + off_rebounds_accuracy) / 3

cat("Points accuracy: ", points_accuracy, "\n", sep = "")
cat("Assists accuracy: ", assists_accuracy, "\n", sep = "")
cat("Offensive_rebounds accuracy: ", off_rebounds_accuracy, "\n", sep = "")
cat("Global accuracy: ", global_accuracy, "\n", sep = "")
