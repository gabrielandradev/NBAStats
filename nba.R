#            ________
#    o      |   __   |
#      \_ O |  |__|  |
#   ____/ \ |___WW___|
#   __/   /     ||
#               ||
#               ||
#_______________||________________



load_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
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

players_sorted_by_pie <- read.csv("players_sorted_by_pie.csv")

all_data <- load_nba_player_box(2023:2025)

all_data <- all_data %>% filter(minutes > 0)

all_data <- all_data %>% filter(team_name != "All-Stars" &
  team_name != "Team Shaq" &
  team_name != "Team Chuck" &
  team_name != "Team Candace" &
  team_name != "Team Kenny")

all_data <- all_data %>% arrange(game_date_time)

all_data <- all_data %>%
  inner_join(players_sorted_by_pie, by = c("athlete_display_name" = "PLAYER"))

all_data <- all_data %>%
  inner_join(all_team_games, by = c("opponent_team_abbreviation" = "TEAM_ABBREVIATION", "game_date" = "GAME_DATE")) %>%
  rename("opponent_defensive_rating" = "E_DEF_RATING")


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

all_data$opponent_defensive_rating <- as.numeric(all_data$opponent_defensive_rating)

all_data$minutes <- as.numeric(all_data$minutes)

all_data <- all_data %>% distinct()

model_vars <- c(
  "athlete_display_name", "game_date", "game_date_time", "points", "assists", "offensive_rebounds",
  "home_away", "season_type", "USG.", "opponent_defensive_rating",
  "minutes"
)

all_data_clean <- all_data %>%
  select(all_of(model_vars)) %>%
  na.omit()


training_percentage <- 0.7
players <- unique(all_data_clean$athlete_display_name)

valid_players_count <- 0

points_accuracy <- 0
assists_accuracy <- 0
off_rebounds_accuracy <- 0

weight_factor <- 365

total_predicted_values <- all_data %>%
  slice(0)

total_actual_values <- all_data %>%
  slice(0)

aggregated_results_list <- list()

for (player in players) {
  player_data <- all_data_clean %>%
    filter(athlete_display_name == player) %>%
    arrange(game_date_time)

  player_data <- player_data %>%
    arrange(game_date) %>%
    mutate(
      days_ago = as.numeric(max(game_date) - game_date),
      weight = exp(-days_ago / weight_factor)
    )

  training_indices <- 1:floor(nrow(player_data) * training_percentage)
  testing_indices <- (floor(nrow(player_data) * training_percentage) + 1):nrow(player_data)

  training_data <- player_data[training_indices, ]
  testing_data <- player_data[testing_indices, ]

  cat("Training model for ", player, "\n", sep = "")

  if (nrow(player_data) < 20) {
    cat("Skipping ", player, ": Insufficient complete games after cleaning.\n", sep = "")
    next
  }

  if (length(testing_indices) == 0) {
    cat("Skipping ", player, ": No testing data after split.\n", sep = "")
    next
  }

  cat("Training model for ", player, "\n", sep = "")

  points_model <- lm(
    points ~ home_away + opponent_defensive_rating + minutes,
    data = training_data,
    weights = training_data$weight,
    na.action = na.omit
  )

  assists_model <- lm(
    assists ~ home_away + opponent_defensive_rating + minutes,
    data = training_data,
    weights = training_data$weight,
    na.action = na.omit
  )

  off_rebounds_model <- lm(
    offensive_rebounds ~ home_away + opponent_defensive_rating + minutes,
    data = training_data,
    weights = training_data$weight,
    na.action = na.omit
  )

  points_k <- calculate_k(training_data$points)
  assists_k <- calculate_k(training_data$assists)
  off_rebounds_k <- calculate_k(training_data$offensive_rebounds)

  prediction <- data.frame(
    points = predict(points_model, newdata = testing_data),
    assists = predict(assists_model, newdata = testing_data),
    off_rebounds = predict(off_rebounds_model, newdata = testing_data)
  )

  prediction[prediction < 0] <- 0

  actual_points <- testing_data$points
  actual_assists <- testing_data$assists
  actual_off_rebounds <- testing_data$offensive_rebounds

  player_results <- testing_data %>%
    mutate(
      predicted_points = prediction$points,
      predicted_assists = prediction$assists,
      predicted_off_rebounds = prediction$off_rebounds
    )

  aggregated_results_list[[player]] <- player_results

  valid_players_count <- valid_players_count + 1

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

  print(points_accuracy)
  print(assists_accuracy)
  print(off_rebounds_accuracy)
}

if (valid_players_count > 0) {
  points_accuracy <- points_accuracy / valid_players_count
  assists_accuracy <- assists_accuracy / valid_players_count
  off_rebounds_accuracy <- off_rebounds_accuracy / valid_players_count

  global_accuracy <- (points_accuracy + assists_accuracy + off_rebounds_accuracy) / 3

  cat("--- Final Results ---\n")
  cat("Modeled ", valid_players_count, " out of ", length(players), " players.\n", sep = "")
  cat("Points accuracy: ", points_accuracy, "\n", sep = "")
  cat("Assists accuracy: ", assists_accuracy, "\n", sep = "")
  cat("Offensive_rebounds accuracy: ", off_rebounds_accuracy, "\n", sep = "")
  cat("Global accuracy: ", global_accuracy, "\n", sep = "")
} else {
  cat("No players were successfully modeled. Please check data and filters.\n")
}



if (length(aggregated_results_list) > 0) {
  # Combine all the individual player data frames from the list into one single data frame
  aggregated_results <- dplyr::bind_rows(aggregated_results_list)

  cat("\n--- Aggregated Results (Head) ---\n")
  cat("This data frame contains both actuals (e.g., 'points') and predictions (e.g., 'predicted_points').\n")
  print(head(aggregated_results))

  # Load ggplot2 for visualization
  load_package("ggplot2")

  cat("\nGenerating plots...\n")

  points_plot <- ggplot(aggregated_results, aes(x = points, y = predicted_points)) +
    geom_point(alpha = 0.3, color = "blue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
    labs(
      title = "Predicted vs. Actual Points (All Players)",
      x = "Actual Points",
      y = "Predicted Points",
      caption = "Red dashed line is y = x (perfect prediction)"
    ) +
    theme_minimal()

  print(points_plot)

  assists_plot <- ggplot(aggregated_results, aes(x = assists, y = predicted_assists)) +
    geom_point(alpha = 0.3, color = "darkgreen") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
    labs(
      title = "Predicted vs. Actual Assists (All Players)",
      x = "Actual Assists",
      y = "Predicted Assists",
      caption = "Red dashed line is y = x (perfect prediction)"
    ) +
    theme_minimal()

  print(assists_plot)


  off_reb_plot <- ggplot(aggregated_results, aes(x = offensive_rebounds, y = predicted_off_rebounds)) +
    geom_point(alpha = 0.3, color = "purple") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
    labs(
      title = "Predicted vs. Actual Offensive Rebounds (All Players)",
      x = "Actual Offensive Rebounds",
      y = "Predicted Offensive Rebounds",
      caption = "Red dashed line is y = x (perfect prediction)"
    ) +
    theme_minimal()

  print(off_reb_plot)
} else {
  cat("\nNo results were aggregated to graph.\n")
}
