#            ________
#    o      |   __   |
#      \_ O |  |__|  |
#   ____/ \ |___WW___|
#   __/   /     ||
#               ||
#               ||
# _______________||________________

# Carga de paquetes necesarios para el proyecto

load_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

load_package("dplyr")
load_package("hoopR")
load_package("ggplot2")

# Funciones auxiliares

calculate_mae <- function(actual, predicted) {
  return(mean(abs(actual - predicted)))
}

calculate_accuracy <- function(actual, predicted, k) {
  mae <- calculate_mae(actual, predicted)
  return(1 / (1 + mae / k))
}

all_data_clean <- read.csv("data_clean_test.csv", header = TRUE)
players <- read.csv("players_test.csv", header = TRUE)
player_models <- readRDS("player_models.rds")

k_coefficients <- read.csv("k_coefficients.csv", header = TRUE)
points_k <- k_coefficients$points
assists_k <- k_coefficients$assists
off_rebounds_k <- k_coefficients$off_rebounds

for (i in 1:nrow(players)) {
  row <- players[i, ]
  player <- row[[1]]
  opponent_team_name <- row[[2]]
  
  testing_data <- all_data_clean %>%
    filter(athlete_display_name == player) %>%
    slice_head()
  
  points_model <- player_models[[player]]$points
  assists_model <- player_models[[player]]$assists
  off_rebounds_model <- player_models[[player]]$off_rebounds

  prediction <- data.frame(
    points = predict(points_model, newdata = testing_data),
    assists = predict(assists_model, newdata = testing_data),
    off_rebounds = predict(off_rebounds_model, newdata = testing_data)
  )

  prediction[prediction < 0] <- 0

  actual_points <- testing_data$points
  actual_assists <- testing_data$assists
  actual_off_rebounds <- testing_data$offensive_rebounds

  points_accuracy <- calculate_accuracy(
    actual_points,
    prediction$points,
    points_k
  )
  assists_accuracy <- calculate_accuracy(
    actual_assists,
    prediction$assists,
    assists_k
  )
  off_rebounds_accuracy <- calculate_accuracy(
    actual_off_rebounds,
    prediction$off_rebounds,
    off_rebounds_k
  )
  
  cat("------------------------------------------------------\n")
  cat(sprintf("Jugador: %-20s\n", player))
  cat(sprintf("Equipo Rival: %-20s\n", opponent_team_name))
  cat("------------------------------------------------------\n")
  cat("Estadística    | Predicho  | Real   | Exactitud (%)\n")
  
  cat(sprintf("%-14s | %-9.2f | %-6.2f | %-6.2f\n",
              "Puntos",
              prediction$points,
              actual_points,
              points_accuracy))
  
  cat(sprintf("%-14s | %-9.2f | %-6.2f | %-6.2f\n",
              "Asistencias",
              prediction$assists,
              actual_assists,
              assists_accuracy))
  
  cat(sprintf("%-14s | %-9.2f | %-6.2f | %-6.2f\n",
              "Reb. Ofensivos",
              prediction$off_rebounds,
              actual_off_rebounds,
              off_rebounds_accuracy))
  cat("\n")
}