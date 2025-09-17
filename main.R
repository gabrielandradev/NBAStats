# Instalamos hoopR y sus dependencias (ver tutorial)
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

install.packages("dplyr")
library(dplyr)

pacman::p_load_current_gh("sportsdataverse/hoopR", dependencies = TRUE, update = TRUE)

player_data <- load_nba_player_box(2023:2024)

nba_defensehub(
  league_id = "00",
  game_scope = "Season",
  player_or_team = "Team",
  player_scope = "All Players",
  season = year_to_season(most_recent_nba_season() - 1),
  season_type = "Regular Season"
)

# Calculamos la media de minutos de las temporadas
player_avg_minutes <- player_data %>%
  group_by(athlete_display_name) %>%
  summarise(avg_minutes = mean(minutes, na.rm = TRUE))

# Unimos la media de minutos con los datos de jugadores
training_data <- player_data %>%
  left_join(player_avg_minutes, by = "athlete_display_name")

# Convertimos los vectores a factores de categorias ej: c1 = ('regular', 'playoffs')
training_data$home_away <- as.factor(training_data$home_away)
training_data$season_type <- as.factor(training_data$season_type)

# Creamos los modelos de regresion lineal para las variables definidas
assists_model <- lm(assists ~ home_away + season_type + avg_minutes,
  data = training_data
)
offensive_rebounds_model <- lm(offensive_rebounds ~ home_away + season_type + avg_minutes,
  data = training_data
)
points_model <- lm(points ~ home_away + season_type + avg_minutes,
  data = training_data
)

predict_game_stats <- function(player_name, game_location, game_type, opponent_team) {
  # Predecimos basados en los juegos anteriores con las mismas caracteristicas
  player_raw_games_stats <- training_data %>%
    filter(
      athlete_display_name == player_name,
      home_away == game_location,
      season_type == game_type
    ) %>%
    unique()

  # Si este juego nunca ocurrio antes, no realizamos la prediccion
  if (nrow(player_games) == 0) {
    stop("No se encuentran datos previos del jugador con esas caracteristicas.")
  }

  # Predecimos utilizando los modelos anteriores
  predicted_assists <- predict(assists_model, aggregated_player_stats)
  predicted_offensive_rebounds <- predict(offensive_rebounds_model, aggregated_player_stats)
  predicted_points <- predict(points_model, aggregated_player_stats)

  # Mostramos las predicciones
  cat("Estadisticas para ", player_name, "en un ", game_location, " game contra los ", opponent_team, " de la temporada:\n")
  cat("Assists:", round(predicted_assists, 2), "\n")
  cat("Offensive Rebounds:", round(predicted_offensive_rebounds, 2), "\n")
  cat("Points:", round(predicted_points, 2), "\n")
}

regular_season_game <- 2
playoffs_game <- 3

game_away <- "away"
game_home <- "home"

predict_game_stats(
  player_name = "Giannis Antetokounmpo",
  game_location = game_home,
  game_type = playoffs_game,
  opponent_team = "Heat"
)
