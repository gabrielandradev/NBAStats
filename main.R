# hoopR y sus dependencias / Datos de la NBA
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load_current_gh("sportsdataverse/hoopR", dependencies = TRUE, update = TRUE)

install.packages("dplyr")
library(dplyr)

# ReadR  / Procesamiento de documentos csv
install.packages("vroom")
install.packages("readr")
library(readr)

# Obtenemos los datos de cada partido de los jugadores en el periodo especiricado
player_data <- load_nba_player_box(2023:2024)

# Leemos los datos de drtg del mismo periodo
defensive_ratings_by_team <- read_csv("defensive_ratings.csv")

# Calculamos la media de minutos de las temporadas
player_avg_minutes <- player_data %>%
  group_by(athlete_display_name) %>%
  summarise(avg_minutes = mean(minutes, na.rm = TRUE))

# Unimos la media de minutos con los datos de jugadores
training_data <- player_data %>%
  left_join(player_avg_minutes, by = "athlete_display_name")

# Unimos el drtg del equipo rival a los datos de jugadores
training_data <- training_data %>%
  left_join(defensive_ratings_by_team, by = join_by(opponent_team_abbreviation == team_abbreviation))

# Convertimos los vectores a factores de categorias ej: c1 = ('regular', 'playoffs')
training_data$home_away <- as.factor(training_data$home_away)
training_data$season_type <- as.factor(training_data$season_type)

# Creamos los modelos de regresion lineal para las variables definidas
assists_model <- lm(assists ~ home_away + season_type + avg_minutes + drtg,
  data = training_data
)
offensive_rebounds_model <- lm(offensive_rebounds ~ home_away + season_type + avg_minutes + drtg,
  data = training_data
)
points_model <- lm(points ~ home_away + season_type + avg_minutes + drtg,
  data = training_data
)

get_opponent_team_abbreviation <- function(full_name) {
  result <- training_data %>%
    filter(team_name == full_name) %>%
    pull(opponent_team_abbreviation)

  if (length(result) > 0) {
    return(result[1])
  } else {
    warning(paste("El equipo '", full_name, "' no se encontro en la tabla.", sep = ""))
    return(NA_character_)
  }
}

predict_game_stats <- function(player_name, game_location, game_type, opponent_team) {
  opponent_team_abbreviation <- get_opponent_team_abbreviation(opponent_team)

  opponent_drtg <- defensive_ratings_by_team %>%
    filter(team_abbreviation == opponent_team_abbreviation) %>%
    pull(drtg)

  # Predecimos basados en los juegos anteriores con las mismas caracteristicas
  player_games <- training_data %>%
    filter(
      athlete_display_name == player_name,
      home_away == game_location,
      season_type == game_type
    ) %>%
    unique()

  if (nrow(player_games) == 0) {
    stop("No se encuentran datos previos del jugador.")
  }

  # Calculamos la media de minutos esperada en base a los datos anteriores
  expected_avg_minutes <- player_games %>%
    summarise(
      avg_minutes = mean(avg_minutes, na.rm = TRUE)
    ) %>%
    pull(avg_minutes)

  # El jugador debe haber sumado minutos en juegos de esas caracteristicas para predecir
  if (length(expected_avg_minutes) == 0 || is.na(expected_avg_minutes)) {
    stop("Error: El jugador no registra minutos en juegos anteriores de esas caracteristicas.")
  }

  new_game_data <- data.frame(
    home_away = game_location,
    season_type = as.factor(game_type),
    avg_minutes = expected_avg_minutes,
    drtg = opponent_drtg
  )

  predicted_assists <- predict(assists_model, new_game_data)
  predicted_offensive_rebounds <- predict(offensive_rebounds_model, new_game_data)
  predicted_points <- predict(points_model, new_game_data)

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

# Ejemplo de uso
predict_game_stats(
  player_name = "LeBron James",
  game_location = game_home,
  game_type = playoffs_game,
  opponent_team = "Heat"
)
