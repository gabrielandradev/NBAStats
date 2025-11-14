#            ________
#    o      |   __   |
#      \_ O |  |__|  |
#   ____/ \ |___WW___|
#   __/   /     ||
#               ||
#               ||
#_______________||________________



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

calculate_k <- function(stats) {
  return(sd(stats))
}

calculate_accuracy <- function(actual, predicted, k) {
  mae <- calculate_mae(actual, predicted)
  return(1 / (1 + mae / k))
}

# Carga y limpieza de datos de las temporadas 2023-2024
# Fuentes: HoopR, NBA

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

# Filtro: Todos los jugadores deben tener al menos un minuto de juego
all_data <- all_data %>% filter(minutes > 0)

# Filtro: Excluimos los partidos amistosos especiales
all_data <- all_data %>% filter(team_name != "All-Stars" &
  team_name != "Team Shaq" &
  team_name != "Team Chuck" &
  team_name != "Team Candace" &
  team_name != "Team Kenny")

# Ordenamos los datos por fecha de juego ascendentemente
all_data <- all_data %>% arrange(game_date_time)

all_data <- all_data %>%
  inner_join(players_sorted_by_pie, by = c("athlete_display_name" = "PLAYER"))

all_data <- all_data %>%
  inner_join(all_team_games, by = c("opponent_team_abbreviation" = "TEAM_ABBREVIATION", "game_date" = "GAME_DATE")) %>%
  rename("opponent_defensive_rating" = "E_DEF_RATING")

all_data$season_type <- replace(all_data$season_type, all_data$season_type == 5, 3)

all_data$home_away <- factor(all_data$home_away, levels = c("home", "away"))

all_data$season_type <- factor(all_data$season_type,
  levels = c(2, 3, 5),
  labels = c("regular", "playoffs", "play_in")
)

relevant_cols <- c(
  "season_type", "home_away", "points", "assists",
  "offensive_rebounds", "opponent_defensive_rating"
)

# Omitimos las filas donde hayan valores nulos
for (col_name in relevant_cols) {
  cat("NA values in ", col_name, ": ", sum(is.na(all_data[[col_name]])), "\n",
    sep = ""
  )
}

all_data$opponent_defensive_rating <- as.numeric(all_data$opponent_defensive_rating)

all_data$minutes <- as.numeric(all_data$minutes)

all_data <- all_data %>% distinct()

# Seleccionamos las variables a utilizar en el modelo
model_vars <- c(
  "athlete_display_name", "game_date", "game_date_time", "points", "assists", "offensive_rebounds",
  "home_away", "season_type", "USG.", "opponent_defensive_rating",
  "minutes"
)

all_data_clean <- all_data %>%
  select(all_of(model_vars)) %>%
  na.omit()

# Utilizaremos 70% de las observaciones para entrenar el modelo
# y el 30% restante para la evaluacion del mismo
training_percentage <- 0.7
players <- unique(all_data_clean$athlete_display_name)

# Contamos el numero de jugadores de los que se pudieron extraer y procesar
# datos para obtener el promedio de los valores agregados mas adelante
valid_players_count <- 0

points_accuracy <- 0
assists_accuracy <- 0
off_rebounds_accuracy <- 0

points_mae <- 0
assists_mae <- 0
off_rebounds_mae <- 0

# Definimos un peso de 365 dias a las observaciones.
# De esta manera, los partidos mas antiguos tienen menor peso sobre
# la prediccion del rendimiento actual del jugador
weight_factor <- 365

total_predicted_values <- all_data %>%
  slice(0)

total_actual_values <- all_data %>%
  slice(0)

aggregated_results_list <- list()

# Iniciamos el calculo de tiempo de ejecucion del sistema
start.time <- Sys.time()

points_k <- calculate_k(all_data$points)
assists_k <- calculate_k(all_data$assists)
off_rebounds_k <- calculate_k(all_data$offensive_rebounds)

cat(points_k, "\n")
cat(assists_k, "\n")
cat(off_rebounds_k, "\n")

# Ahora queremos ajustar un modelo por jugador. Para ello obtendremos las
# observaciones correspondientes a cada uno y nos aseguraremos de que cumplan
# los requisitos: haber jugado al menos un partido de playoffs y al menos
# 20 partidos en general
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

  regular_data <- player_data[player_data$season_type == "regular", ]
  playoffs_data <- player_data[player_data$season_type == "playoffs", ]

  if (nrow(playoffs_data) == 0) {
    cat(player, " no registra partidos de playoffs")
    next
  }

  regular_training_indices <- 1:floor(nrow(regular_data) * training_percentage)
  playoffs_training_indices <- 1:floor(nrow(playoffs_data) * training_percentage)

  training_data <- rbind(
    regular_data[regular_training_indices, ],
    playoffs_data[playoffs_training_indices, ]
  )

  testing_data <- rbind(
    regular_data[-regular_training_indices, ],
    playoffs_data[-playoffs_training_indices, ]
  )

  if (nrow(player_data) < 20) {
    cat("Skipping ", player, ": Insufficient complete games after cleaning.\n", sep = "")
    next
  }

  cat("Training model for ", player, "\n", sep = "")

  # Para cada modelo de prediccion definimos las variables a utilizar:
  # 1. Etapa de la temporada
  # 2. Condicion de localia
  # 3. Defensive Rating del equipo oponente
  # Ademas, asignamos los pesos correspondientes

  points_model <- lm(
    points ~ season_type + home_away + opponent_defensive_rating,
    data = training_data,
    weights = training_data$weight,
    na.action = na.omit
  )

  assists_model <- lm(
    assists ~ season_type + home_away + opponent_defensive_rating,
    data = training_data,
    weights = training_data$weight,
    na.action = na.omit
  )

  off_rebounds_model <- lm(
    offensive_rebounds ~ season_type + home_away + opponent_defensive_rating,
    data = training_data,
    weights = training_data$weight,
    na.action = na.omit
  )

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

  # Calculamos el Error Medio Absoluto para cada observacion
  points_mae <- points_mae + calculate_mae(actual_points, prediction$points)
  assists_mae <- assists_mae + calculate_mae(actual_assists, prediction$assists)
  off_rebounds_mae <- off_rebounds_mae + calculate_mae(actual_off_rebounds, prediction$off_rebounds)

  # Agregamos las exactitudes registradas para el modelo del jugador
  # A la suma actual de las exactitudes previas
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
  # Tomamos el promedio de los errores y las exactitudes registradas
  # segun el numero de jugadores evaluados
  points_mae <- points_mae / valid_players_count
  assists_mae <- assists_mae / valid_players_count
  off_rebounds_mae <- off_rebounds_mae / valid_players_count

  cat("MAE de puntos: ", points_mae, "\n")
  cat("MAE de asistencias: ", assists_mae, "\n")
  cat("MAE de rebotes ofensivos: ", off_rebounds_mae, "\n")


  # Calculamos las exactitudes finales del modelo
  points_accuracy <- points_accuracy / valid_players_count
  assists_accuracy <- assists_accuracy / valid_players_count
  off_rebounds_accuracy <- off_rebounds_accuracy / valid_players_count

  global_accuracy <- (points_accuracy + assists_accuracy + off_rebounds_accuracy) / 3

  # Mostramos los resultados finales
  cat("--- Resultados finales ---\n")
  cat("Se modelaron ", valid_players_count, " de ", length(players), " jugadores.\n", sep = "")
  cat("Eficiencia de puntos: ", points_accuracy, "\n", sep = "")
  cat("Eficiencia de asistencias: ", assists_accuracy, "\n", sep = "")
  cat("Eficiencia de rebotes ofensivos ", off_rebounds_accuracy, "\n", sep = "")
  cat("Eficiencia global: ", global_accuracy, "\n", sep = "")
} else {
  cat("No se pudo establecer modelos para ninguno de los jugadores.\n")
}

# Si se pudieron registrar resultados, elaboramos los graficos correspondientes
if (length(aggregated_results_list) > 0) {
  aggregated_results <- dplyr::bind_rows(aggregated_results_list)

  print(head(aggregated_results))

  cat("\nGenerando graficos...\n")

  points_plot <- ggplot(aggregated_results, aes(x = points, y = predicted_points)) +
    geom_point(alpha = 0.3, color = "blue") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = "Puntos Predichos vs Reales (Todos los jugadores)",
      x = "Puntos reales",
      y = "Puntos predichos",
      caption = "Línea roja es y = x (predicción perfecta)"
    ) +
    theme_minimal()

  print(points_plot)

  assists_plot <- ggplot(aggregated_results, aes(x = assists, y = predicted_assists)) +
    geom_point(alpha = 0.3, color = "darkgreen") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = "Asistencias Predichas vs Reales (Todos los jugadores)",
      x = "Asistencias reales",
      y = "Asistencias predichas",
      caption = "Línea roja es y = x (predicción perfecta)"
    ) +
    theme_minimal()

  print(assists_plot)


  off_reb_plot <- ggplot(aggregated_results, aes(x = offensive_rebounds, y = predicted_off_rebounds)) +
    geom_point(alpha = 0.3, color = "purple") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = "Rebotes Ofensivos Predichos vs Reales (Todos los jugadores)",
      x = "Rebotes ofensivos reales",
      y = "Rebotes ofensivos predichos",
      caption = "Línea roja es y = x (predicción perfecta)"
    ) +
    theme_minimal()

  print(off_reb_plot)
} else {
  cat("\nNo se registraron resultados para graficar.\n")
}

# Finalizamos el calculo de tiempo de ejecucion del sistema
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
