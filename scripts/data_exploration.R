
library(tidyverse)

# Cargar el dataset original
spotify <- read_csv("data/raw/dataset.csv")

# ==========================================
# 1. EXPLORACIÓN INICIAL
# ==========================================

# Mostrar las dimensiones del dataset
dim(spotify)

# Mostrar los nombres de las columnas
colnames(spotify)

# Mostrar las primeras filas
head(spotify)

# Obtener un resumen estadístico
summary(spotify)


# ==========================================
# 2. VALORES AUSENTES
# ==========================================

# Contar los valores ausentes de cada columna
colSums(is.na(spotify))

# Mostrar las filas que contienen valores ausentes
spotify %>%
  filter(if_any(everything(), is.na))


# ==========================================
# 3. ANÁLISIS DE DUPLICADOS
# ==========================================

# Preparar los datos eliminando valores ausentes y duplicados exactos
spotify_clean <- spotify %>%
  drop_na() %>%
  select(-`...1`) %>%
  distinct()

# Contar las apariciones de cada canción
duplicated_tracks <- spotify_clean %>%
  count(track_id, sort = TRUE) %>%
  filter(n > 1)

# Contar las canciones que aparecen repetidas
nrow(duplicated_tracks)

# Mostrar los registros de la canción más repetida
spotify_clean %>%
  filter(track_id == duplicated_tracks$track_id[1]) %>%
  select(track_name, artists, popularity, track_genre)

# Contar las canciones con diferentes valores de popularidad
spotify_clean %>%
  group_by(track_id) %>%
  summarise(
    different_popularity = n_distinct(popularity),
    .groups = "drop"
  ) %>%
  filter(different_popularity > 1) %>%
  nrow()


# ==========================================
# 4. CONSISTENCIA DE LOS DATOS
# ==========================================

# Seleccionar las características musicales
audio_features <- c(
  "duration_ms", "explicit", "danceability",
  "energy", "key", "loudness", "mode",
  "speechiness", "acousticness",
  "instrumentalness", "liveness",
  "valence", "tempo", "time_signature"
)

# Comprobar si las características cambian entre registros de una misma canción
spotify_clean %>%
  group_by(track_id) %>%
  summarise(
    across(all_of(audio_features), n_distinct),
    .groups = "drop"
  ) %>%
  summarise(
    across(all_of(audio_features), ~ sum(.x > 1))
  )

# Comprobar si los nombres, artistas y álbumes son consistentes
spotify_clean %>%
  group_by(track_id) %>%
  summarise(
    artists = n_distinct(artists),
    album_name = n_distinct(album_name),
    track_name = n_distinct(track_name),
    .groups = "drop"
  ) %>%
  summarise(
    artists = sum(artists > 1),
    album_name = sum(album_name > 1),
    track_name = sum(track_name > 1)
  )


# ==========================================
# 5. ANÁLISIS DE VALORES ANÓMALOS
# ==========================================

# Unificar las canciones para analizar los valores anómalos
spotify_clean <- spotify_clean %>%
  group_by(track_id) %>%
  summarise(
    across(-c(popularity, track_genre), first),
    popularity = median(popularity),
    track_genre = paste(sort(unique(track_genre)), collapse = "; "),
    .groups = "drop"
  )

# Contar canciones con duraciones poco habituales
spotify_clean %>%
  summarise(
    zero_duration = sum(duration_ms == 0),
    short_tracks = sum(duration_ms < 30000),
    long_tracks = sum(duration_ms > 1200000)
  )

# Contar canciones con tempo igual a cero
sum(spotify_clean$tempo == 0)

# Contar canciones con compás igual a cero
sum(spotify_clean$time_signature == 0)

# Analizar la coincidencia de tempo y compás iguales a cero
spotify_clean %>%
  summarise(
    both_zero = sum(tempo == 0 & time_signature == 0),
    only_tempo_zero = sum(tempo == 0 & time_signature != 0),
    only_signature_zero = sum(tempo != 0 & time_signature == 0)
  )


# ==========================================
# 6. VALIDACIÓN FINAL
# ==========================================

# Comprobar las dimensiones del dataset
dim(spotify_clean)

# Comprobar que no quedan valores ausentes
sum(is.na(spotify_clean))

# Comprobar que no quedan identificadores duplicados
sum(duplicated(spotify_clean$track_id))

# Comprobar el rango de popularidad
range(spotify_clean$popularity)

# Comprobar que no hay duraciones inválidas
sum(spotify_clean$duration_ms <= 0)

# Seleccionar las variables con valores entre 0 y 1
normalized_features <- c(
  "danceability", "energy", "speechiness",
  "acousticness", "instrumentalness",
  "liveness", "valence"
)

# Comprobar que las variables cumplen sus rangos esperados
spotify_clean %>%
  summarise(
    across(
      all_of(normalized_features),
      ~ sum(.x < 0 | .x > 1)
    )
  )
