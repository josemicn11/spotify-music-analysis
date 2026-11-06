
library(tidyverse)

# Cargar el dataset original
spotify <- read_csv("data/raw/dataset.csv")

# Eliminar valores ausentes, columna innecesaria y duplicados exactos
spotify_clean <- spotify %>%
  drop_na() %>%
  select(-`...1`) %>%
  distinct()

# Unificar las canciones repetidas
spotify_clean <- spotify_clean %>%
  group_by(track_id) %>%
  summarise(
    across(-c(popularity, track_genre), first),
    popularity = median(popularity),
    track_genre = paste(sort(unique(track_genre)), collapse = "; "),
    .groups = "drop"
  )

# Guardar el dataset limpio
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(spotify_clean, "data/processed/spotify_clean.csv")


