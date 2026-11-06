library(tidyverse)

# ==========================================
# 1. PREPARACION DE LOS DATOS
# ==========================================

# Carga el dataset limpio
spotify <- read_csv("data/processed/spotify_clean.csv", show_col_types = FALSE, progress = FALSE)


# Crea las carpetas para guardar los resultados
dir.create("results/popularity", recursive = TRUE, showWarnings = FALSE)
dir.create("results/genres", recursive = TRUE, showWarnings = FALSE)

# ==========================================
# 2. RESUMEN GENERAL DE POPULARIDAD
# ==========================================

resumen_popularidad <- spotify %>%
  summarise(
    n_canciones = n_distinct(track_id),
    media = mean(popularity),
    mediana = median(popularity),
    desviacion = sd(popularity),
    minimo = min(popularity),
    primer_cuartil = as.numeric(quantile(popularity, 0.25)),
    tercer_cuartil = as.numeric(quantile(popularity, 0.75)),
    maximo = max(popularity),
    canciones_popularidad_cero = sum(popularity == 0),
    porcentaje_popularidad_cero = mean(popularity == 0) * 100
  )

write_csv(
  resumen_popularidad,
  "results/popularity/01_resumen_popularidad_global.csv"
)
print(resumen_popularidad)

# Representa la distribucion de popularidad
grafica_histograma <- ggplot(spotify, aes(x = popularity)) +
  geom_histogram(binwidth = 5, boundary = 0,
                 color = "white", fill="#4C78A8") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  labs(
    title = "Distribución de popularidad de las canciones",
    x = "Popularidad de Spotify",
    y = "Número de canciones"
  ) +
  theme_minimal(base_size = 12)

print(grafica_histograma)
ggsave(
  "results/popularity/01_distribucion_popularidad.png",
  plot = grafica_histograma, width = 10, height = 6, dpi = 300
)

# ==========================================
# 3. INTERVALOS DE POPULARIDAD
# ==========================================
spotify_intervalos <- spotify %>%
  mutate(
    intervalo_popularidad = cut(
      popularity,
      breaks = c(-Inf, 25, 50, 75, Inf),
      labels = c("0–25", ">25–50", ">50–75", ">75–100"),
      right = TRUE,
      ordered_result = TRUE
    )
  )

resumen_intervalos <- spotify_intervalos %>%
  group_by(intervalo_popularidad, .drop = FALSE) %>%
  summarise(
    n_canciones = n(),
    porcentaje = n() / nrow(spotify) * 100,
    popularidad_media = mean(popularity),
    popularidad_mediana = median(popularity),
    .groups = "drop"
  )

write_csv(
  resumen_intervalos,
  "results/popularity/02_canciones_por_intervalo_popularidad.csv"
)
print(resumen_intervalos)

# Representa las canciones por intervalo de popularidad
grafica_intervalos <- ggplot(
  resumen_intervalos,
  aes(x = intervalo_popularidad, y = n_canciones)
) +
  geom_col(fill = "#E07A5F") +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.35, size = 3.8
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Número de canciones por intervalo de popularidad",
    x = "Intervalo de popularidad",
    y = "Número de canciones"
  ) +
  theme_minimal(base_size = 12)

print(grafica_intervalos)
ggsave(
  "results/popularity/02_intervalos_popularidad.png",
  plot = grafica_intervalos, width = 9, height = 6, dpi = 300
)

# ==========================================
# 4. SEPARAR LOS GENEROS
# ==========================================

# Cuenta cada cancion una vez por genero
# Una cancion puede pertenecer a varios generos
spotify_generos <- spotify %>%
  select(track_id, track_name, artists, popularity, track_genre) %>%
  separate_rows(track_genre, sep = "\\s*;\\s*") %>%
  mutate(genero = str_to_lower(str_trim(track_genre))) %>%
  filter(!is.na(genero), genero != "") %>%
  distinct(track_id, genero, .keep_all = TRUE)

resumen_generos <- spotify_generos %>%
  group_by(genero) %>%
  summarise(
    n_canciones = n_distinct(track_id),
    media = mean(popularity),
    mediana = median(popularity),
    desviacion = sd(popularity),
    cuartil_25 = as.numeric(quantile(popularity, 0.25)),
    cuartil_75 = as.numeric(quantile(popularity, 0.75)),
    porcentaje_popularidad_cero = mean(popularity == 0) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(media))

write_csv(
  resumen_generos,
  "results/genres/03_popularidad_por_genero.csv"
)

cat("Número de géneros distintos tras separar las etiquetas:",
    n_distinct(spotify_generos$genero), "\n")
cat("Número de combinaciones únicas canción-género:",
    nrow(spotify_generos), "\n\n")

# Selecciona los generos con al menos 100 canciones
min_canciones_genero <- 100
resumen_generos_filtrado <- resumen_generos %>%
  filter(n_canciones >= min_canciones_genero)

# Selecciona los 20 generos con mayor y menor popularidad media
top20_generos <- resumen_generos_filtrado %>%
  arrange(desc(media)) %>%
  slice_head(n = 20)

bottom20_generos <- resumen_generos_filtrado %>%
  arrange(media) %>%
  slice_head(n = 20)

write_csv(top20_generos, "results/genres/04_top20_generos_popularidad_media.csv")
write_csv(bottom20_generos, "results/genres/05_bottom20_generos_popularidad_media.csv")

cat("\nTop 20 géneros por popularidad media:\n")
print(top20_generos %>% select(genero, n_canciones, media, mediana))
cat("\n20 géneros con menor popularidad media:\n")
print(bottom20_generos %>% select(genero, n_canciones, media, mediana))

# Representa los 20 generos con mayor popularidad media
grafica_top20 <- ggplot(
  top20_generos,
  aes(x = reorder(genero, media), y = media)
) +
  geom_col(fill = "#8A6FBF") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(
    title = "20 géneros con mayor popularidad media",
    x = "Género",
    y = "Popularidad media"
  ) +
  theme_minimal(base_size = 12)

print(grafica_top20)
ggsave(
  "results/genres/03_top20_generos_popularidad_media.png",
  plot = grafica_top20, width = 10, height = 8, dpi = 300
)

# Representa los 20 generos con menor popularidad media
grafica_bottom20 <- ggplot(
  bottom20_generos,
  aes(x = reorder(genero, media), y = media)
) +
  geom_col(fill = "#BED1F2") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(
    title = "20 géneros con menor popularidad media",
    x = "Género",
    y = "Popularidad media"
  ) +
  theme_minimal(base_size = 12)

print(grafica_bottom20)
ggsave(
  "results/genres/04_bottom20_generos_popularidad_media.png",
  plot = grafica_bottom20, width = 10, height = 8, dpi = 300
)

# ==========================================
# 5. BOXPLOT: DISTRIBUCION DE POPULARIDAD POR GENERO
# ==========================================
generos_extremos <- bind_rows(
  resumen_generos_filtrado %>% slice_max(media, n = 10, with_ties = FALSE),
  resumen_generos_filtrado %>% slice_min(media, n = 10, with_ties = FALSE)
) %>%
  distinct(genero) %>%
  pull(genero)

datos_boxplot_generos <- spotify_generos %>%
  filter(genero %in% generos_extremos)

grafica_boxplot_generos <- ggplot(
  datos_boxplot_generos,
  aes(x = fct_reorder(genero, popularity, .fun = median), y = popularity, fill = genero)
) +
  geom_boxplot(
    outlier.alpha = 0.12, 
    outlier.size = 0.4,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(
    title = "Distribución de popularidad en los géneros extremos",
    x = "Género",
    y = "Popularidad de Spotify"
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_viridis_d(option = "plasma")

print(grafica_boxplot_generos)
ggsave(
  "results/genres/05_boxplot_popularidad_por_genero.png",
  plot = grafica_boxplot_generos, width = 10, height = 9, dpi = 300
)
