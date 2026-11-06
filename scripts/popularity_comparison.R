# ==========================================
# 1. PREPARACION DE LOS DATOS
# ==========================================
# Carga el dataset limpio
archivo_csv <- "data/processed/spotify_clean.csv"
spotify <- read.csv(archivo_csv, stringsAsFactors = FALSE, check.names = FALSE)

if (!"popularity" %in% names(spotify)) {
  stop("No se encontro la columna 'popularity' en el archivo CSV.")
}
spotify$popularity <- as.numeric(as.character(spotify$popularity))
spotify <- spotify[is.finite(spotify$popularity), ]

# Crea la carpeta para guardar los resultados
salida <- "results/popularity_comparison"
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# ==========================================
# 2. CALCULAR LOS CUARTILES DE POPULARITY
# ==========================================
cuartiles_popularity <- quantile(spotify$popularity,
                                  probs = c(.25, .50, .75), na.rm = TRUE)
names(cuartiles_popularity) <- c("Q1_25_por_ciento", "Q2_mediana", "Q3_75_por_ciento")
print(cuartiles_popularity)
write.csv(data.frame(cuartil = names(cuartiles_popularity),
                     valor = as.numeric(cuartiles_popularity)),
          file.path(salida, "cuartiles_popularity.csv"), row.names = FALSE)

# ==========================================
# 3. SELECCIONAR EL 25 % INFERIOR Y SUPERIOR
# ==========================================
# Se usan los puntos de corte inclusivos: popularity <= Q1 y popularity >= Q3.
# Si hay empates en los puntos de corte, el grupo puede superar el 25 %.
q1 <- unname(cuartiles_popularity["Q1_25_por_ciento"])
q3 <- unname(cuartiles_popularity["Q3_75_por_ciento"])
grupo_inferior <- spotify[spotify$popularity <= q1, , drop = FALSE]
grupo_superior <- spotify[spotify$popularity >= q3, , drop = FALSE]

cat("Canciones en el 25 % inferior (incluyendo empates):", nrow(grupo_inferior), "\n")
cat("Canciones en el 25 % superior (incluyendo empates):", nrow(grupo_superior), "\n")

# Selecciona las caracteristicas musicales
caracteristicas <- c(
  "danceability", "energy", "loudness", "speechiness", "acousticness",
  "instrumentalness", "liveness", "valence", "tempo", "duration_ms"
)
no_disponibles <- setdiff(caracteristicas, names(spotify))
if (length(no_disponibles) > 0) {
  stop(paste("Faltan estas columnas en el CSV:", paste(no_disponibles, collapse = ", ")))
}

# Convierte las variables a valores numericos y la duracion a minutos
for (variable in caracteristicas) {
  spotify[[variable]] <- as.numeric(as.character(spotify[[variable]]))
  grupo_inferior[[variable]] <- as.numeric(as.character(grupo_inferior[[variable]]))
  grupo_superior[[variable]] <- as.numeric(as.character(grupo_superior[[variable]]))
}
grupo_inferior$duration_min <- grupo_inferior$duration_ms / 60000
grupo_superior$duration_min <- grupo_superior$duration_ms / 60000
grupo_inferior$duration_ms <- NULL
grupo_superior$duration_ms <- NULL
caracteristicas[caracteristicas == "duration_ms"] <- "duration_min"

# ==========================================
# 4. COMPARAR CARACTERISTICAS MUSICALES ENTRE AMBOS GRUPOS
# ==========================================
estadistico <- function(x, funcion) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else funcion(x)
}

comparacion <- do.call(rbind, lapply(caracteristicas, function(variable) {
  inferior <- grupo_inferior[[variable]]
  superior <- grupo_superior[[variable]]
  data.frame(
    caracteristica = variable,
    media_25_inferior = estadistico(inferior, mean),
    media_25_superior = estadistico(superior, mean),
    diferencia_medias_superior_menos_inferior =
      estadistico(superior, mean) - estadistico(inferior, mean),
    mediana_25_inferior = estadistico(inferior, median),
    mediana_25_superior = estadistico(superior, median),
    diferencia_medianas_superior_menos_inferior =
      estadistico(superior, median) - estadistico(inferior, median)
  )
}))
write.csv(comparacion,
          file.path(salida, "comparacion_caracteristicas_musicales.csv"), row.names = FALSE)
print(comparacion, digits = 4, row.names = FALSE)

# ==========================================
# 5. BOXPLOTS COMPARATIVOS
# ==========================================
# Crea un diagrama de caja para cada caracteristica
for (variable in caracteristicas) {
  valores <- c(grupo_inferior[[variable]], grupo_superior[[variable]])
  grupos <- factor(
    c(rep("25 % inferior", nrow(grupo_inferior)),
      rep("25 % superior", nrow(grupo_superior))),
    levels = c("25 % inferior", "25 % superior")
  )
  png(file.path(salida, paste0("boxplot_", variable, ".png")),
      width = 900, height = 700, res = 140)
  boxplot(valores ~ grupos, col = c("lightblue", "salmon"),
          xlab = "Grupo de popularidad", ylab = variable,
          main = paste("Comparacion de", variable, "por popularidad"),
          names = c("25 % inferior", "25 % superior"), outline = FALSE)
  dev.off()
}

# Representa las diferencias de medias y medianas
png(file.path(salida, "diferencias_medias_medianas.png"),
    width = 1300, height = 850, res = 140)
par(mar = c(8, 5, 4, 2))
matriz_diferencias <- rbind(
  "Diferencia de medias" = comparacion$diferencia_medias_superior_menos_inferior,
  "Diferencia de medianas" = comparacion$diferencia_medianas_superior_menos_inferior
)
barplot(matriz_diferencias, beside = TRUE, las = 2,
        col = c("steelblue2", "darkorange2"),
        names.arg = comparacion$caracteristica,
        ylab = "Diferencia (grupo superior - inferior)",
        main = "Diferencias entre grupos por caracteristica")
legend("topright", legend = rownames(matriz_diferencias),
       fill = c("steelblue2", "darkorange2"), bty = "n")
dev.off()

# ==========================================
# 6. CALCULAR Y GUARDAR DIFERENCIAS ENTRE MEDIAS O MEDIANAS
# ==========================================
# Las diferencias ya aparecen junto a los estadisticos en la tabla comparativa.
# Positivo significa que el valor es mayor en el grupo de popularidad superior.
cat("\nDiferencias calculadas (superior menos inferior):\n")
print(comparacion[c("caracteristica",
                    "diferencia_medias_superior_menos_inferior",
                    "diferencia_medianas_superior_menos_inferior")],
      digits = 4, row.names = FALSE)
cat("\nResultados guardados en:", normalizePath(salida), "\n")
