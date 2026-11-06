# ==========================================
# 1. PREPARACION DE LOS DATOS
# ==========================================
# Carga el dataset limpio
archivo_csv <- "data/processed/spotify_clean.csv"
spotify <- read.csv(archivo_csv, stringsAsFactors = FALSE, check.names = FALSE)

# Selecciona las variables para el analisis
variables_necesarias <- c("danceability", "duration_ms", "popularity")
no_disponibles <- setdiff(variables_necesarias, names(spotify))
if (length(no_disponibles) > 0) {
  stop(paste("Faltan estas columnas en el CSV:", paste(no_disponibles, collapse = ", ")))
}

datos <- spotify[, variables_necesarias]
datos[] <- lapply(datos, function(x) as.numeric(as.character(x)))
datos$duration_min <- datos$duration_ms / 60000
datos$duration_ms <- NULL
datos <- datos[complete.cases(datos), ]

# Crea la carpeta para guardar los resultados
salida <- "results/danceability_duration_popularity"
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# ==========================================
# 2. RELACION ENTRE DANCEABILITY Y POPULARITY
# ==========================================
correlacion_bailabilidad <- cor(
  datos$danceability, datos$popularity, method = "pearson"
)
cat("Correlacion de Pearson entre danceability y popularity:",
    round(correlacion_bailabilidad, 3), "\n")

# ==========================================
# 3. RELACION ENTRE DURACION Y POPULARITY
# ==========================================
correlacion_duracion <- cor(
  datos$duration_min, datos$popularity, method = "pearson"
)
cat("Correlacion de Pearson entre duracion (minutos) y popularity:",
    round(correlacion_duracion, 3), "\n")

resumen_correlaciones <- data.frame(
  relacion = c("danceability y popularity", "duracion_min y popularity"),
  correlacion_pearson = c(correlacion_bailabilidad, correlacion_duracion)
)
write.csv(resumen_correlaciones,
          file.path(salida, "correlaciones_pearson.csv"), row.names = FALSE)

# ==========================================
# 4. GRAFICOS DE DISPERSION
# ==========================================
# Se guarda un grafico por relacion; la linea roja representa el ajuste lineal.
png(file.path(salida, "dispersion_danceability_popularity.png"),
    width = 1000, height = 750, res = 140)
plot(datos$danceability, datos$popularity,
     pch = 16, cex = .45,
     col = grDevices::adjustcolor("steelblue4", alpha.f = .2),
     xlab = "Danceability", ylab = "Popularidad",
     main = "Danceability y popularidad")
abline(lm(popularity ~ danceability, data = datos), col = "firebrick", lwd = 2)
dev.off()

png(file.path(salida, "dispersion_duracion_popularity.png"),
    width = 1000, height = 750, res = 140)
plot(datos$duration_min, datos$popularity,
     pch = 16, cex = .45,
     col = grDevices::adjustcolor("steelblue4", alpha.f = .2),
     xlab = "Duracion (minutos)", ylab = "Popularidad",
     main = "Duracion y popularidad")
abline(lm(popularity ~ duration_min, data = datos), col = "firebrick", lwd = 2)
dev.off()

# ==========================================
# 5. COMPARAR POPULARIDAD ENTRE INTERVALOS DE DURACION
# ==========================================
# Intervalos en minutos: menos de 2, 2-3, 3-4, 4-5 y 5 o mas.
limites_duracion <- c(-Inf, 2, 3, 4, 5, Inf)
etiquetas_duracion <- c("< 2 min", "2-3 min", "3-4 min", "4-5 min", "5+ min")
datos$intervalo_duracion <- cut(
  datos$duration_min, breaks = limites_duracion,
  labels = etiquetas_duracion, right = FALSE
)

resumen_intervalos <- do.call(rbind, lapply(levels(datos$intervalo_duracion), function(intervalo) {
  popularidad_intervalo <- datos$popularity[datos$intervalo_duracion == intervalo]
  data.frame(
    intervalo_duracion = intervalo,
    numero_pistas = length(popularidad_intervalo),
    media_popularidad = if (length(popularidad_intervalo)) mean(popularidad_intervalo) else NA_real_,
    mediana_popularidad = if (length(popularidad_intervalo)) median(popularidad_intervalo) else NA_real_
  )
}))
write.csv(resumen_intervalos,
          file.path(salida, "popularidad_por_intervalo_duracion.csv"), row.names = FALSE)

png(file.path(salida, "popularidad_por_intervalo_duracion.png"),
    width = 1100, height = 750, res = 140)
boxplot(popularity ~ intervalo_duracion, data = datos,
        col = c("lightblue", "lightgreen", "wheat", "mistyrose", "thistle"),
        xlab = "Intervalo de duracion", ylab = "Popularidad",
        main = "Popularidad por intervalos de duracion",
        outline = FALSE)
dev.off()

# ==========================================
# 6. INTERPRETAR RESULTADOS SIN ATRIBUIR CAUSALIDAD
# ==========================================
interpretar_correlacion <- function(r) {
  fuerza <- if (abs(r) < .10) "muy debil" else
    if (abs(r) < .30) "debil" else
      if (abs(r) < .50) "moderada" else "fuerte"
  direccion <- if (abs(r) < .10) "sin una direccion lineal clara" else
    if (r > 0) "positiva" else "negativa"
  paste(fuerza, direccion)
}

intervalos_validos <- resumen_intervalos[!is.na(resumen_intervalos$mediana_popularidad), ]
if (nrow(intervalos_validos) > 0) {
  intervalo_mayor_mediana <- intervalos_validos$intervalo_duracion[
    which.max(intervalos_validos$mediana_popularidad)
  ]
  intervalo_menor_mediana <- intervalos_validos$intervalo_duracion[
    which.min(intervalos_validos$mediana_popularidad)
  ]
  comparacion_intervalos <- paste0(
    "La mediana de popularidad mas alta aparece en el intervalo ",
    intervalo_mayor_mediana, " y la mas baja en ", intervalo_menor_mediana, "."
  )
} else {
  comparacion_intervalos <- "No hay intervalos con datos suficientes para compararlos."
}

interpretacion <- c(
  "INTERPRETACION DE LOS RESULTADOS",
  "",
  paste0("Danceability y popularidad: r = ", round(correlacion_bailabilidad, 3),
         "; asociacion ", interpretar_correlacion(correlacion_bailabilidad), "."),
  paste0("Duracion y popularidad: r = ", round(correlacion_duracion, 3),
         "; asociacion ", interpretar_correlacion(correlacion_duracion), "."),
  comparacion_intervalos,
  "",
  paste("Estas correlaciones describen asociaciones lineales en este conjunto de datos.",
        "No demuestran que la bailabilidad o la duracion causen cambios en la popularidad.",
        "Otros factores pueden influir y Pearson puede no reflejar relaciones no lineales.")
)
writeLines(interpretacion, file.path(salida, "interpretacion_resultados.txt"))
cat(paste(interpretacion, collapse = "\n"), "\n")
cat("\nArchivos guardados en:", normalizePath(salida), "\n")
