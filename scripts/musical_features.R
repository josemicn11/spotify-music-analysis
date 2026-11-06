# ==========================================
# 1. PREPARACION DE LOS DATOS
# ==========================================
# Carga el dataset limpio
archivo_csv <- "data/processed/spotify_clean.csv"
spotify <- read.csv(archivo_csv, stringsAsFactors = FALSE, check.names = FALSE)

# ==========================================
# 2. SELECCION DE LAS VARIABLES MUSICALES
# ==========================================
# Selecciona las caracteristicas musicales
variables_musicales <- c(
  "danceability", "energy", "key", "loudness", "mode", "speechiness",
  "acousticness", "instrumentalness", "liveness", "valence", "tempo",
  "time_signature", "duration_ms"
)
no_disponibles <- setdiff(variables_musicales, names(spotify))
if (length(no_disponibles) > 0) {
  stop(paste("Faltan estas columnas en el CSV:", paste(no_disponibles, collapse = ", ")))
}

musica <- spotify[, variables_musicales]
musica$duration_min <- musica$duration_ms / 60000
musica$duration_ms <- NULL
variables_musicales <- c(setdiff(variables_musicales, "duration_ms"), "duration_min")
musica[variables_musicales] <- lapply(
  musica[variables_musicales], function(x) as.numeric(as.character(x))
)

# ==========================================
# 3. CALCULO DE MEDIA, MEDIANA, DESVIACION TIPICA Y RANGOS
# ==========================================
resumen <- do.call(rbind, lapply(variables_musicales, function(variable) {
  x <- musica[[variable]]
  x <- x[is.finite(x)]
  data.frame(
    variable = variable,
    n = length(x), # Numero de canciones
    media = if (length(x)) mean(x) else NA_real_,
    mediana = if (length(x)) median(x) else NA_real_,
    desviacion_tipica = if (length(x) > 1) sd(x) else NA_real_,
    minimo = if (length(x)) min(x) else NA_real_,
    maximo = if (length(x)) max(x) else NA_real_,
    rango = if (length(x)) diff(range(x)) else NA_real_,
    q1 = if (length(x)) unname(quantile(x, .25)) else NA_real_,
    q3 = if (length(x)) unname(quantile(x, .75)) else NA_real_
  )
}))

# Crea la carpeta para guardar los resultados
salida <- "results/descriptive"
dir.create(salida, recursive = TRUE, showWarnings = FALSE)
write.csv(resumen, file.path(salida, "resumen_variables_musicales.csv"), row.names = FALSE)
print(resumen, digits = 4, row.names = FALSE)

# ==========================================
# 4. GRAFICAS DE LOS CALCULOS OBTENIDOS
# ==========================================
# Representa cada estadistico en un grafico
# Las variables conservan sus unidades originales
estadisticos <- c("media", "mediana", "desviacion_tipica", "rango")
for (estadistico in estadisticos) {
  valores <- resumen[[estadistico]]
  names(valores) <- resumen$variable
  png(file.path(salida, paste0("grafica_", estadistico, ".png")),
      width = 1200, height = 800, res = 140)
  par(mar = c(8, 4, 3, 1))
  barplot(valores, las = 2, col = "steelblue2",
          main = paste("Variable musical:", estadistico),
          ylab = estadistico, cex.names = .75)
  dev.off()
}

# ==========================================
# 5. INVESTIGACION DE LAS DISTRIBUCIONES
# ==========================================
# Calcula los cuantiles, el rango intercuartil y la asimetria
variables_distribucion <- c("danceability", "energy", "valence", "duration_min")
investigacion_distribucion <- do.call(rbind, lapply(variables_distribucion, function(variable) {
  x <- musica[[variable]]
  x <- x[is.finite(x)]
  data.frame(
    variable = variable,
    n = length(x),
    minimo = if (length(x)) min(x) else NA_real_,
    q1 = if (length(x)) unname(quantile(x, .25)) else NA_real_,
    mediana = if (length(x)) median(x) else NA_real_,
    q3 = if (length(x)) unname(quantile(x, .75)) else NA_real_,
    maximo = if (length(x)) max(x) else NA_real_,
    rango_intercuartil = if (length(x)) IQR(x) else NA_real_,
    asimetria = if (length(x) > 2 && sd(x) > 0) mean((x - mean(x))^3) / sd(x)^3 else NA_real_
  )
}))
write.csv(investigacion_distribucion,
          file.path(salida, "investigacion_distribuciones.csv"), row.names = FALSE)
print(investigacion_distribucion, digits = 4, row.names = FALSE)

# ==========================================
# 6. HISTOGRAMAS Y OTROS GRAFICOS DESCRIPTIVOS
# ==========================================
# Crea los histogramas e indica la media y la mediana
for (variable in variables_distribucion) {
  x <- musica[[variable]]
  x <- x[is.finite(x)]
  png(file.path(salida, paste0("histograma_", variable, ".png")),
      width = 1000, height = 700, res = 140)
  par(mar = c(4, 4, 3, 1))
  hist(x, breaks = "FD", col = "steelblue2", border = "white",
       main = paste("Distribucion de", variable), xlab = variable, ylab = "Frecuencia")
  abline(v = mean(x), col = "firebrick", lwd = 2, lty = 2)
  abline(v = median(x), col = "darkgreen", lwd = 2, lty = 3)
  legend("topright", c("Media", "Mediana"), col = c("firebrick", "darkgreen"),
         lty = c(2, 3), lwd = 2, bty = "n", cex = .8)
  dev.off()
}

# Crea un diagrama de caja para cada caracteristica
rasgos_caja <- c("danceability", "energy", "valence")
colores_caja <- c("lightblue", "lightgreen", "mistyrose")
for (i in seq_along(rasgos_caja)) {
  variable <- rasgos_caja[i]
  png(file.path(salida, paste0("diagrama_caja_", variable, ".png")),
      width = 900, height = 700, res = 140)
  par(mar = c(5, 4, 3, 1))
  boxplot(musica[[variable]], col = colores_caja[i],
          main = paste("Diagrama de caja de", variable), ylab = "Valor", xlab = variable)
  dev.off()
}

# Crea el diagrama de caja de la duracion
png(file.path(salida, "diagrama_caja_duration_min.png"), width = 1000, height = 500, res = 140)
par(mar = c(5, 4, 3, 1))
boxplot(musica$duration_min, col = "wheat", horizontal = TRUE,
        main = "Duracion de las pistas", xlab = "Minutos")
dev.off()

# Representa la relacion entre bailabilidad y energia
png(file.path(salida, "dispersion_energia_bailabilidad.png"), width = 900, height = 700, res = 140)
plot(musica$danceability, musica$energy,
     pch = 16, cex = .45, col = grDevices::adjustcolor("steelblue4", alpha.f = .25),
     xlab = "Danceability", ylab = "Energy", main = "Danceability y energy")
abline(lm(energy ~ danceability, data = musica), col = "firebrick", lwd = 2)
dev.off()

cat("\nArchivos guardados en:", normalizePath(salida), "\n")
