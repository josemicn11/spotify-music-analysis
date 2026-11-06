# ==========================================
# 1. PREPARACION DE LOS DATOS
# ==========================================
# Carga el dataset limpio
archivo_csv <- "data/processed/spotify_clean.csv"
spotify <- read.csv(archivo_csv, stringsAsFactors = FALSE, check.names = FALSE)

# Selecciona las variables para el analisis de correlaciones
# duration_ms se convierte a minutos para facilitar la interpretacion.
variables <- c(
  "popularity", "danceability", "energy", "loudness", "speechiness",
  "acousticness", "instrumentalness", "liveness", "valence", "tempo",
  "duration_ms"
)
no_disponibles <- setdiff(variables, names(spotify))
if (length(no_disponibles) > 0) {
  stop(paste("Faltan estas columnas en el CSV:", paste(no_disponibles, collapse = ", ")))
}

datos_correlacion <- spotify[, variables]
datos_correlacion$duration_min <- datos_correlacion$duration_ms / 60000
datos_correlacion$duration_ms <- NULL
datos_correlacion[] <- lapply(datos_correlacion, function(x) as.numeric(as.character(x)))

# Crea la carpeta para guardar los resultados
salida <- "results/correlations"
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# ==========================================
# 2. CORRELACIONES DE PEARSON ENTRE POPULARIDAD Y CARACTERISTICAS
# ==========================================
caracteristicas <- setdiff(colnames(datos_correlacion), "popularity")
correlaciones_popularidad <- data.frame(
  caracteristica = caracteristicas,
  correlacion_pearson = vapply(caracteristicas, function(variable) {
    cor(datos_correlacion$popularity, datos_correlacion[[variable]],
        method = "pearson", use = "pairwise.complete.obs")
  }, numeric(1))
)
correlaciones_popularidad <- correlaciones_popularidad[
  order(abs(correlaciones_popularidad$correlacion_pearson), decreasing = TRUE),
]
write.csv(correlaciones_popularidad,
          file.path(salida, "correlaciones_con_popularidad.csv"), row.names = FALSE)

# ==========================================
# 3. RELACIONES ENTRE LAS PROPIAS CARACTERISTICAS MUSICALES
# ==========================================
matriz_caracteristicas <- cor(
  datos_correlacion[caracteristicas], method = "pearson", use = "pairwise.complete.obs"
)
pares <- which(upper.tri(matriz_caracteristicas), arr.ind = TRUE)
correlaciones_caracteristicas <- data.frame(
  variable_1 = rownames(matriz_caracteristicas)[pares[, 1]],
  variable_2 = colnames(matriz_caracteristicas)[pares[, 2]],
  correlacion_pearson = matriz_caracteristicas[pares]
)
correlaciones_caracteristicas <- correlaciones_caracteristicas[
  order(abs(correlaciones_caracteristicas$correlacion_pearson), decreasing = TRUE),
]
write.csv(correlaciones_caracteristicas,
          file.path(salida, "correlaciones_entre_caracteristicas.csv"), row.names = FALSE)

# ==========================================
# 4. CREAR Y GUARDAR LA MATRIZ COMPLETA DE CORRELACIONES
# ==========================================
# Calcula la matriz con popularidad y caracteristicas musicales
matriz_correlaciones <- cor(
  datos_correlacion, method = "pearson", use = "pairwise.complete.obs"
)
write.csv(matriz_correlaciones,
          file.path(salida, "matriz_correlaciones_pearson.csv"), row.names = TRUE)
print(round(matriz_correlaciones, 3))

# ==========================================
# 5. REPRESENTAR LA MATRIZ MEDIANTE UN MAPA DE CALOR
# ==========================================
# Crea el mapa de calor de las correlaciones
colores <- colorRampPalette(c("#2166AC", "white", "#B2182B"))(201)
png(file.path(salida, "mapa_calor_correlaciones.png"),
    width = 1400, height = 1200, res = 140)
par(mar = c(10, 10, 4, 2))
image(x = seq_len(ncol(matriz_correlaciones)),
      y = seq_len(nrow(matriz_correlaciones)),
      z = t(matriz_correlaciones[nrow(matriz_correlaciones):1, , drop = FALSE]),
      col = colores, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "",
      main = "Matriz de correlaciones de Pearson")
axis(1, at = seq_len(ncol(matriz_correlaciones)), labels = colnames(matriz_correlaciones),
     las = 2, cex.axis = .8)
axis(2, at = seq_len(nrow(matriz_correlaciones)),
     labels = rev(rownames(matriz_correlaciones)), las = 2, cex.axis = .8)
for (i in seq_len(nrow(matriz_correlaciones))) {
  for (j in seq_len(ncol(matriz_correlaciones))) {
    text(j, nrow(matriz_correlaciones) - i + 1,
         labels = sprintf("%.2f", matriz_correlaciones[i, j]), cex = .65)
  }
}
dev.off()

# ==========================================
# 6. INTERPRETACION DE LAS ASOCIACIONES LINEALES
# ==========================================
# Criterio orientativo basado en el valor absoluto de r:
# < 0.10 muy debil; 0.10-0.29 debil;
# 0.30-0.49 moderada; >= 0.50 fuerte.
interpretar_fuerza <- function(r) {
  magnitud <- abs(r)
  if (is.na(magnitud)) return("no calculable")
  if (magnitud < .10) return("muy debil")
  if (magnitud < .30) return("debil")
  if (magnitud < .50) return("moderada")
  "fuerte"
}
interpretar_direccion <- function(r) {
  if (is.na(r) || abs(r) < .10) return("sin direccion clara")
  if (r > 0) "positiva" else "negativa"
}

correlaciones_popularidad$fuerza <- vapply(
  correlaciones_popularidad$correlacion_pearson, interpretar_fuerza, character(1)
)
correlaciones_popularidad$direccion <- vapply(
  correlaciones_popularidad$correlacion_pearson, interpretar_direccion, character(1)
)
correlaciones_caracteristicas$fuerza <- vapply(
  correlaciones_caracteristicas$correlacion_pearson, interpretar_fuerza, character(1)
)
correlaciones_caracteristicas$direccion <- vapply(
  correlaciones_caracteristicas$correlacion_pearson, interpretar_direccion, character(1)
)
write.csv(correlaciones_popularidad,
          file.path(salida, "interpretacion_popularidad.csv"), row.names = FALSE)
write.csv(correlaciones_caracteristicas,
          file.path(salida, "interpretacion_caracteristicas.csv"), row.names = FALSE)

cat("\nCorrelaciones de popularidad (ordenadas por fuerza):\n")
print(correlaciones_popularidad, digits = 3, row.names = FALSE)
cat("\nAsociaciones mas fuertes entre caracteristicas:\n")
print(head(correlaciones_caracteristicas, 10), digits = 3, row.names = FALSE)
cat("\nArchivos guardados en:", normalizePath(salida), "\n")
