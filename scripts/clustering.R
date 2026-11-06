library(tidyverse)
library(cluster)

# ==========================================
# 1. PREPARACION DE LOS DATOS
# ==========================================

# Carga el dataset
spotify <- read_csv("data/processed/spotify_clean.csv")

# Crea la carpeta para guardar los resultados
dir.create("results/clustering", recursive = TRUE, showWarnings = FALSE)

# Selecciona las caracteristicas musicales
audio_features <- c(
  "danceability",
  "energy",
  "valence",
  "acousticness",
  "instrumentalness",
  "speechiness",
  "liveness"
)

# Crea el dataset para el clustering
clustering_data <- spotify %>%
  select(all_of(audio_features))

# Comprueba los NAs
colSums(is.na(clustering_data))

# Comprueba las desviaciones tipicias (comprueba que todas las variables tienen variabilidad)
apply(clustering_data, 2, sd)




# ==========================================
# 2. ESTANDARIZACION DE VARIABLES (que todas las variables tengan media = 0 y desviacion tipica = 1)
# ==========================================

# Estandariza las caracteristicas musicales
clustering_scaled <- scale(clustering_data)

# Comprueba la desviacion tipica
apply(clustering_scaled, 2, sd)




# ==========================================
# 3.1. SELECCION DEL NUMERO DE CLUSTERS (mediante el metodo del codo)
# ==========================================

# Valores de k means que vamos a probar
valores_k <- 1:12

# Vector para guardar el WCSS
wcss <- numeric(length(valores_k))

# Fija la semilla
set.seed(123)

# Ejecuta k-means con distintos valores de k
for(k in valores_k){
  modelo <- kmeans(clustering_scaled, centers = k, nstart = 10)
  wcss[k] <- modelo$tot.withinss
}

# Resultados codo
data.frame(k = valores_k, WCSS = wcss)

# Guarda el grafico del metodo del codo
png("results/clustering/metodo_codo.png", width = 1200, height = 800, res = 150)

# Grafico metodo del codo
plot(valores_k, wcss, type = "b", pch = 19, col = "red", xlab = "Número de clusters (K)", ylab = "WCSS", main = "Método del codo")

dev.off()




# ==========================================
# 3.2. SELECCION DEL NUMERO DE CLUSTERS (mediante el coeficiente de silueta)
# ==========================================

# Fija la semilla
set.seed(123)

# Selecciona una muestra aleatoria de 2000 canciones
indices <- sample(1:nrow(clustering_scaled), 2000)
muestra_scaled <- clustering_scaled[indices, ]

# Calcula las distancias entre las canciones de la muestra
distancias <- dist(muestra_scaled)

# Valores de K que vamos a probar
valores_k_silueta <- 2:12

# Vector para guardar la silueta media
silueta_media <- numeric(length(valores_k_silueta))

# Calcula la silueta para cada valor de K
for(k in valores_k_silueta){
  modelo <- kmeans(muestra_scaled, centers = k, nstart = 10)
  silueta <- silhouette(modelo$cluster, distancias)
  silueta_media[k - 1] <- mean(silueta[, 3])
}

# Resultados silueta
data.frame(K = valores_k_silueta, Silueta = silueta_media)

# Guarda el grafico del coeficiente de silueta
png("results/clustering/coeficiente_silueta.png", width = 1200, height = 800, res = 150)

# Grafico de la silueta
plot(valores_k_silueta, silueta_media, type = "b", pch = 19, col = "red", xlab = "Número de clusters (K)", ylab = "Silueta media",
     main = "Coeficiente de silueta")

dev.off()




# ==========================================
# 4. EJECUCION DEL MODELO K-MEANS
# ==========================================

# Fija la semilla
set.seed(123)

# Ejecuta k-means con 8 clusters
modelo_kmeans <- kmeans(clustering_scaled, centers = 8, nstart = 25, iter.max = 100)

modelo_kmeans

# Asigna un cluster a cada cancion
spotify$cluster <- modelo_kmeans$cluster

# Comprueba el numero de canciones por cluster
table(spotify$cluster)

# Guarda el modelo y el dataset con los clusters

saveRDS(modelo_kmeans, "results/clustering/modelo_kmeans_k8.rds")

write_csv(spotify, "results/clustering/spotify_clusters_k8.csv")




# ==========================================
# 5. CARACTERIZACION DE LOS CLUSTERS
# ==========================================

# Calcula la media de las caracteristicas musicales por cluster
perfiles <- aggregate(spotify[, audio_features], by = list(Cluster = spotify$cluster), FUN = mean)

# Redondea los resultados a tres decimales
perfiles[, -1] <- round(perfiles[, -1], 3)

# Muestra los perfiles musicales
print(perfiles)

# Muestra los centroides estandarizados
round(modelo_kmeans$centers, 2)




# ==========================================
# 5.1. MAPA DE CALOR DE LOS CLUSTERS
# ==========================================

# Obtiene los centroides estandarizados
centroides <- as.data.frame(modelo_kmeans$centers)

# Anade el identificador de cada cluster
centroides$Cluster <- 1:8

# Transforma los datos para crear el grafico
centroides_long <- centroides %>%
  pivot_longer(
    cols = all_of(audio_features),
    names_to = "Caracteristica",
    values_to = "Valor"
  )

# Crea el mapa de calor
grafico_perfiles <- ggplot(centroides_long, aes(x = Caracteristica, y = factor(Cluster), fill = Valor)
) + geom_tile(color = "white") + 
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "firebrick",
    midpoint = 0,
    limits = c(-2, 2),
    oob = scales::squish
  ) + labs(
    title = "Perfiles musicales de los clusters",
    x = "Característica musical",
    y = "Cluster",
    fill = "Valor estandarizado"
  ) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Muestra el grafico
print(grafico_perfiles)

# Guarda el grafico
ggsave(
  "results/clustering/mapa_calor_clusters_k8.png", plot = grafico_perfiles,
  width = 11,
  height = 6,
  dpi = 300
)




# ==========================================
# 5.2. VISUALIZACION DE LOS CLUSTERS CON PCA
# ==========================================

# Realiza el analisis de componentes principales
pca <- prcomp(clustering_scaled)

# Crea un dataset con las dos primeras componentes
datos_pca <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], Cluster = factor(spotify$cluster))

# Selecciona una muestra para visualizar mejor los puntos
set.seed(123)
indices_pca <- sample(1:nrow(datos_pca), 4000)
muestra_pca <- datos_pca[indices_pca, ]

# Calcula el centro de cada cluster en el plano PCA
centroides_pca <- aggregate(cbind(PC1, PC2) ~ Cluster, data = datos_pca, FUN = mean)

# Grafico PCA 
grafico_pca <- ggplot(muestra_pca, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.5, size = 1.6) +
  geom_point(
    data = centroides_pca,
    aes(x = PC1, y = PC2),
    color = "black",
    size = 4,
    shape = 4,
    stroke = 1.5,
    inherit.aes = FALSE
  ) +
  geom_label(
    data = centroides_pca,
    aes(x = PC1, y = PC2, label = Cluster),
    color = "black",
    fill = "white",
    fontface = "bold",
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    "#FF7F00", "#A65628", "#F781BF", "#17BECF"
  )) +
  labs(
    title = "Visualización de los clusters mediante PCA",
    x = "Primera componente principal (PC1)",
    y = "Segunda componente principal (PC2)",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

# Muestra el grafico
print(grafico_pca)

# Guarda el grafico
ggsave("results/clustering/pca_clusters_k8.png", plot = grafico_pca,
  width = 10,
  height = 7,
  dpi = 300
)

# Que porcentaje de la variabilidad musical representa cada uno de los dos ejes del grafico
summary(pca)$importance[2, 1:2] * 100




# ==========================================
# 6. RELACION CON LA POPULARIDAD Y LOS GENEROS
# ==========================================

# 6.1. Calculo de la popularidad por cluster

# Calcula la estadistica de popularidad para cada cluster
popularidad_clusters <- spotify %>%
  group_by(cluster) %>%
  summarise(Canciones = n(), Media = mean(popularity), Mediana = median(popularity), Desviacion = sd(popularity),
    .groups = "drop"
  )

# Redondea los resultados
popularidad_clusters[, 3:5] <- round(popularidad_clusters[, 3:5], 2)

print(popularidad_clusters)

# Guarda la tabla
write_csv(popularidad_clusters, "results/clustering/popularidad_clusters_k8.csv")

# Grafico de popularidad por cluster
grafico_popularidad <- ggplot(spotify, aes(x = factor(cluster), y = popularity)
) +
  geom_boxplot(fill = "steelblue", alpha = 0.7,
               outlier.alpha = 0.15) +
  stat_summary(fun = mean, geom = "point",
               color = "red", size = 3) +
  labs(title = "Distribución de popularidad por cluster",
    x = "Cluster",
    y = "Popularidad"
  ) + theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Muestra el grafico
print(grafico_popularidad)

# Guarda el grafico
ggsave(
  "results/clustering/popularidad_clusters_k8.png", plot = grafico_popularidad,
  width = 10,
  height = 7,
  dpi = 300
)




# ==========================================
# 6.2. COMPOSICION POR GENEROS
# ==========================================

# Separa las canciones que pertenecen a varios generos
generos_clusters <- spotify %>%
  select(track_id, cluster, track_genre) %>%
  separate_rows(track_genre, sep = ";\\s*") %>%
  distinct(track_id, cluster, track_genre)

# Cuenta cuantas canciones de cada genero hay en cada cluster
tabla_generos <- generos_clusters %>%
  count(cluster, track_genre, name = "Canciones")

# Selecciona los dos generos mas frecuentes de cada cluster
top_generos <- tabla_generos %>%
  group_by(cluster) %>%
  slice_max(Canciones, n = 2, with_ties = FALSE) %>%
  ungroup()

# Muestra los resultados
print(top_generos)

# Selecciona los generos
generos_destacados <- unique(top_generos$track_genre)

# Calcula las canciones por cluster y genero
composicion_generos <- tabla_generos %>%
  filter(track_genre %in% generos_destacados) %>%
  complete(cluster = 1:8, track_genre = generos_destacados, fill = list(Canciones = 0))

# Calcula el tamano de cada cluster
tamano_clusters <- spotify %>%
  count(cluster, name = "Total_cluster")

# Calcula el porcentaje de los generos principales
generos_resumen <- top_generos %>%
  left_join(tamano_clusters, by = "cluster") %>%
  mutate(Porcentaje = 100 * Canciones / Total_cluster)

# Crea el grafico de generos
grafico_generos <- ggplot(generos_resumen, aes(x = Porcentaje, y = track_genre)) +
  geom_col(fill = "steelblue", width = 0.6) + geom_text(
    aes(label = paste0(round(Porcentaje, 1), "%")),
    hjust = -0.15,
    size = 4 
  ) + facet_wrap(~cluster, ncol = 2, scales = "free") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))
  ) +
  labs(title = "Géneros más frecuentes por cluster",
    y = "Género musical"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 13, face = "bold"),
    panel.grid.major.y = element_blank()
  )

# Muestra el grafico
print(grafico_generos)

# Guarda el grafico
ggsave(
  "results/clustering/generos_clusters_k8.png",
  plot = grafico_generos,
  width = 12,
  height = 11,
  dpi = 300
)





