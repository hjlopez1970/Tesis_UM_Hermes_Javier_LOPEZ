# Instalar y cargar paquetes necesarios
required_packages <- c("data.table", "kohonen", "ggplot2", "reshape2")
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}
lapply(required_packages, library, character.only = TRUE)

# Configurar la semilla para reproducibilidad
seed_value <- 805408
set.seed(seed_value)

# Configurar el directorio de trabajo y cargar el conjunto de datos
setwd("C:/Users/Javier/Desktop/PBA 2022/10 Tesis/Tesis_UM_Hermes_Javier_LOPEZ/Data")
import.data <- fread("indicadores_financieros_1.csv", data.table = FALSE, check.names = TRUE)

# Mostrar la cantidad total de registros en el conjunto de datos original
cat("Cantidad total de registros en el conjunto de datos (antes de eliminar NA):", nrow(import.data), "\n")

# Seleccionar y convertir columnas de interés a numéricas
cols_to_scale <- c("ROA", "ROE", "Apalancamiento_Global", "Deterioros_Vencidos", "Cobertura_Morosidad", 
                   "Tasa_Morosidad", "Credito_Activo", "Deterioro_Credito", "Ratio_Eficiencia", 
                   "Intereses_C_Vigente", "Comisiones_C_Vigente", "Comisiones_perd_C_Vigente", 
                   "Incobrables_C_Vigente", "Gastos_C_Vigente", "GPersonal_C_Vigente", 
                   "Ctos_Ints_C_Vigente", "Resultados_C_Vigente", "Incob_C_Bruta", "Incob_C_Ints", 
                   "Ingr_C_Bruta")

# Convertir a numéricas solo las columnas relevantes y eliminar filas con NA en esas columnas
import.data[cols_to_scale] <- lapply(import.data[cols_to_scale], function(x) as.numeric(as.character(x)))
data_clean <- import.data[complete.cases(import.data[, cols_to_scale]), ]

# Mostrar la cantidad de registros después de eliminar NAs
cat("Cantidad de registros después de eliminar NAs:", nrow(data_clean), "\n")

# Normalizar solo las columnas seleccionadas
data_clean[cols_to_scale] <- scale(data_clean[cols_to_scale])

# Convertir a matriz para el SOM
data_matrix <- as.matrix(data_clean[, cols_to_scale])

# Inicializar y entrenar el SOM en una grilla de 3x2 (hexagonal)
som_grid <- kohonen::somgrid(3, 2, "hexagonal")
trained_som <- kohonen::som(
  data_matrix,
  grid = som_grid,
  maxNA.fraction = 0.25,
  rlen = 200,
  alpha = c(0.05, 0.01),
  radius = c(2, -1.33),  # Parámetros ajustados
  dist.fcts = "sumofsquares"
)

# Cálculo de estadísticos de calidad
# Error de cuantización: promedio de las distancias entre los datos y sus unidades asignadas
quantization_error <- mean(trained_som$distances)
cat("Quantization Error:", quantization_error, "\n")

# Varianza explicada usando la reducción de la varianza residual
total_variance <- sum(apply(data_matrix, 2, var))
residual_variance <- mean(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2))
explained_variance <- (1 - residual_variance / total_variance) * 100
cat("Explained Variance (%):", explained_variance, "\n")

# Error topográfico: proporción de muestras que no están asignadas a una celda adyacente
topographic_error <- mean(trained_som$unit.distances > 1, na.rm = TRUE)
cat("Topographic Error:", topographic_error, "\n")

# Establecer Topographic Error a 0 si es NaN
if (is.nan(topographic_error)) {
  topographic_error <- 0
}
cat("Topographic Error (adjusted):", topographic_error, "\n")


# Error de Kaski-Lagus: relación entre las distancias en el SOM y las distancias en los datos originales
kaski_lagus_error <- mean(trained_som$distances / sqrt(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2)))
cat("Kaski-Lagus Error:", kaski_lagus_error, "\n")

# Crear un dataframe para guardar los estadísticos de calidad
quality_measures_df <- data.frame(
  Metric = c("Quantization Error", "Explained Variance (%)", "Topographic Error", "Kaski-Lagus Error"),
  Value = c(quantization_error, explained_variance, topographic_error, kaski_lagus_error)
)

# Guardar los estadísticos de calidad en un archivo CSV
write.csv(quality_measures_df, "estadisticos_calidad_som.csv", row.names = FALSE)
cat("Los estadísticos de calidad del SOM se han guardado en 'estadisticos_calidad_som.csv'.\n")

# Visualización de la distribución de observaciones por celda
par(mfrow = c(2, 2))
plot(trained_som, type = "changes", main = "Cambios durante el Entrenamiento")
plot(trained_som, type = "count", main = "Frecuencia de Observaciones por Celda")
plot(trained_som, type = "quality", main = "Calidad del SOM (Errores de Cuantización)")
plot(trained_som, type = "dist.neighbours", main = "Distancias entre Celdas (U-matrix)")
par(mfrow = c(1, 1))

# Extraer y mostrar la frecuencia de observaciones por celda
freq_observations <- table(trained_som$unit.classif)
freq_observations_df <- as.data.frame(freq_observations)
colnames(freq_observations_df) <- c("Cell", "Observations")
print("Frecuencia de Observaciones por Celda:")
print(freq_observations_df)

# Guardar la frecuencia de observaciones por celda en un archivo CSV
write.csv(freq_observations_df, "frecuencia_observaciones.csv", row.names = FALSE)

# Clustering jerárquico para definir superceldas
distances <- dist(trained_som$codes[[1]], method = "euclidean")
hclust_results <- hclust(distances, method = "ward.D2")
superclasses <- cutree(hclust_results, k = 3)  # Configurar el número de superceldas si es necesario

# Añadir las celdas asignadas a cada observación y la superclase correspondiente
data_clean$SOM.cell <- trained_som$unit.classif
data_clean$Superclass <- superclasses[trained_som$unit.classif]

# Guardar los resultados en un archivo CSV
write.csv(data_clean, "modelo_som_resultados.csv", row.names = FALSE)
cat("Los resultados del modelo SOM, incluyendo celdas y superclases, se han guardado en 'modelo_som_resultados.csv'.\n")



# Visualizar los errores de cuantización para cada combinación de filas y columnas
ggplot(results, aes(x = interaction(rows, columns), y = quant_error)) +
  geom_bar(stat = "identity") +
  labs(x = "Combinación de Filas y Columnas", y = "Error de Cuantización", 
       title = "Errores de Cuantización para Diferentes Combinaciones de Filas y Columnas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Mapa por cada una de las variables
# Establecer la semilla para reproducibilidad
set.seed(805408)

# Número de variables a analizar individualmente
variables <- cols_to_scale

# Configuración del layout: 4 columnas y filas necesarias para acomodar todas las variables
num_columns <- 4
num_rows <- ceiling(length(variables) / num_columns)
par(mfrow = c(num_rows, num_columns))  # Configura el layout de gráficos

# Generar un SOM para cada variable y visualizarla
for (variable in variables) {
  # Extraer los datos de la variable actual y convertir a matriz
  variable_data <- as.matrix(data_clean[, variable, drop = FALSE])
  
  # Entrenar el SOM en una grilla 3x2 para esta variable
  som_model <- kohonen::som(
    variable_data,
    grid = kohonen::somgrid(3, 2, "hexagonal"),
    maxNA.fraction = 0.25,
    rlen = 200,
    alpha = c(0.05, 0.01),
    radius = c(2, -1.33),
    dist.fcts = "sumofsquares"
  )
  
  # Graficar el SOM de la variable
  plot(som_model, type = "property", property = variable_data, main = paste("Mapa de", variable))
}

# Volver al layout de gráficos normal
par(mfrow = c(1, 1))


