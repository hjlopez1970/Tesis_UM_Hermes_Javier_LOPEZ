# Cargar e instalar paquetes necesarios
required_packages <- c("aweSOM", "kohonen", "data.table", "ggplot2")
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}
lapply(required_packages, library, character.only = TRUE)

# Configurar el directorio de trabajo y cargar el conjunto de datos
setwd("C:/Users/Javier/Desktop/PBA 2022/10 Tesis/Tesis_UM_Hermes_Javier_LOPEZ/Data")


# Primer Modelo Semilla 805408 y 6 celdas
# Parte 1 - Importar datos y configurar semilla
import.data <- fread("indicadores_financieros_1.csv", stringsAsFactors = TRUE, data.table = FALSE, check.names = TRUE)

seed_value <- 805408
set.seed(seed_value)
cat("Semilla utilizada:", seed_value, "\n\n")

# Seleccionar y escalar columnas de interés
cols_to_scale <- c("ROA", "ROE", "Apalancamiento_Global", "Deterioros_Vencidos", "Cobertura_Morosidad", 
                   "Tasa_Morosidad", "Credito_Activo", "Deterioro_Credito", "Ratio_Eficiencia", 
                   "Intereses_C_Vigente", "Comisiones_C_Vigente", "Comisiones_perd_C_Vigente", 
                   "Incobrables_C_Vigente", "Gastos_C_Vigente", "GPersonal_C_Vigente", 
                   "Ctos_Ints_C_Vigente", "Resultados_C_Vigente", "Incob_C_Bruta", "Incob_C_Ints", "Ingr_C_Bruta")

# Escalar y eliminar filas con NA en las columnas seleccionadas
import.data[cols_to_scale] <- lapply(import.data[cols_to_scale], as.numeric)
data_clean <- import.data[complete.cases(import.data[, cols_to_scale]), ]
data_matrix <- scale(as.matrix(data_clean[, cols_to_scale]))

# Mostrar cantidad total de observaciones después de eliminar NAs
cat("Cantidad total de observaciones en el conjunto de datos:", nrow(data_clean), "\n")

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

# Calcular métricas de calidad del SOM
# Quantization error
quantization_error <- mean(trained_som$distances)
cat("Quantization Error:", quantization_error, "\n")

# Explained variance
total_variance <- sum(apply(data_matrix, 2, var))
residual_variance <- mean(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2))
explained_variance <- (1 - residual_variance / total_variance) * 100
cat("Explained Variance (%):", explained_variance, "\n")

# Topographic error
topographic_error <- mean(trained_som$unit.distances > 1, na.rm = TRUE)
if (is.nan(topographic_error)) {
  topographic_error <- 0
}
cat("Topographic Error (adjusted):", topographic_error, "\n")

# Kaski-Lagus error
kaski_lagus_error <- mean(trained_som$distances / sqrt(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2)))
cat("Kaski-Lagus Error:", kaski_lagus_error, "\n")

# Guardar las métricas de calidad en un archivo CSV
quality_measures_df <- data.frame(
  Metric = c("Quantization Error", "Explained Variance (%)", "Topographic Error", "Kaski-Lagus Error"),
  Value = c(quantization_error, explained_variance, topographic_error, kaski_lagus_error)
)
write.csv(quality_measures_df, "estadisticos_calidad_som.csv", row.names = FALSE)
cat("Los estadísticos de calidad del SOM se han guardado en 'estadisticos_calidad_som.csv'.\n")

# Clustering jerárquico para definir superceldas
distances <- dist(trained_som$codes[[1]], method = "euclidean")
hclust_results <- hclust(distances, method = "ward.D2")
superclasses <- cutree(hclust_results, k = 3)

# Añadir celdas y superclase a los datos originales
data_clean$SOM.cell <- trained_som$unit.classif
data_clean$Superclass <- superclasses[trained_som$unit.classif]

# Guardar el archivo actualizado con Superclass y SOM.cell
write.csv(data_clean, "aweSOM_06_805408.csv", row.names = FALSE)
cat("Los resultados del modelo SOM, incluyendo celdas y superclases, se han guardado en 'aweSOM_06_805408.csv'.\n")

# Visualización del SOM con aweSOMplot ajustado
aweSOMplot(som = trained_som, type = "Cloud", data = data_clean,
           variables = c("Short_name"), superclass = superclasses, 
           obsNames = data_clean[, "Short_name_año"],  # Usar "Short_name" y año en la leyenda         
             palvar = "rainbow", legendFontsize = 12, size = 615)



# Mostrar cantidad de observaciones por celda
obs_count_per_cell <- table(trained_som$unit.classif)
cat("Cantidad de observaciones en cada celda:\n")
print(as.data.frame(obs_count_per_cell))





# Segundo Modelo Semilla 805408 y 8 celdas
# Importar datos y configurar semilla
import.data <- fread("indicadores_financieros_1.csv", stringsAsFactors = TRUE, data.table = FALSE, check.names = TRUE)

seed_value <- 805408
set.seed(seed_value)
cat("Semilla utilizada:", seed_value, "\n\n")

# Seleccionar y escalar columnas de interés
cols_to_scale <- c("ROA", "ROE", "Apalancamiento_Global", "Deterioros_Vencidos", "Cobertura_Morosidad", 
                   "Tasa_Morosidad", "Credito_Activo", "Deterioro_Credito", "Ratio_Eficiencia", 
                   "Intereses_C_Vigente", "Comisiones_C_Vigente", "Comisiones_perd_C_Vigente", 
                   "Incobrables_C_Vigente", "Gastos_C_Vigente", "GPersonal_C_Vigente", 
                   "Ctos_Ints_C_Vigente", "Resultados_C_Vigente", "Incob_C_Bruta", "Incob_C_Ints", "Ingr_C_Bruta")

# Escalar y eliminar filas con NA en las columnas seleccionadas
import.data[cols_to_scale] <- lapply(import.data[cols_to_scale], as.numeric)
data_clean <- import.data[complete.cases(import.data[, cols_to_scale]), ]
data_matrix <- scale(as.matrix(data_clean[, cols_to_scale]))

# Mostrar cantidad total de observaciones después de eliminar NAs
cat("Cantidad total de observaciones en el conjunto de datos:", nrow(data_clean), "\n")

# Inicializar y entrenar el SOM en una grilla de 3x2 (hexagonal)
som_grid <- kohonen::somgrid(4, 2, "hexagonal")
trained_som <- kohonen::som(
  data_matrix,
  grid = som_grid,
  maxNA.fraction = 0.25,
  rlen = 200,
  alpha = c(0.05, 0.01),
  radius = c(2, -1.33),  # Parámetros ajustados
  dist.fcts = "sumofsquares"
)

# Calcular métricas de calidad del SOM
# Quantization error
quantization_error <- mean(trained_som$distances)
cat("Quantization Error:", quantization_error, "\n")

# Explained variance
total_variance <- sum(apply(data_matrix, 2, var))
residual_variance <- mean(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2))
explained_variance <- (1 - residual_variance / total_variance) * 100
cat("Explained Variance (%):", explained_variance, "\n")

# Topographic error
topographic_error <- mean(trained_som$unit.distances > 1, na.rm = TRUE)
if (is.nan(topographic_error)) {
  topographic_error <- 0
}
cat("Topographic Error (adjusted):", topographic_error, "\n")

# Kaski-Lagus error
kaski_lagus_error <- mean(trained_som$distances / sqrt(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2)))
cat("Kaski-Lagus Error:", kaski_lagus_error, "\n")

# Guardar las métricas de calidad en un archivo CSV
quality_measures_df <- data.frame(
  Metric = c("Quantization Error", "Explained Variance (%)", "Topographic Error", "Kaski-Lagus Error"),
  Value = c(quantization_error, explained_variance, topographic_error, kaski_lagus_error)
)
write.csv(quality_measures_df, "estadisticos_calidad_som.csv", row.names = FALSE)
cat("Los estadísticos de calidad del SOM se han guardado en 'estadisticos_calidad_som.csv'.\n")

# Clustering jerárquico para definir superceldas
distances <- dist(trained_som$codes[[1]], method = "euclidean")
hclust_results <- hclust(distances, method = "ward.D2")
superclasses <- cutree(hclust_results, k = 3)

# Añadir celdas y superclase a los datos originales
data_clean$SOM.cell <- trained_som$unit.classif
data_clean$Superclass <- superclasses[trained_som$unit.classif]

# Guardar el archivo actualizado con Superclass y SOM.cell
write.csv(data_clean, "aweSOM_08_805408.csv", row.names = FALSE)

cat("Los resultados del modelo SOM, incluyendo celdas y superclases, se han guardado en 'aweSOM_06_805408.csv'.\n")

# Visualización del SOM con aweSOMplot ajustado
aweSOMplot(som = trained_som, type = "Cloud", data = data_clean,
           variables = c("Short_name"), superclass = superclasses, 
           obsNames = data_clean[, "Short_name_año"],  # Usar "Short_name" y año en la leyenda         
           palvar = "rainbow", legendFontsize = 10, size = 615)

# Mostrar cantidad de observaciones por celda
obs_count_per_cell <- table(trained_som$unit.classif)
cat("Cantidad de observaciones en cada celda:\n")
print(as.data.frame(obs_count_per_cell))







# Tercer Modelo Semilla 805408 y 12 celdas
# Importar datos y configurar semilla
import.data <- fread("indicadores_financieros_1.csv", stringsAsFactors = TRUE, data.table = FALSE, check.names = TRUE)

seed_value <- 805408
set.seed(seed_value)
cat("Semilla utilizada:", seed_value, "\n\n")

# Seleccionar y escalar columnas de interés
cols_to_scale <- c("ROA", "ROE", "Apalancamiento_Global", "Deterioros_Vencidos", "Cobertura_Morosidad", 
                   "Tasa_Morosidad", "Credito_Activo", "Deterioro_Credito", "Ratio_Eficiencia", 
                   "Intereses_C_Vigente", "Comisiones_C_Vigente", "Comisiones_perd_C_Vigente", 
                   "Incobrables_C_Vigente", "Gastos_C_Vigente", "GPersonal_C_Vigente", 
                   "Ctos_Ints_C_Vigente", "Resultados_C_Vigente", "Incob_C_Bruta", "Incob_C_Ints", "Ingr_C_Bruta")

# Escalar y eliminar filas con NA en las columnas seleccionadas
import.data[cols_to_scale] <- lapply(import.data[cols_to_scale], as.numeric)
data_clean <- import.data[complete.cases(import.data[, cols_to_scale]), ]
data_matrix <- scale(as.matrix(data_clean[, cols_to_scale]))

# Mostrar cantidad total de observaciones después de eliminar NAs
cat("Cantidad total de observaciones en el conjunto de datos:", nrow(data_clean), "\n")

# Inicializar y entrenar el SOM en una grilla de 3x2 (hexagonal)
som_grid <- kohonen::somgrid(4, 3, "hexagonal")
trained_som <- kohonen::som(
  data_matrix,
  grid = som_grid,
  maxNA.fraction = 0.25,
  rlen = 200,
  alpha = c(0.05, 0.01),
  radius = c(2, -1.33),  # Parámetros ajustados
  dist.fcts = "sumofsquares"
)

# Calcular métricas de calidad del SOM
# Quantization error
quantization_error <- mean(trained_som$distances)
cat("Quantization Error:", quantization_error, "\n")

# Explained variance
total_variance <- sum(apply(data_matrix, 2, var))
residual_variance <- mean(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2))
explained_variance <- (1 - residual_variance / total_variance) * 100
cat("Explained Variance (%):", explained_variance, "\n")

# Topographic error
topographic_error <- mean(trained_som$unit.distances > 1, na.rm = TRUE)
if (is.nan(topographic_error)) {
  topographic_error <- 0
}
cat("Topographic Error (adjusted):", topographic_error, "\n")

# Kaski-Lagus error
kaski_lagus_error <- mean(trained_som$distances / sqrt(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2)))
cat("Kaski-Lagus Error:", kaski_lagus_error, "\n")

# Guardar las métricas de calidad en un archivo CSV
quality_measures_df <- data.frame(
  Metric = c("Quantization Error", "Explained Variance (%)", "Topographic Error", "Kaski-Lagus Error"),
  Value = c(quantization_error, explained_variance, topographic_error, kaski_lagus_error)
)
write.csv(quality_measures_df, "estadisticos_calidad_som.csv", row.names = FALSE)
cat("Los estadísticos de calidad del SOM se han guardado en 'estadisticos_calidad_som.csv'.\n")

# Clustering jerárquico para definir superceldas
distances <- dist(trained_som$codes[[1]], method = "euclidean")
hclust_results <- hclust(distances, method = "ward.D2")
superclasses <- cutree(hclust_results, k = 3)

# Añadir celdas y superclase a los datos originales
data_clean$SOM.cell <- trained_som$unit.classif
data_clean$Superclass <- superclasses[trained_som$unit.classif]

# Guardar el archivo actualizado con Superclass y SOM.cell
write.csv(data_clean, "aweSOM_12_805408.csv", row.names = FALSE)
cat("Los resultados del modelo SOM, incluyendo celdas y superclases, se han guardado en 'aweSOM_06_805408.csv'.\n")

# Visualización del SOM con aweSOMplot ajustado
aweSOMplot(som = trained_som, type = "Cloud", data = data_clean,
           variables = c("Short_name"), superclass = superclasses, 
           obsNames = data_clean[, "Short_name_año"],  # Usar "Short_name" y año en la leyenda         
           palvar = "rainbow", legendFontsize = 14, size = 615)

# Mostrar cantidad de observaciones por celda
obs_count_per_cell <- table(trained_som$unit.classif)
cat("Cantidad de observaciones en cada celda:\n")
print(as.data.frame(obs_count_per_cell))









# Cuarto Modelo  Modelo Semilla 805408 y 6 celdas con base completa 
# Parte 1 - Importar datos y configurar semilla
import.data <- fread("indicadores_financieros_0.csv", stringsAsFactors = TRUE, data.table = FALSE, check.names = TRUE)

seed_value <- 805408
set.seed(seed_value)
cat("Semilla utilizada:", seed_value, "\n\n")

# Seleccionar y escalar columnas de interés
cols_to_scale <- c("ROA", "ROE", "Apalancamiento_Global", "Deterioros_Vencidos", "Cobertura_Morosidad", 
                   "Tasa_Morosidad", "Credito_Activo", "Deterioro_Credito", "Ratio_Eficiencia", 
                   "Intereses_C_Vigente", "Comisiones_C_Vigente", "Comisiones_perd_C_Vigente", 
                   "Incobrables_C_Vigente", "Gastos_C_Vigente", "GPersonal_C_Vigente", 
                   "Ctos_Ints_C_Vigente", "Resultados_C_Vigente", "Incob_C_Bruta", "Incob_C_Ints", "Ingr_C_Bruta")

# Escalar y eliminar filas con NA en las columnas seleccionadas
import.data[cols_to_scale] <- lapply(import.data[cols_to_scale], as.numeric)
data_clean <- import.data[complete.cases(import.data[, cols_to_scale]), ]
data_matrix <- scale(as.matrix(data_clean[, cols_to_scale]))

# Mostrar cantidad total de observaciones después de eliminar NAs
cat("Cantidad total de observaciones en el conjunto de datos:", nrow(data_clean), "\n")

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

# Calcular métricas de calidad del SOM
# Quantization error
quantization_error <- mean(trained_som$distances)
cat("Quantization Error:", quantization_error, "\n")

# Explained variance
total_variance <- sum(apply(data_matrix, 2, var))
residual_variance <- mean(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2))
explained_variance <- (1 - residual_variance / total_variance) * 100
cat("Explained Variance (%):", explained_variance, "\n")

# Topographic error
topographic_error <- mean(trained_som$unit.distances > 1, na.rm = TRUE)
if (is.nan(topographic_error)) {
  topographic_error <- 0
}
cat("Topographic Error (adjusted):", topographic_error, "\n")

# Kaski-Lagus error
kaski_lagus_error <- mean(trained_som$distances / sqrt(rowSums((data_matrix - trained_som$codes[[1]][trained_som$unit.classif, ])^2)))
cat("Kaski-Lagus Error:", kaski_lagus_error, "\n")

# Guardar las métricas de calidad en un archivo CSV
quality_measures_df <- data.frame(
  Metric = c("Quantization Error", "Explained Variance (%)", "Topographic Error", "Kaski-Lagus Error"),
  Value = c(quantization_error, explained_variance, topographic_error, kaski_lagus_error)
)
write.csv(quality_measures_df, "estadisticos_calidad_som.csv", row.names = FALSE)
cat("Los estadísticos de calidad del SOM se han guardado en 'estadisticos_calidad_som.csv'.\n")

# Clustering jerárquico para definir superceldas
distances <- dist(trained_som$codes[[1]], method = "euclidean")
hclust_results <- hclust(distances, method = "ward.D2")
superclasses <- cutree(hclust_results, k = 3)

# Añadir celdas y superclase a los datos originales
data_clean$SOM.cell <- trained_som$unit.classif
data_clean$Superclass <- superclasses[trained_som$unit.classif]

# Guardar el archivo actualizado con Superclass y SOM.cell
write.csv(data_clean, "aweSOM_06_805408.csv", row.names = FALSE)
cat("Los resultados del modelo SOM, incluyendo celdas y superclases, se han guardado en 'aweSOM_06_805408_2003.csv'.\n")

# Visualización del SOM con aweSOMplot ajustado
aweSOMplot(som = trained_som, type = "Cloud", data = data_clean,
           variables = c("Short_name"), superclass = superclasses, 
           obsNames = data_clean[, "Short_name_año"],  # Usar "Short_name" y año en la leyenda         
           palvar = "rainbow", legendFontsize = 12, size = 615)



# Mostrar cantidad de observaciones por celda
obs_count_per_cell <- table(trained_som$unit.classif)
cat("Cantidad de observaciones en cada celda:\n")
print(as.data.frame(obs_count_per_cell))





