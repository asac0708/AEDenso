#install.packages("summarytools")
library(summarytools)
library(ggplot2)
library(lmtest)
library(dplyr)
library(tidyr)


getwd()
setwd("C:/Users/sofia/Documents/GitHub/AEDenso/Comida/src")


datos_comida <- read.csv("../input/epi_r.csv")



# Filtrar las filas donde el rating es de 5.0 para tener
# mejor conocimiento de los datos objetivo
datos_filtrados <- datos_comida[datos_comida$rating >= 4.2, ]


# Primer acercamiento -----------------------------------------------------

# Crear una lista para almacenar las columnas con medias mayores a 0.75
columnas_altas_media <- c()

# Obtener el número de columnas en datos_filtrados
num_columnas <- ncol(datos_filtrados)

# Recorrer las columnas de la séptima en adelante
for (i in 7:num_columnas) {
  # Nombre de la columna actual
  nombre_columna <- colnames(datos_filtrados)[i]
  
  # Calcular la media de la columna actual
  media_columna <- mean(datos_filtrados[[nombre_columna]])
  
  # Verificar si la media es mayor a 0.75
  if (media_columna > 0.5) {
    # Agregar el nombre de la columna a la lista
    print(media_columna)
    columnas_altas_media <- c(columnas_altas_media, nombre_columna)
  }
}

# Imprimir las columnas con medias mayores a 0.75
if (length(columnas_altas_media) > 0) {
  cat("Las siguientes columnas tienen una media mayor a 0.75:\n")
  print(columnas_altas_media)
} else {
  cat("No hay columnas con una media mayor a 0.75.\n")
}

# CONCLUSION DEL PRIMER ACERCAMIENTO:
#Al ninguna de las columnas después de la 6ta tener una media mayor al 75%, se puede concluir que estas variables NO poseen relación con el ranking.


# Segundo acercamiento ----------------------------------------------------

# Crear una versión limpia de datos_filtrados sin las columnas innecesarias
datos_limpios <- datos_comida %>% select(1:6, `bon.appétit`)


# Eliminar filas con NA en las columnas relevantes
datos_limpios <- datos_limpios[complete.cases(datos_limpios[, c('calories', 'protein', 'fat', 'sodium')]), ]

# Crear un histograma para las calorías
ggplot(datos_limpios, aes(x = calories)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Calorías")

#Hay datos muy atipicos en la tabla, por lo cual se tomarán solo el cuantil 95.

# Filtrar los datos para incluir solo aquellos dentro del cuantil 95
datos_limpios_cuantil_95 <- datos_limpios[apply(datos_limpios[, 3:ncol(datos_limpios)], 1, function(row) {
  all(row <= 1407)
}), ]


# Analisis descriptivo ----------------------------------------------------

# Mostrar un resumen descriptivo de las variables
summary(datos_limpios_cuantil_95)

# Crear un histograma para las calorías
ggplot(datos_limpios_cuantil_95, aes(x = calories)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Calorías")

# Crear un histograma para la proteína
ggplot(datos_limpios_cuantil_95, aes(x = protein)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribución de Proteína")

# Crear un histograma para la grasa
ggplot(datos_limpios_cuantil_95, aes(x = fat)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black") +
  labs(title = "Distribución de Grasa")


# Modelos -----------------------------------------------------------------

# Ajustar el modelo de regresión lineal Todas las variables
modelo <- lm(rating ~ bon, data = datos_limpios_cuantil_95)

# Resumen del modelo
summary(modelo)

# Prueba de normalidad de los residuos
shapiro.test(residuals(modelo))

# Prueba de homocedasticidad
bptest(modelo)

# Gráficos de diagnóstico del modelo
par(mfrow = c(2, 2))
plot(modelo)

datos_filtrados <- datos_limpios_cuantil_95[datos_limpios_cuantil_95$rating >= 5, ]


# Crear el gráfico de dispersión
ggplot(datos_filtrados, aes(x = calories, y = fat, color = `rating`)) +
  geom_point() +
  labs(title = "Scatter Plot of Rating vs. Fat",
       x = "calories",
       y = "Fat") +
  theme_minimal()
# Crear el gráfico de dispersión
ggplot(datos_filtrados, aes(x = sodium, y = calories, color = `bon.appétit`)) +
  geom_point() +
  labs(title = "Scatter Plot of Rating vs. Fat",
       x = "Rating",
       y = "Fat") +
  theme_minimal()

ggplot(datos_limpios_cuantil_95, aes(x = as.factor(rating), y = fat)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Fat vs Rating",
       x = "Rating",
       y = "Fat") +
  theme_minimal()

datos_comida[7:ncol(datos_comida)] <- lapply(datos_comida[7:ncol(datos_comida)], as.numeric)

# Seleccionar la columna 'rating' y las columnas binarias (de la columna 7 en adelante)
datos_binarios <- datos_comida %>%
  select(rating, 7:ncol(datos_comida))

# Verificar la estructura del dataframe
str(datos_binarios)

# Crear la tabla de recuentos
tabla_recuento <- datos_binarios %>%
  gather(key = "variable", value = "value", -rating) %>%
  group_by(rating, variable) %>%
  summarise(recuento = sum(value, na.rm = TRUE)) %>%
  spread(key = "variable", value = "recuento", fill = 0)

# Verificar el resultado
print(tabla_recuento)

ggplot(tabla_recuento, aes(x = as.factor(rating), y = bon.appétit)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Fat vs Rating",
       x = "Rating",
       y = "Fat") +
  theme_minimal()
# Supongamos que 'datos_comida' es tu dataframe original


# Convertir columnas a numéricas si es posible y aplicar el filtro
filtro_columnas <- function(df) {
  # Convertir columnas a numéricas si es posible
  df <- df %>% mutate(across(everything(), ~ suppressWarnings(as.numeric(.)), .names = "num_{col}"))
  
  # Filtrar solo las columnas convertidas a numéricas
  df_num <- df %>% select(starts_with("num_"))
  
  # Aplicar la condición de filtrado
  df_num %>% select_if(function(col) sum(col[1:6], na.rm = TRUE) < sum(col[(nrow(df_num)-1):nrow(df_num)], na.rm = TRUE))
}

# Aplicar la función al dataframe
datos_filtrados <- filtro_columnas(tabla_recuento)

# Verificar el resultado
str(datos_filtrados)


sum_columnas <- colSums(datos_filtrados[, sapply(datos_filtrados, is.numeric)], na.rm = TRUE)
nueva_fila <- data.frame(matrix(sum_columnas, nrow = 1))
colnames(nueva_fila) <- colnames(datos_filtrados)[sapply(datos_filtrados, is.numeric)]
nueva_fila <- nueva_fila[colnames(datos_filtrados)]
datos_filtrados <- bind_rows(datos_filtrados, nueva_fila)
promedio_suma_filas <- mean(as.numeric(nueva_fila[1, sapply(nueva_fila, is.numeric)]), na.rm = TRUE)


# Seleccionar las columnas cuyo valor en la fila 9 sea menor a 500
columnas_seleccionadas <- names(datos_filtrados[, 9][datos_filtrados[9,] < 500])

# Filtrar el dataframe para mantener solo las columnas seleccionadas
datos_filtrados_nuevo <- select(datos_filtrados, all_of(columnas_seleccionadas))

# Verificar el resultado
print(datos_filtrados)

