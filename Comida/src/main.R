library(ggplot2)
library(lmtest)
library(dplyr)
library(tidyr)


getwd()
setwd("C:/Users/prestamour/Documents/GitHub/AEDenso/Comida/src")
datos_comida <- read.csv("../input/epi_r.csv")

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

# Calcular la media de la columna actual
media_columna <- mean(datos_filtrados[[nombre_columna]])

# Verificar si la media es mayor a 0.75
if (media_columna > 0.5) {
  # Agregar el nombre de la columna a la lista
  print(media_columna)
  columnas_altas_media <- c(columnas_altas_media, nombre_columna)
}


# Imprimir las columnas con medias mayores a 0.75
if (length(columnas_altas_media) > 0) {
  cat("Las siguientes columnas tienen una media mayor a 0.5:\n")
  print(columnas_altas_media)
} else {
  cat("No hay columnas con una media mayor a 0.5.\n")
}

# CONCLUSION DEL PRIMER ACERCAMIENTO:
#Al ninguna de las columnas después de la 6ta tener una media mayor al 75%, se puede concluir que estas variables NO poseen relación con el ranking.

# INTENTO 2:

# Vamos a intentar hacer una tabla con las columnas que tengan la mayor cantidad de datos independientemente del rating

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

ggplot(tabla_recuento, aes(x = as.factor(rating), y = bon.appÃ.tit)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Fat vs Rating",
       x = "Rating",
       y = "Fat") +
  theme_minimal()

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

# Asumiendo que tienes un dataframe llamado 'datos_filtrados'

# Seleccionar las columnas cuyo valor en la fila 9 sea menor a 500
columnas_seleccionadas <- names(datos_filtrados)[sapply(datos_filtrados[9, ], function(x) all(x > 500))]

# Filtrar el dataframe para mantener solo las columnas seleccionadas
datos_filtrados <- select(datos_filtrados, all_of(columnas_seleccionadas))

# Verificar el resultado
print(datos_filtrados)

# Eliminar la fila número 10
datos_filtrados <- datos_filtrados[-10, ]
# Eliminar la fila número 9
datos_filtrados <- datos_filtrados[-9, ]

# Variables importantes y sus graficos:
ggplot(datos_filtrados, aes(x = as.factor(rating), y = num_bon.appÃ.tit)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Bon ápetit vs Rating",
       x = "Rating",
       y = "Bon ápetit") +
  theme_minimal()

ggplot(datos_filtrados, aes(x = as.factor(rating), y = num_tree.nut.free)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Tree nut free vs Rating",
       x = "Rating",
       y = "Tree nut free") +
  theme_minimal()

ggplot(datos_filtrados, aes(x = as.factor(rating), y = num_wheat.gluten.free)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Wheat gluten free vs Rating",
       x = "Rating",
       y = "Wheat gluten free") +
  theme_minimal()

# MODELO FINAL ------------------------------------------------------------

modeloF <- lm(rating ~ num_bon.appÃ.tit + num_tree.nut.free + num_wheat.gluten.free, data = datos_filtrados)

summary(modeloF)
