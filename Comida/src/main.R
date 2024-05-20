#install.packages("summarytools")
library(summarytools)
library(ggplot2)


getwd()
setwd("C:/Users/sofia/Documents/Hackaton Datos/Comida/src/")


datos_comida <- read.csv("../input/epi_r.csv")

colnames(datos_comida)

unique(datos_comida)


# Filtrar las filas donde 'calories' es mayor que 500
datos_filtrados <- datos_comida[datos_comida$rating == 5.0, ]


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
  if (media_columna > 0.75) {
    # Agregar el nombre de la columna a la lista
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
datos_limpios <- subset(datos_filtrados, select = 1:6)

# Mostrar las primeras filas de datos_limpios para verificar
head(datos_limpios)

# Eliminar filas con NA en las columnas relevantes
datos_limpios <- datos_limpios[complete.cases(datos_limpios[, c('calories', 'protein', 'fat', 'sodium')]), ]

# Crear un histograma para las calorías
ggplot(datos_limpios, aes(x = calories)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Calorías")

# Crear un histograma para la proteína
ggplot(datos_limpios, aes(x = protein)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribución de Proteína")

# Crear un histograma para la grasa
ggplot(datos_limpios, aes(x = fat)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black") +
  labs(title = "Distribución de Grasa")

# Crear un histograma para el sodio
ggplot(datos_limpios, aes(x = sodium)) +
  geom_histogram(binwidth = 50, fill = "orange", color = "black") +
  labs(title = "Distribución de Sodio")

#Hay datos muy atipicos en la tabla, por lo cual se tomarán solo el cuantil 95.

quantile(datos_limpios$calories, 0.95)

# Filtrar los datos para incluir solo aquellos dentro del cuantil 95
datos_limpios_cuantil_95 <- datos_limpios[apply(datos_limpios[, 3:ncol(datos_limpios)], 1, function(row) {
  all(row <= 1407)
}), ]
