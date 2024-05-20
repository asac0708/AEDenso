#install.packages("summarytools")
library(summarytools)
library(ggplot2)
library(lmtest)



getwd()
setwd("C:/Users/jonec/Documents/Dev/AEDenso/Comida/src")


datos_comida <- read.csv("../input/epi_r.csv")

# Filtrar las filas donde el rating es de 5.0 para tener
# mejor conocimiento de los datos objetivo
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
datos_limpios <- subset(datos_comida, select = 1:6)

# Mostrar las primeras filas de datos_limpios para verificar
head(datos_limpios)

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
modelo <- lm(rating ~ fat, data = datos_limpios_cuantil_95)

# Resumen del modelo
summary(modelo)

# Prueba de normalidad de los residuos
shapiro.test(residuals(modelo))

# Prueba de homocedasticidad
bptest(modelo)

# Gráficos de diagnóstico del modelo
par(mfrow = c(2, 2))
plot(modelo)

# Gráfico de boxplot para sodium vs rating
ggplot(datos_limpios_cuantil_95, aes(x = as.factor(rating), y = sodium)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Sodium vs Rating",
       x = "Rating",
       y = "Sodium") +
  theme_minimal()

ggplot(datos_limpios_cuantil_95, aes(x = as.factor(rating), y = fat)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  labs(title = "Fat vs Rating",
       x = "Rating",
       y = "Fat") +
  theme_minimal()

