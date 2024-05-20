
# Estadística descriptiva -------------------------------------------------

# Histogramas -------------------------------------------------------------

# Grafico de barras negocios verdes registrados por año
ggplot(datos_filtrados_negocios_años_tot, aes(x = as.character(`AÑO...Año.de.registro.`))) +
  geom_bar(fill = "darkblue") +
  labs(title = "Número de Negocios Verdes Registrados por Año",
       x = "Año",
       y = "Número de Negocios") +
  theme_minimal()

#Para comparar la poblacion por departamento con los negocios verdes se escogen los años con más datos y correspondecia en la tabla del dane: 2020 y 2021



# Grafico de barras Regiones
# Corregir Eje Cafetero y antioquia
ggplot(datos_filtrados_negocios, aes(x = `REGIÓN..Donde.se.encuentra.el.Negocio.Verde.`)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Número de Negocios Verdes por Región",
       x = "Región",
       y = "Número de Negocios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x para mejor legibilidad

#Grafico de barras departamento
ggplot(datos_filtrados_negocios, aes(x = `DEPARTAMENTO..Donde.se.encuentra.el.Negocio.Verde.`)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Número de Negocios Verdes por Departamento",
       x = "Departamento",
       y = "Número de Negocios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x para mejor legibilidad

#Grafico de barras por categoria
#En mercados de carbono hay poco y nada
ggplot(datos_filtrados_negocios, aes(x = `CATEGORÍA..Del.Negocio.Verde.`)) +
  geom_bar(fill = "beige") +
  labs(title = "Número de Negocios Verdes por Categoría",
       x = "Categoría",
       y = "Número de Negocios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x para mejor legibilidad

#Grafico de barras por sector
#Condensar numeros pequeños
ggplot(datos_filtrados_negocios, aes(x = `SECTOR..Al.cual.pertenece.el.Negocio.Verde.`)) +
  geom_bar(fill = "green") +
  labs(title = "Número de Negocios Verdes por Sector",
       x = "Sector",
       y = "Número de Negocios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x para mejor legibilidad

#Grafico de barras por categoria comercial
ggplot(datos_filtrados_negocios, aes(x = `CATEGORÍA.COMERCIAL..Al.cual.pertenece.el.Negocio.Verde.`)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Número de Negocios Verdes por Categoría Comercial",
       x = "Categoría Comercial",
       y = "Número de Negocios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))  # Rotar etiquetas del eje x para mejor legibilidad


# Graficos per capita -----------------------------------------------------
ggplot(conteo_departamentos, aes(x = reorder(DEPARTAMENTO..Donde.se.encuentra.el.Negocio.Verde., Negocios_per_capita), y = Negocios_per_capita)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(x = "Departamento", y = "Negocios per cápita", title = "Negocios per cápita por departamento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()
# Crear el gráfico de barras con diferenciación por año
ggplot(conteo_departamentos, aes(x = reorder(DEPARTAMENTO..Donde.se.encuentra.el.Negocio.Verde., Negocios_per_capita), y = Negocios_per_capita, fill = factor(AÑO...Año.de.registro.))) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Departamento", y = "Negocios per cápita", title = "Negocios per cápita por departamento (por año)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Filtrar datos para el año 2020
conteo_departamentos_2020 <- subset(conteo_departamentos, AÑO...Año.de.registro. == 2020)

# Crear el gráfico de barras para 2020
ggplot(conteo_departamentos_2020, aes(x = reorder(DEPARTAMENTO..Donde.se.encuentra.el.Negocio.Verde., Negocios_per_capita), y = Negocios_per_capita, fill = factor(AÑO...Año.de.registro.))) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Departamento", y = "Negocios per cápita", title = "Negocios per cápita por departamento (2020)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Filtrar datos para el año 2021
conteo_departamentos_2021 <- subset(conteo_departamentos, AÑO...Año.de.registro. == 2021)

# Crear el gráfico de barras para 2021
ggplot(conteo_departamentos_2021, aes(x = reorder(DEPARTAMENTO..Donde.se.encuentra.el.Negocio.Verde., Negocios_per_capita), y = Negocios_per_capita, fill = factor(AÑO...Año.de.registro.))) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Departamento", y = "Negocios per cápita", title = "Negocios per cápita por departamento (2021)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


