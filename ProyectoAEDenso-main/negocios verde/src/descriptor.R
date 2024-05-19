
# Estadística descriptiva -------------------------------------------------

# Histogramas -------------------------------------------------------------

# Grafico de barras negocios verdes registrados por año
ggplot(datos_filtrados_negocios, aes(x = as.character(`AÑO...Año.de.registro.`))) +
  geom_bar(fill = "darkblue") +
  labs(title = "Número de Negocios Verdes Registrados por Año",
       x = "Año",
       y = "Número de Negocios") +
  theme_minimal()

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
#Corregir Boyaca y san andres
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

