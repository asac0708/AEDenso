# Correlaciones -----------------------------------------------------------

# Seleccionar las columnas de la 2 a la 6
datos_seleccionados <- tabla_unida[,2:5]

# Calcular la matriz de correlaciones
matriz_correlaciones <- cor(datos_seleccionados, use = "complete.obs")

# PCA ---------------------------------------------------------------------
# Eliminar filas con valores NA
datos_limpios <- na.omit(datos_seleccionados)

# Estandarizar los datos limpios
datos_estandarizados <- scale(datos_limpios)

# Realizar el PCA
pca_resultado <- prcomp(datos_estandarizados, center = TRUE, scale. = TRUE)

# Mostrar un resumen del resultado y su grafica
summary(pca_resultado)
plot(pca_resultado,type="l")

#Mostrar resumen y vectores
biplot(pca_resultado, scale=0)

# Interpretacion APC a nivel pais -----------------------------------------

# Calcular loadings de países en los componentes principales
loadings_paises <- pca_resultado$rotation[, 1:2]
loadings_paises
# Crear un dataframe con los loadings de los países
data_paises <- as.data.frame(loadings_paises)
data_paises$Country <- rownames(data_paises)  # Agregar nombres de países como columna

# Graficar
ggplot(data_paises, aes(x = PC1, y = PC2, label = Country)) +
  geom_point() +
  geom_text(size = 3, vjust = 1.5) +  # Agregar etiquetas de países
  labs(x = "Componente Principal 1", y = "Componente Principal 2", title = "Gráfico de Dispersión de Países en ACP")

pc1_economia <- apply(pca_resultado$rotation[,1]*datos_limpios,1,sum)
pc2_economia <- apply(pca_resultado$rotation[,2]*datos_limpios,1,sum)

datos_limpios$pc1_economia <- pc1_economia
datos_limpios$pc2_economia <- pc2_economia

plot(datos_limpios$pc1_economia,datos_limpios$pc2_economia)

tabla_economia <- datos_limpios
write.csv(tabla_economia, "tabla_economia.csv", row.names = TRUE)
