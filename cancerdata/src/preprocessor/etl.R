base_cancer <- data_cancer[, 2:8] # Quitando la columna de las jurisdicciones
rownames(base_cancer) <- data_cancer$Jurisdiction # Cambiarle el nombre a las filas
base_cancer <- base_cancer[-1, ] #Quitandole variables que alteran los resultados
data_cancer <- data_cancer[-1, ] #Quitandole variables que alteran los resultados
rownames(base_cancer) <- data_cancer$Jurisdiction # Cambiarle el nombre a las filas

pca <- PCA(base_cancer) # Cuantas dimensiones queremos obtener 


# Valores propios ---------------------------------------------------------

pca$eig
plot(pca$eig[,1], pch = 20, type = 'b')


# Variables ---------------------------------------------------------------

pca$var$cor

pca$var$contrib

plot(pca$var$cos2)


# Individuos --------------------------------------------------------------

pca$ind$contrib


# Agrupamiento ------------------------------------------------------------

distancias <- dist(pca$var$coord)
arbol <- hclust(distancias)
plot(arbol)



# Tabla de contingencia ---------------------------------------------------

Total_Males <- table(base_cancer$Total, base_cancer$Males)
Total_Females <- table(base_cancer$Total, base_cancer$Females)
Total_Blacks <- table(base_cancer$Total, base_cancer$Blacks)
Total_Whites <- table(base_cancer$Total, base_cancer$Whites)



chisq.test(Total_Males)
chisq.test(Total_Females)
chisq.test(Total_Blacks)
chisq.test(Total_Whites)



# Sin los valores Nulos----------------------------------------------------------------------------------------


data_cancer2 <- na.omit(data_cancer)
base_cancer2 <- data_cancer2[, 2:8] # Quitando la columna de las jurisdicciones
rownames(base_cancer2) <- data_cancer2$Jurisdiction # Cambiarle el nombre a las filas
pca <- PCA(base_cancer2, ncp = 3)  

# Valores propios

pca$eig
plot(pca$eig[,1], pch = 20, type = 'b')

# Variables

pca$var$cor
pca$var$contrib
plot(pca$var$cos2)

# Individuos 

pca$ind$contrib

# Agrupamiento

distancias <- dist(pca$var$coord)
arbol <- hclust(distancias)
plot(arbol)


# Tabla de contingencia

Total_Males <- table(base_cancer2$Total, base_cancer2$Males)
Total_Females <- table(base_cancer2$Total, base_cancer2$Females)
Total_Blacks <- table(base_cancer2$Total, base_cancer2$Blacks)
Total_Whites <- table(base_cancer2$Total, base_cancer2$Whites)

chisq.test(Total_Males)
chisq.test(Total_Females)
chisq.test(Total_Blacks)
chisq.test(Total_Whites)
