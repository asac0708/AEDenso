base_cancer <- data_cancer[, 2:8]
rownames(base_cancer) <- data_cancer$Jurisdiction
base_cancer <- base_cancer[-1, ]


pca <- PCA(base_cancer, ncp = 3) # Cuantas dimensiones queremos obtener 

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
grupos <- cutree(arbol,3)


indices <- pca$var$coord %>%data.frame()
indices[, 'grupo'] <- factor(grupos)

ggplot(data = indices, aes(x = Dim.1, y = Dim.2, color = grupo))+
  geom_point()


grupos2 <- kmeans(indices, 3)
indices[, 'grupos2'] <- factor(grupos2$cluster)

ggplot(data = indices, aes(x = Dim.1, y = Dim.2, color = grupos2))+
  geom_point()

# Tabla de contingencia

tabla_contingencia <- table(base_cancer$Total, base_cancer$Males)

tab <- table(base_cancer$Males, base_cancer$Females)


chisq.test(tab)




