base_cancer <- data_cancer[, 2:8]
rownames(base_cancer) <- data_cancer$Jurisdiction
base_cancer <- base_cancer[-1, ]


pca <- PCA(base_cancer, ncp = 3) # Cuantas dimensiones queremos obtener 


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
# grupos <- cutree(arbol,3)
# 
# 
# indices <- pca$var$coord %>%data.frame()
# indices[, 'grupo'] <- factor(grupos)
# 
# ggplot(data = indices, aes(x = Dim.1, y = Dim.2, color = grupo))+
#   geom_point()
# 
# 
# grupos2 <- kmeans(indices, 3)
# indices[, 'grupos2'] <- factor(grupos2$cluster)
# 
# ggplot(data = indices, aes(x = Dim.1, y = Dim.2, color = grupos2))+
#   geom_point()


# Tabla de contingencia ---------------------------------------------------

Total_Males <- table(base_cancer$Total, base_cancer$Males)
Total_Females <- table(base_cancer$Total, base_cancer$Males)
Total_Blacks <- table(base_cancer$Total, base_cancer$Males)
Total_Whites <- table(base_cancer$Total, base_cancer$Males)
Total_Males <- table(base_cancer$Total, base_cancer$Males)



chisq.test(Total_Males)
chisq.test(Total_Females)
chisq.test(Total_Blacks)
chisq.test(Total_Whites)

fisher.test(Total_Blacks)



# Sin los valores Nulos----------------------------------------------------------------------------------------


base_cancer2 <- na.omit(base_cancer)
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
Total_Females <- table(base_cancer2$Total, base_cancer2$Males)
Total_Blacks <- table(base_cancer2$Total, base_cancer2$Males)
Total_Whites <- table(base_cancer2$Total, base_cancer2$Males)
Total_Males <- table(base_cancer2$Total, base_cancer2$Males)



chisq.test(Total_Males)
chisq.test(Total_Females)
chisq.test(Total_Blacks)
chisq.test(Total_Whites)
