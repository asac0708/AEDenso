getwd()
setwd("C:/Users/sofia/Documents/GitHub/AEDenso/animales/src/")

# Instalar y cargar paquetes necesarios
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("dplyr")

library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(gplots)

datos <- read.csv("../input/Listado_oficial_de_las_especies_silvestres_amenazadas_de_la_diversidad_biol_gica_colombiana_continental_y_marino_costera_-_Resoluci_n_0126_de_2024_20240526.csv")

tabla_familia <- table(datos$FAMILIA,datos$ESTADO.DE.AMENAZA)

tabla_orden <- table(datos$ORDEN,datos$ESTADO.DE.AMENAZA)

tabla_clase <- table(datos$CLASE,datos$ESTADO.DE.AMENAZA)


balloonplot(t(tabla_familia))
balloonplot(t(tabla_orden))
balloonplot(t(tabla_clase))

chisq.test(tabla_orden)
chisq.test(tabla_familia)

uni.mca <- CA(tabla_clase, graph= TRUE)

#Analisis de correspondencias multiples
fit2 <- MCA(datos[c(7,8,20)], graph = TRUE)
summary(fit2)
fviz_screeplot(fit2,addlabels = TRUE)
fviz_mca(fit2,axes=c(1,2))
