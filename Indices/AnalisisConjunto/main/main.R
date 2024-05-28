library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

getwd()
setwd("C:/Users/sofia/Documents/GitHub/AEDenso/Indices/AnalisisConjunto/")

tabla_educacion <- read.csv("./input/tabla_educacion.csv")
tabla_economia <- read.csv("./input/tabla_economia.csv")
tabla_ambiente <- read.csv("./input/datos_ambiental.csv")
region_codes <- read_delim('./input/UNcodes.csv', delim = ';')

#Tablas solo con los indices
# Seleccionando la primera y las dos últimas columnas de cada tabla
tabla_educacion <- tabla_educacion[, c(1, ncol(tabla_educacion)-1, ncol(tabla_educacion))]
tabla_economia <- tabla_economia[, c(1, ncol(tabla_economia)-1, ncol(tabla_economia))]
tabla_ambiente <- tabla_ambiente[, c(1, ncol(tabla_ambiente)-1, ncol(tabla_ambiente))]


# Unir las tablas mediante la primera columna
merged_table <- merge(tabla_educacion, tabla_economia, by = "X",all=TRUE)
merged_table <- merge(merged_table, tabla_ambiente, by = "X",all=TRUE)

#Regiones
colnames(region_codes)
region_codes <- region_codes[, c("ISO-alpha3 Code", "Sub-region Name", "Region Name")]

# Unir las tablas mediante "ISO-alpha3 Code" y "X"
merged_table <- merge(merged_table, region_codes, by.x = "X", by.y = "ISO-alpha3 Code", all.x = TRUE)


# Educacion  --------------------------------------------------------------

# Filtrar NA en pc1_educacion, pc2_educacion y Region Name
merged_table_filtered_education <- merged_table[complete.cases(merged_table[, c("pc1_educacion", "pc2_educacion", "Sub-region Name","Region Name")]),]

# Crear la gráfica de dispersión pintado por region
ggplot(data = merged_table_filtered_education, aes(x = pc1_educacion, y = pc2_educacion, color = `Sub-region Name`)) +
  geom_point() +
  labs(x = "PC1 Educación", y = "PC2 Educación", title = "Gráfico PC1 vs PC2 Educación por Región")

#Crear la gráfica de dispersion pintado por region
ggplot(data = merged_table_filtered_education, aes(x = pc1_educacion, y = pc2_educacion, color = `Region Name`)) +
  geom_point() +
  labs(x = "PC1 Educación", y = "PC2 Educación", title = "Gráfico PC1 vs PC2 Educación por Región")


# Economia ----------------------------------------------------------------
# Filtrar NA en pc1_economia, pc2_economia, Sub-region Name y Region Name
merged_table_filtered_economy <- merged_table[complete.cases(merged_table[, c("pc1_economia", "pc2_economia", "Sub-region Name", "Region Name")]),]

# Crear la gráfica de dispersión pintado por Sub-region Name
ggplot(data = merged_table_filtered_economy, aes(x = pc1_economia, y = pc2_economia, color = `Sub-region Name`)) +
  geom_point() +
  labs(x = "PC1 Economía", y = "PC2 Economía", title = "Gráfico PC1 vs PC2 Economía por Sub-Región")

# Crear la gráfica de dispersión pintado por Region Name
ggplot(data = merged_table_filtered_economy, aes(x = pc1_economia, y = pc2_economia, color = `Region Name`)) +
  geom_point() +
  labs(x = "PC1 Economía", y = "PC2 Economía", title = "Gráfico PC1 vs PC2 Economía por Región")


# Ambiental ---------------------------------------------------------------
# Filtrar NA en pc1_ambiental, pc2_ambiental, Sub-region Name y Region Name
merged_table_filtered_ambiental <- merged_table[complete.cases(merged_table[, c("pc1_ambiental", "pc2_ambiental", "Sub-region Name", "Region Name")]),]

# Crear la gráfica de dispersión pintado por Sub-region Name
ggplot(data = merged_table_filtered_ambiental, aes(x = pc1_ambiental, y = pc2_ambiental, color = `Sub-region Name`)) +
  geom_point() +
  labs(x = "PC1 Ambiental", y = "PC2 Ambiental", title = "Gráfico PC1 vs PC2 Ambiental por Sub-Región")

# Crear la gráfica de dispersión pintado por Region Name
ggplot(data = merged_table_filtered_ambiental, aes(x = pc1_ambiental, y = pc2_ambiental, color = `Region Name`)) +
  geom_point() +
  labs(x = "PC1 Ambiental", y = "PC2 Ambiental", title = "Gráfico PC1 vs PC2 Ambiental por Región")




