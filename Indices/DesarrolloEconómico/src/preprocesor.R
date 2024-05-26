

# Aplicar la función valores_representativos a cada una de las nuevas tablas
rep_porcentaje_inflacion <- valores_representativos(porcentaje_inflacion)
rep_porcentaje_PIB_Agricultura <- valores_representativos(porcentaje_PIB_Agricultura)
rep_PIB_per_capita <- valores_representativos(PIB_per_capita)
rep_ahorro_bruto_PIB <- valores_representativos(ahorro_bruto_PIB)
rep_exportacion_tecnologia <- valores_representativos(exportacion_tecnologia)
rep_porcentaje_desempleo <- valores_representativos(porcentaje_desempleo)

tablas <- list(
  #rep_porcentaje_inflacion,
  rep_porcentaje_PIB_Agricultura,
  rep_PIB_per_capita,
  rep_ahorro_bruto_PIB,
  #rep_exportacion_tecnologia
  rep_porcentaje_desempleo
)


# Usar Reduce y merge para unir todas las tablas mediante las columnas Country.Code y Country.Name
tabla_unida <- Reduce(function(x, y) merge(x, y, by = c("Country.Code","Country.Name"), all = TRUE), tablas)
tabla_unida <- as.data.frame(tabla_unida)


# Establecer Country.Code como nombres de fila
rownames(tabla_unida) <- tabla_unida$Country.Code

# Eliminar la columna Country.Code si ya no la necesitas
tabla_unida <- tabla_unida[ , !names(tabla_unida) %in% "Country.Code"]

