
# Aplicar la función valores_representativos a cada tabla
rep_agua_dulce <- valores_representativos(agua_dulce)
rep_areas_protegidas <- valores_representativos(areas_protegidas)
rep_energia_renovable <- valores_representativos(energia_renovable)
rep_energia_fosil <- valores_representativos(energia_fosil)
rep_emisiones_co2 <- valores_representativos(emisiones_co2)

# Crear una lista con todos los valores representativos
tablas <- list(
  rep_agua_dulce,
  rep_areas_protegidas,
  rep_energia_renovable,
  rep_energia_fosil,
  rep_emisiones_co2
)

# Usar Reduce y merge para unir todas las tablas mediante las columnas Country.Code y Country.Name
tabla_unida <- Reduce(function(x, y) merge(x, y, by = c("Country.Code", "Country.Name"), all = TRUE), tablas)
tabla_unida <- as.data.frame(tabla_unida)

# Establecer Country.Code como nombres de fila
rownames(tabla_unida) <- tabla_unida$Country.Code

# Eliminar la columna Country.Code si ya no la necesitas
tabla_unida <- tabla_unida[ , !names(tabla_unida) %in% "Country.Code"]

# Mostrar las primeras filas para verificar
head(tabla_unida)
