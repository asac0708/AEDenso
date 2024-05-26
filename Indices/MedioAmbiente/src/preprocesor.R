
# Aplicar la función valores_representativos a cada tabla
rep_agua_dulce <- valores_representativos(agua_dulce)
rep_areas_protegidas <- valores_representativos(areas_protegidas)
rep_energia_renovable <- valores_representativos(energia_renovable)
rep_energia_fosil <- valores_representativos(energia_fosil)
rep_emisiones_co2 <- valores_representativos(emisiones_co2)
rep_agriculture_land <- valores_representativos(agriculture_land)
rep_electric_consumption <- valores_representativos(electric_consumption)


# Eliminar la columna Country.Name usando indexación básica de R
rep_energia_renovable <- rep_energia_renovable[, !(names(rep_energia_renovable) %in% "Country.Name")]
rep_energia_fosil <- rep_energia_fosil[, !(names(rep_energia_fosil) %in% "Country.Name")]
#rep_emisiones_co2 <- rep_emisiones_co2[, !(names(rep_emisiones_co2) %in% "Country.Name")]
rep_electric_consumption <- rep_electric_consumption[, !(names(rep_electric_consumption) %in% "Country.Name")]

# Crear una lista con todos los valores representativos
tablas <- list(
  #rep_agua_dulce,
  #rep_areas_protegidas,
  rep_energia_renovable,
  rep_energia_fosil,
  rep_emisiones_co2,
  #rep_agriculture_land,
  rep_electric_consumption
)




# Usar Reduce y merge para unir todas las tablas mediante las columnas Country.Code y Country.Name
tabla_unida <- Reduce(function(x, y) merge(x, y, by = c("Country.Code"), all = TRUE), tablas)
tabla_unida <- as.data.frame(tabla_unida)


# Establecer Country.Code como nombres de fila
rownames(tabla_unida) <- tabla_unida$Country.Code

# Eliminar la columna Country.Code si ya no la necesitas
tabla_unida <- tabla_unida[ , !names(tabla_unida) %in% "Country.Code"]


