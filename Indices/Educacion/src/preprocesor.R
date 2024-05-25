
# Quitar columnas innecesarias --------------------------------------------

# Seleccionar las columnas que queremos mantener, eliminando las no deseadas
porcentaje_inversion <- porcentaje_inversion %>%
  select(-matches("^X(196[0-9]|19[7-9][0-9]|200[0-9]|2010|2011|2023)$"), -X)
# Seleccionar las columnas que queremos mantener eliminando las no deseadas
alfabetizacion_total <- alfabetizacion_total %>%
  select(-matches("^X(196[0-9]|19[7-9][0-9]|200[0-9]|2010|2011|2023)$"), -X)
# Seleccionar las columnas que queremos mantener eliminando las no deseadas
proporcion_maestro_estudiantes <- proporcion_maestro_estudiantes %>%
  select(-matches("^X(196[0-9]|19[7-9][0-9]|200[0-9]|2010|2011|2023)$"), -X)



porcentaje_inversion <- as.data.frame(porcentaje_inversion)
# Valores representativos -------------------------------------------------
rep_porcentaje_inversion <- valores_representativos(porcentaje_inversion)
rep_alfabetizacion_total <- valores_representativos(alfabetizacion_total)
rep_proporcion_maestro_estudiantes <- valores_representativos(proporcion_maestro_estudiantes)
rep_inscripcion_primaria <- valores_representativos(inscripcion_primaria)
rep_inscripcion_secundaria <- valores_representativos(inscripcion_secundaria)
rep_porcentaje_maestros_primaria <- valores_representativos(porcentaje_maestros_primaria)


# Tabla de trabajo --------------------------------------------------------

# Suponiendo que ya tienes las tablas rep_porcentaje_inversion, rep_alfabetizacion_total, etc.
# Crear una lista con todas las tablas
tablas <- list(
  rep_porcentaje_inversion,
  rep_alfabetizacion_total,
  rep_proporcion_maestro_estudiantes,
  rep_inscripcion_primaria,
  rep_inscripcion_secundaria,
  rep_porcentaje_maestros_primaria
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