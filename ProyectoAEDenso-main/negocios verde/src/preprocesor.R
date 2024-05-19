
# Preprocesamiento de datos -----------------------------------------------

datos_filtrados_departamento <- datos_departamento[datos_departamento$AÑO == 2022 & datos_departamento$`ÁREA GEOGRÁFICA` == "Total", ]

columnas_a_eliminar <- c("Descripción..Del.Negocio.Verde.", 
                         "RAZÓN.SOCIAL..Del.Negocio.Verde.",
                         "NOMBRE.REPRESENTANTE..Del.Negocio.Verde.",
                         "Producto.Principal..Que.promueve.el.Negocio.Verde.",
                         "MUNICIPIO..Donde.se.encuentra.el.Negocio.Verde.",
                         "AUTORIDAD.AMBIENTAL..Donde.se.encuentra.el.Negocio.Verde.")

# Seleccionar las columnas que no están en la lista de columnas a eliminar
datos_filtrados_negocios <- datos_negocios[, !(names(datos_negocios) %in% columnas_a_eliminar)]

# Identificacion de variables con categorias significativas
unique(datos_filtrados_negocios$CATEGORÍA..Del.Negocio.Verde.)  # 3 
unique((datos_filtrados_negocios$SECTOR..Al.cual.pertenece.el.Negocio.Verde.)) # 13
unique(datos_filtrados_negocios$REGIÓN..Donde.se.encuentra.el.Negocio.Verde.) # 11
unique(datos_filtrados_negocios$CATEGORÍA.COMERCIAL..Al.cual.pertenece.el.Negocio.Verde.) # 22
unique(datos_filtrados_negocios$SUBSECTOR..Al.cual.pertenece.el.Negocio.Verde.) # 23

