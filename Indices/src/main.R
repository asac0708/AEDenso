library(dplyr)


getwd()
setwd("C:/Users/sofia/Documents/Hackaton Datos/Indices/src/")

# Funciones ---------------------------------------------------------------
valores_representativos <- function(dataset) {
  num_filas <- nrow(dataset)
  num_columnas <- ncol(dataset)
  
  resultados <- data.frame(Country.Name = character(), Country.Code = character(), `indicator` = numeric(), Valor.Final = numeric(), stringsAsFactors = FALSE)
  for (i in 1:num_filas) {
    #print(co2[i,1])
    valores <- c()  # Vector para almacenar los 5 valores representativos
    
    j <- num_columnas  # Empezamos desde la �ltima columna
    
    while (j >= 5 && is.na(dataset[i, j])) {
      j <- j - 1  # Decrementamos j para ir hacia la izquierda
    }
    k <- j
    while (j >= 5 && j >= k - 4) {
      dato <- dataset[i, j]
      if(!is.na(dato)){
      valores <- c(valores, dato)}  # Agregar el dato al vector de valores
      j <- j - 1  # Decrementamos j para ir hacia la izquierda
    }
    print("")
    cat("Fila", i, ":", valores, "\n")  # Imprimir el vector de valores representativos
    contador_evaluacion <- 0
    if(length(valores)>1){
      for (h in 2:length(valores)) {
        print("h")
        print(h)
        print("valores")
        print(valores)
        if(valores[h]-valores[h-1]>0){
          contador_evaluacion <- contador_evaluacion +1
        }else if(valores[h]-valores[h-1]<0){
          contador_evaluacion <- contador_evaluacion -1
        }else{
          
        }
      }
    }
    print("contador evaluacion")
    print(contador_evaluacion)
    valor_final <- NA
    if(contador_evaluacion==length(valores)-1){
      valor_final <- valores[length(valores)]
    }else if(contador_evaluacion==length(valores)+1){
      valor_final <- valores[length(valores)]
    }else{
      valor_final <- mean(valores)
    }
    print("Valor de i")
    print(i)
    fila <- data.frame(Country.Name = dataset[i, 1], Country.Code = dataset[i, 2], `indicator` = valor_final)
    print(fila)
    resultados <- rbind(resultados, fila)
  }
  nuevo_nombre <- dataset[1, 3] 
  names(resultados)[3] <- nuevo_nombre
  return(resultados)
}

# Lectura datos -----------------------------------------------------------

#Porcentaje de gasto público
porcentaje_inversion <- read.csv("../input/educacion/porcentaje_gasto_publico.csv", skip=4)
#Porcentaje de alfabetizacion
alfabetizacion_total <- read.csv("../input/educacion/tasa_afabetizacion_total.csv", skip=4)
#Proporcion maestro estudiante
proporcion_maestro_estudiantes <- read.csv("../input/educacion/proporcion_estudiantes_maestro.csv",skip=4)
#Inscripcion_primaria
inscripcion_primaria <- read.csv("../input/educacion/inscripcion_primaria_neta.csv", skip=4)
#Inscripcion secundaria
inscripcion_secundaria <- read.csv("../input/educacion/inscripicion_secundaria_neta.csv",skip=4)
porcentaje_maestros_primaria <- read.csv("../input/educacion/maestros_capacitados_primaria.csv", skip=4)
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


# Correlaciones -----------------------------------------------------------

# Supongamos que ya tienes el dataframe tabla_unida

# Seleccionar las columnas de la 3 a la 8
datos_seleccionados <- tabla_unida[, 3:8]

# Calcular la matriz de correlaciones
matriz_correlaciones <- cor(datos_seleccionados, use = "complete.obs")

# PCA ---------------------------------------------------------------------
# Estandarizar los datos
datos_estandarizados <- scale(datos_seleccionados)
datos_seleccionados<- na.omit(datos_seleccionados)
datos_seleccionados <- datos_seleccionados[!apply(datos_seleccionados, 1, function(x) any(is.infinite(x))) ]
1# Realizar el PCA
pca_resultado <- prcomp(datos_estandarizados, center = TRUE, scale. = TRUE)

# Mostrar un resumen del resultado
summary(pca_resultado)

