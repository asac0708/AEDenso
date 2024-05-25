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
