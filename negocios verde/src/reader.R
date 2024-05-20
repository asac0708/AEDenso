datos_negocios <- read.csv("../input/Listado_de_Negocios_Verdes.csv")


ruta_xlsx <- "../input/poblacion por departamento.xlsx"
# Leer el archivo Excel y cargar una hoja específica en un data frame
datos_departamento <- read_excel(ruta_xlsx, sheet = 1, skip=8)  # Cambia el número de hoja según sea necesario
