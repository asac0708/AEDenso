

# #Medio Ambiente ---------------------------------------------------------

#agua dulcce per capita metro cubico
agua_dulce <- read.csv("../input/MedioAmbiente/recursos_agua_dulce_per_capita_m3.csv", skip=4)

# Porcentaje areas protegidas
areas_protegidas <- read.csv("../input/MedioAmbiente/porcentaje_areas_protegidas.csv", skip=4)

#Porcentaje energía renovable
energia_renovable <- read.csv("../input/MedioAmbiente/porcentaje_energía_renovable.csv", skip =4)

#Porcentaje de energia proveniente de combustibles fosiles
energia_fosil <- read.csv("../input/MedioAmbiente/porcentaje_energia_fosil.csv",skip=4)

#Emisiones co2 per capita toneladas
emisiones_co2 <- read.csv("../input/MedioAmbiente/emisiones_co2_per_capita_toneladas.csv", skip=4)



agriculture_land <- read.csv("../input/co2/agricultural_land(% of land area).csv", skip=3)
electric_consumption <- read.csv("../input/co2/electric_consumption(kWh per capita).csv", skip=3)
#population_growth <- read.csv("../input/population_growth/popularion_growth.csv", skip=3)
