faces(base_cancer, fill = FALSE)

jurisdiccion_Males <- ggplot(base_cancer, aes(y = Males, x = Females)) +
  geom_point() +
  xlim(c(0, 2500))

jurisdiccion_Males <- ggplot(base_cancer, aes(y = Jurisdiction, x = Males)) +
  geom_point() +
  xlim(c(0, 2500))

jurisdiccion_Females <- ggplot(base_cancer, aes(y = Jurisdiction, x = Females)) +
  geom_point() +
  xlim(c(0, 2500))

jurisdiccion_Whites <- ggplot(base_cancer, aes(y = Jurisdiction, x = Whites)) +
  geom_point() +
  xlim(c(0, 4000))

jurisdiccion_Blacks <- ggplot(base_cancer, aes(y = Jurisdiction, x = Blacks)) +
  geom_point() +
  xlim(c(0, 2000))

Male_Female <- ggplot(base_cancer, aes(x = Males, y = Females)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión",
       x = "Males",
       y = "Females") +
  theme_minimal() + 
  xlim(c(0, 1000)) +
  ylim(c(0, 1000))

Black_White <- ggplot(base_cancer, aes(x = Whites, y = Blacks)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión",
       x = "Whites",
       y = "Blacks") +
  theme_minimal() + 
  xlim(c(0, 2000)) +
  ylim(c(0, 500))