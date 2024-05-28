faces(base_cancer, fill = FALSE)

Total_Females <- ggplot(base_cancer, aes(y = Total, x = Females)) +
  geom_point() +
  labs(title = "Total_Females",
       x = "Females",
       y = "Total") +
  xlim(c(0, 5000)) +
  ylim(c(0, 5000))

Total_Males <- ggplot(base_cancer, aes(y = Total, x = Males)) +
  geom_point() +
  labs(title = "Total_Males",
       x = "Males",
       y = "Total") +
  xlim(c(0, 2500)) +
  ylim(c(0, 5000))

Total_Whites <- ggplot(base_cancer, aes(y = Total, x = Whites)) +
  geom_point() +
  labs(title = "Total_Whites",
       x = "Whites",
       y = "Total") +
  xlim(c(0, 5000)) +
  ylim(c(0, 5000))

Total_Blacks <- ggplot(base_cancer, aes(y = Total, x = Blacks)) +
  geom_point() +
  labs(title = "Total_Blacks",
       x = "Blacks",
       y = "Total") +
  xlim(c(0, 5000)) +
  ylim(c(0, 5000))



Male_Female <- ggplot(base_cancer, aes(x = Males, y = Females)) +
  geom_point() +
  labs(title = "Male_Female",
       x = "Males",
       y = "Females") +
  theme_minimal() + 
  xlim(c(0, 1000)) +
  ylim(c(0, 1000))

Black_White <- ggplot(base_cancer, aes(x = Whites, y = Blacks)) +
  geom_point() +
  labs(title = "Black_White",
       x = "Whites",
       y = "Blacks") +
  theme_minimal() + 
  xlim(c(0, 2000)) +
  ylim(c(0, 2500))
