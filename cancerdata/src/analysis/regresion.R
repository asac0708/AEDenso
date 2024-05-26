model1 <- lm(data = base_cancer,
            Total ~ Females + Males + Blacks + Whites)

summary(model1)

# Buena especcificacion del modelo:
  # R^2
  # R^2 ajustado
  # resset.ramset -> Buscar una mejor tranf.
  
resettest(model1)

# Estudio de los errores

mean(model1$residuals)

plot(model1$residuals, pch  = 20)

hist(model1$residuals, breaks = 40)

# Heterocedasticidad
# H0: var cte ~ x^2(p) Homocedasticidad
# Ha:                  Heterocedasticidad 
#
# Si el p valor es muy pequeño, rechazamos

bptest(model1)

# Normalidad de los errores

shapiro.test(model1$residuals)

# Estudio de los residuos

# Q-Q plot: En y tiene los residuios estandarizados, y en x los quantiles 
