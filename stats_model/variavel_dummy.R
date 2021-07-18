library(tidyverse)

data <- read_csv('Aula12_exercícios.csv')

# (a)
model <- lm(Vendas ~ Preços + Primavera + Verão + Outono, data = data)

summary(model)

# Vendas = 34.07636 - 0.77561 (Preços) + 15.54146 (Primavera) + 30.48454 (Verão) + 0.64717 (Outono)

new_model <- lm(Vendas ~ Preços + Primavera + Verão, data = data)

summary(new_model)

new_data <- data.frame(
  Preços = c(30, 30, 30, 30, 20, 40, 15, 15),
  Primavera = c(1, 0, 0, 0, 1, 0, 0, 0),
  Verão = c(0, 1, 0, 0, 0, 1, 0, 0),
  Outono = c(0, 0, 1, 0, 0, 0, 1, 0)
)

predict(new_model, new_data)
