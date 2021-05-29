# funcional solution
library(tidyverse)

# ex1
dados <- read_csv2('MAC0113-EP1.csv')
dados$diâmetro <- as.numeric(dados$diâmetro)
dados

# ex2 
circunferencia <- dados$circunferência
diametro <- dados$diâmetro

# ex3
dados <- dados %>%
  mutate(
    meupi = map2_dbl(.x = circunferência, .y = diâmetro, ~.x/.y)
  )

# ex4
piMedio <- mean(dados$meupi)

# ex5
piVar <- var(dados$meupi)

# ex6 
# ...