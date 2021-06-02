# funcional solution
library(tidyverse)
library(psych)
  
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





# extra
# outliers
hist(dados$circunferência)
boxplot(dados$circunferência, outline = TRUE)

hist(dados$diâmetro)
boxplot(dados$diâmetro, outline = TRUE)

## winsorização: 
### todos valores abaixo de 14 serão colocados como 14
### todos valores acima de 14450.28 serão colocados como 14450.28
quantile(dados$circunferência, probs=c(0.01, 0.99, 0.05, 0.95))


## winsor 0.05
dados <- within(dados,
                {circunferencia_w_5 <- psych::winsor(dados$circunferência, trim = 0.05)})

dados <- within(dados,
                {diametro_w_5 <- psych::winsor(dados$diâmetro, trim = 0.05)})


## winsor 0.01
dados <- within(dados,
                {circunferencia_w_1 <- psych::winsor(dados$circunferência, trim = 0.01)})

dados <- within(dados,
                {diametro_w_1 <- psych::winsor(dados$diâmetro, trim = 0.01)})



## revendo as distribuições
### circun
hist(dados$circunferência)
hist(dados$circunferencia_w_1)
hist(dados$circunferencia_w_5)

### diam
hist(dados$diâmetro)
hist(dados$diametro_w_1)
hist(dados$diametro_w_5)


## box plots
### circun
boxplot(dados$circunferência)
boxplot(dados$circunferencia_w_1)
boxplot(dados$circunferencia_w_5)

### diam
boxplot(dados$diâmetro)
boxplot(dados$diametro_w_1)
boxplot(dados$diametro_w_5)


## plot
plot(dados$diametro_w_5, dados$circunferencia_w_5, main="Outliers removed \n A much better fit!")
abline(lm(diametro_w_5 ~ circunferencia_w_5,  data=dados), col="blue")

plot(dados$diâmetro, dados$circunferência)
