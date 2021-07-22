### Introdução ao R ###
##  Alessandro Freire


summary(cars) #Isto é um comentário, não um comando.

?mean

## R como calculadora
2 + 2 #Soma
7 - 2 #Subtração
2*3 #Multiplicação
9/3 #Divisão
2^4 #Potenciação
sqrt(64) #Raíz quadrada
log(10) #logaritmo

##Criando Objetos

a <- 3
a

a@ <- 3 #Gera erro

1a <- 3 #Gera erro

a1 <- 4 #Não gera erro

a.1 <- 5 #Não gera erro

##Operações com objetos

a <- 3 + 2
a

b <- 8 - 4
b

c <- a + b
c

d <- sqrt(c)
d


##Função print

print(a)
print(a+b)

##Substituindo e Eliminando Objetos

a <- 12 - 3
a

rm(a)
a

#Vetores

idade_1 <- c(23, 19, 34, 52, 40)
idade_1

idade_2 <- c(42, 21, 36, 70)
idade_2

idade_3 <- c(idade_1, idade_2)
idade_3

idade_1[2]

idade_1[2]/4

idade_1
idade_1[-3]

#Funções básicas

length(idade_1)

min(idade_1)
max(idade_1)

mean(idade_2) #Média 

var(idade_2) #Variância

sd(idade_2) #Desvio-padrão 

idade_1_NA <- c(23, 19, 34, 52, 40, NA)
mean(idade_1_NA) #Sem o argumento para remoção de NAs, o R nos retorna NA.

mean(idade_1_NA, na.rm = TRUE)

seq(from = 1, to = 10)

seq(from = 2, to = 30, by = 2)

sum(idade_1)

range(idade_1)

sqrt(max(idade_1))

##Operadores lógicos ou booleanos

4 == 4 #Igual a

4 > 5 #Maior que

7 != 6 #Diferente de

5 >= 5 #Maior ou igual a

4 <= 3 #Menor ou igual a

is.null(idade_1_NA) #É nulo

is.na(idade_1_NA) #É faltante

##Definindo um diretório de trabalho

getwd()

setwd("C:/Users/aless/Downloads")

setwd("C:\\Users\\aless\\Downloads")

#Instalando e carregando pacotes

install.packages("dplyr") #Instalando
library(dplyr) #Carregando


#Visualizando dados 

install.packages("poliscidata")
library(poliscidata)
head(world)


head(dados.peso.altura)

head(world)

head(world, 10)

View(world)

#Encerrando uma sessão no R

#quit()

#q()

