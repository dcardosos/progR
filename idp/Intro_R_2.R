#Gabarito das questões

#Questão 2

pais <- c("Afeganistão", "Albania", "Argélia", "Angola", "Argentina")
pais

gini <- c(29.4, 33.0, 35.3, 58.6, 48.8)
gini

alfab <- c(28.1, NA, 69.9, 67.4, 97.2)
alfab

#Questão 3 

media_gini <- mean(gini)
media_gini

dp_gini <- sd(gini)
dp_gini

media_alfab <- mean(alfab, na.rm = TRUE)
media_alfab

dp_alfab <- sd(alfab, na.rm = TRUE)
dp_alfab


#Questão 4 

gini_raiz <- sqrt(gini[2])
gini_raiz

#Questão 5

soma_pares <- sum(seq(from = 2, to = 268, by = 2))
soma_pares


soma_impares <- sum(seq(from = 1, to = 251, by = 2))
soma_impares

#Questão 6

#install.packages("gapminder")

library(gapminder)

head(gapminder)

#População do Afeganistão em 1952 era de 8.425.333. 

# DATAFRAMES

# Dataframes são tabelas de dados com uma lista de vetores de tamanhos iguais. 
# É o tipo de objeto mais comumente usado em an?lises de dados. Suponha que queiramos criar um dataframe 
# com os vetores `pais` e `gini` e `alfab`. Chamaremos o nosso dataframe de `desigualdades`. 
# Para criarmos um dataframe basta usar a função `data.frame()`:

pais <- c("Afeganistão", "Albania", "Argélia", "Angola", "Argentina")

gini <- c(29.4, 33.0, 35.3, 58.6, 48.8)

alfab <- c(28.1, NA, 69.9, 67.4, 97.2)

desigualdades <- data.frame(pais, gini, alfab)

desigualdades

View(desigualdades)

#Podemos adicionar variáveis ao nosso dataframe. Por exemplo:

exp_vida <- c(45.25, 80.30, 76.31, 39.83, 77.31)

desigualdades <- data.frame(desigualdades, exp_vida)

desigualdades

# Para selecionarmos uma coluna de um dataframe, usamos o s?mbolo `$`. Também podemos usar o operador `[]`.

desigualdades$pais

desigualdades[,1]

# Se quisermos uma tabela de frequências simples de uma coluna de um dataframe, usamos a função `table`:

table(desigualdades$gini)

# Caso queiramos uma tabela com frequências relativas, podemos usar a função `prop.table` e, dentro dela, 
# inserir a função `table`:


prop.table(table(desigualdades$gini))


#Uma forma mais simples de gerar tabelas de frequências simples e relativas é usar o pacote "expss".

install.packages("expss")
library(expss)
library(poliscidata)

fre(world$regime_type3)

# Abrindo dados em .csv
# Um dos principais formatos de armazenamento de dados é o comma separated value (csv). 
# Para abrirmos uma base de dados nesse formato no R, usamos a função read.csv().


setwd("C:\\Users\\aless\\Downloads") #Insira aqui o endereço da pasta onde o banco de dados do TSE está salvo no seu computador.

dados.tse <- read.csv("detalhe_votacao_munzona_2020_BRASIL.csv", sep = ";")

head(dados.tse)

# Se quisermos criar uma nova variável, basta nominá-la e calculá-la usando o operador `$`. Vamos criar
# uma variável de votos inválidos, somando votos brancos e nulos.

dados.tse$QT_VOTOS_INV <- dados.tse$QT_VOTOS_NULOS + dados.tse$QT_VOTOS_BRANCOS

summary(dados.tse$QT_VOTOS_INV)

# Agora podemos calcular estatísticas da nossa variável.

mean(dados.tse$QT_VOTOS_NULOS)
sd(dados.tse$QT_VOTOS_NULOS)

# Filtrando dados

# A função `subset()` é extremamente útil em análises de dados por permitir que identifiquemos padrões em subconjuntos de nossos dados. 
# Essa função possui dois argumentos: o primeiro é a base de dados a partir da qual criaremos um filtro e o segundo é 
# a variável (ou variáveis) que serão utilizadas para a construção desse filtro. 
# Para usarmos a função subset, devemos usar os operadores lógicos.
# 
# Suponha que queiramos analisar apenas dados da base de dados `gapminder` referentes ao Brasil. Poderíamos criar 
# uma nova base de dados chamada `gapminder.brasil` usando um filtro com o nome do país.


library(gapminder)

gapminder.brasil <- subset(gapminder, country == "Brazil")

head(gapminder.brasil)

# Note que usamos acima o operador lógico de identidade `==`. Ou seja, o filtro seleciona apenas as observações 
# pertinentes ao Brasil com base na variável `country`. Note Também que, como a variável `country` é uma variável de 
# caracteres, devemos usar aspas. Se quisermos usar mais de uma variável como filtro, podemos usar o operador `&`. 
# Suponha que queiramos uma base de dados apenas sobre os Estados Unidos da década de 1980 em diante.

gapminder.us.subset <- subset(gapminder, country == "United States" & year >= 1980)

head(gapminder.us.subset)