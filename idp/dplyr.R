# Introdução

# O dplyr é um pacote desenvolvido por Hadley Wickham, um dos principais programadores da comunidade de R.
# A principal função do dplyr é facilitar a manipulação de dados.
# O operador pipe (%>%) faz parte do pacote dplyr e faz com que os códigos em R fiquem mais limpos e fáceis de entender.
# Usando o dplyr e o operador pipe, podemos programar de forma mais rápida, limpa e eficiente.

# R básico
# Ainda que o R básico seja bastante versátil e potente, alguns comandos podem se mostrar pouco “limpos” e por vezes 
# complicados.
# Um dos princípios básicos de qualquer linguagem de programação é o DRY(Don’t Repeat Yourself). 
# Alguns comandos do R básico acabam induzindo repetições desnecessárias.

# Por exemplo:

nome <- c("João", "Fernanda", "Daniel", "Letícia", "Marcos", "Thiago", "Gabriela", "Natália")
sexo <- c("Masculino", "Feminino", "Masculino", 
          "Feminino", "Masculino", "Masculino", "Feminino", "Feminino")
altura <- c(1.76, 1.67, 1.85, 1.60, 1.78, 1.83, 1.80, 1.59)
peso <- c(78.64, 59.03, 100.2, 70.0, 72.0, 86.7, 71.3, 54.8)


# Create a data frame from the vectors
dados_pessoais <- data.frame(nome, sexo, altura, peso)


#Note que, usando o R básico, repetimos o nome da base de dados 
#por quatro vezes quando queremos criar uma nova variável. Também
#repetimos quatro vezes o sinal $, para selecionar elementos de um 
#objeto.

dados_pessoais$altura_pes <- dados_pessoais$altura*3.281
dados_pessoais$peso_libras <- dados_pessoais$peso*2.205

# Operador pipe
# Para evitarmos repetições desnecessárias e deixarmos nosso código mais fácil de ler, 
# podemos usar o operador pipe (%>%).
# 
# Esse operador signfica “e depois” na linguagem R.
# 
# Ou seja, o pipe dá sequência a um comando e evita que façamos repetições.
# 
# Por exemplo:

#install.packages("dplyr")
library(dplyr)
dados_pessoais <- dados_pessoais %>%
  mutate(altura_pes = altura*3.281,
         peso_libras = peso *2.205)



# Dados
# Vamos trabalhar com as bases de dados Dahldims.sav e Democracy.sav, além de bases dos pacotes gapminder e poliscidata.
# A base Dahldims.sav foi montada por Michael Coppedge e Wolfgang Reinicke e traz variáveis relativas ao 
# conceito de poliarquia de Robert Dahl. Mais detalhes aqui: https://www3.nd.edu/~mcoppedg/crd/datacrd.htm.
# A base Democracy.sav foi montada por Pipa Norris e traz uma série de dados referentes a dimensões importantes 
# da democracia, tais como a liberdade de expressão e a participação da mulher na política. 
# Mais detalhes aqui: https://sites.hks.harvard.edu/fs/pnorris/Data/Data.htm

library(haven)
library(ggplot2)
setwd("C:\\Users\\aless\\Downloads") #Insira aqui o endereço da pasta onde o banco de dados Dahldims está salvo no seu computador.
dahl <- read_spss("DahlDims.sav")

dahl_brasil <- dahl[dahl$abbr == "BRZL",]
ggplot(dahl_brasil, aes(x = year, y = CONTEST)) + geom_point()

# Usando o pipe, simplificamos o código. 

dahl %>%
  filter(abbr == "BRZL") %>%
  ggplot(aes(x = year, y = CONTEST)) + geom_point()

# O Pacote dplyr
# O pacote dplyr inclui uma série de funções que simplificam a programação.
# É um pacote excelente para a manipulação de dados.
# Algumas das principais funções do dplyr são: filter, arrange, select, mutate, group_by e summarise.

# Filter

# A função filter() é equivalente ao uso de [] para selecionar valores de variáveis e à função subset().
# Uma das principais vantagens da função filter() em relação ao uso de [] e à função subset() é que ela é mais rápida 
# quando estamos lidando com bases de dados maiores. Além disso, ela também permite operar em SQL sem exigir que 
# os dados sejam carregados na memória. O primeiro argumento da função filter() corresponde ao dataframe e o segundo 
# argumento corresponde à variável de interesse. Note que não precisamos repetir o nome da base de dados quando estamos 
# usando o operador pipe porque o R entende que o comando seguinte se refere ao objeto dahl.
# Se quisermos apenas os dados a partir do ano de 1960 para o Afeganistão da base de dados dahl, podemos usar:

dahl %>%
  filter(abbr == "AFGN" & year >= 1960) %>%
  head(3)

# Cabe notar que o comando anterior NÃO gera uma nova base de dados no ambiente global do R.
# Ou seja, se quisermos que a nova base de dados apareça no ambiente global, devemos criar um novo 
# objeto, usando o sinal <-.

dahl_afg_1960 <- dahl %>%
  filter(abbr == "AFGN" & year >= 1960) 
head(dahl_afg_1960)

#Arrange

# A função arrange() ordena os dados de um dataframe.
# O primeiro argumento da função corresponde ao dataframe e o segundo à variável (ou variáveis) a partir 
# da qual se deseja ordenar uma base de dados. Caso queiramos ordenar os dados em ordem descresente, podemos usar 
# a função desc().

dahl %>% 
  filter(abbr == "BRZL") %>% 
  select(year, CONTEST) %>% 
  arrange(CONTEST)

# Cabe lembrar que o padrão de ordenamento da função arrange() é ascendente.

dahl %>%
  arrange(year, CONTEST) %>%
  head()

# Mutate
# A função mutate() cria novas variáveis em um dataframe.

library(gapminder)

gapminder %>%
  mutate(gdp = gdpPercap*pop)

# Naturalmente, podemos criar mais de uma variável.

library(poliscidata)

world %>%
  mutate(spendeduc_health_diff = spendeduc - spendhealth) %>%
  select(country, spendeduc, spendhealth, spendeduc_health_diff, regime_type3) %>%
  arrange(spendeduc_health_diff) %>% 
  head()

# Transmute

# A função transmute() é uma variação da função mutate(). A diferença é que transmute() deleta todas as 
# demais variáveis e mantém apenas as que foram criadas. É uma função ideal para quando queremos criar um novo 
# dataframe. Note que, se quisermos manter uma variável do banco de dados anterior, basta digitar o seu nome dentro 
# do comando transmute.

library(poliscidata)

world %>%
  transmute(spendeduc_health_diff = spendeduc - spendhealth, country, pop_65_older) %>%
  head()


# Select
# A função select() seleciona variáveis de um dataframe.

dahl %>%
  select(year, CONTEST) %>%
  summary()

# Podemos usar os argumentos starts_with(), ends_with() e contains() se 
# quisermos selecionar variáveis com nomes específicos.

setwd("C:\\Users\\aless\\Downloads") #Insira aqui o endereço da pasta onde o banco de dados Democracy está salvo no seu computador.
democracy <- read_spss("Democracy.sav")

democracy %>%
  select(starts_with("cpi")) %>%
  head()

democracy %>%
  select(ends_with("mean")) %>%
  head()

democracy %>%
  select(contains("press")) %>%
  head()

# Summarise

# A função `summarise()` fornece estatísticas sumárias definidas pelo próprio usuário.

dahl %>%
  filter(!is.na(CONTEST)) %>%
  summarise(cont_mean = mean(CONTEST),
            cont_sd = sd(CONTEST),
            cont_max = max(CONTEST)) 

# Group by
# A função group_by() agrupa os dados a partir de variáveis selecionadas.
# Essa é uma função essencial, pois nos poupa muito tempo com comandos desnecessários e repetitivos.
# Por exemplo, se quiséssemos a média, o desvio-padrão e o valor máximo de CONTEST para cada país usando o R básico, 
# poderíamos fazer “à mão” da seguinte forma:

cont_mean <- mean(dahl$CONTEST[dahl$cname=="Afghanistan"], na.rm = T)
cont_sd <- sd(dahl$CONTEST[dahl$cname=="Afghanistan"], na.rm = T)
cont_max <- max(dahl$CONTEST[dahl$cname=="Afghanistan"], na.rm = T)

# É evidente que esse método é extremamente tedioso, lento e ineficiente.
# 
# Usando a função group_by() simplificamos a tarefa:

dahl %>%
  filter(!is.na(CONTEST)) %>%
  group_by(cname) %>%
  summarise(cont_mean = mean(CONTEST),
            cont_sd = sd(CONTEST),
            cont_max = max(CONTEST)) 

# Note que, no exemplo anterior, se tentássemos agrupar os dados depois de calcular as estatísticas, 
# o R nos retornaria um erro.

# Isso ocorre porque já não estamos manipulando o dataframe original, mas sim um objeto com tr?s elementos.

# Count
# A função count() conta o número de observações em um ou mais grupos.

dahl %>%
  count(cname) 

# Salvando objetos
# Para salvarmos as alterações que fizemos nos nossos dados ou outros objetos que criamos (gráficos, por exemplo), 
# devemos sempre lembrar de usar o sinal de `<-`.

dahl <- dahl %>%
  mutate(CONTEST_INCLUS_DIFF = CONTEST - INCLUS)
  
head(dahl)


cuba_grafico <- dahl %>%
  filter(cname == "Cuba") %>%
  ggplot(aes(x = year, y = CONTEST)) + geom_point() 

cuba_grafico

# setwd("C:\\Users\\aless\\Downloads")
# library(haven)
# library(dplyr)
dahl <- read_spss("DahlDims.sav")

dahl.novo <- dahl %>%
  mutate(CONTEST_INCLUS_DIFF = CONTEST - INCLUS)

# .csv
write.csv(dahl.novo, "dahl.novo.csv")

# .dta (Stata)
write_dta(dahl.novo, "dahl.novo.dta")

# .sav (SPSS)
write_sav(dahl.novo, "dahl.novo.sav")

# .Rds (R)
#install.packages(readr)
library(readr)

write_rds(dahl.novo, "dahl.novo.rds")