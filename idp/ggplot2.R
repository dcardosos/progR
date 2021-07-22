
## **O pacote ggplot2**

# O pacote `ggplot2` simplifica a elaboração de gráficos no R, ao mesmo tempo em que fornece uma série de opções que 
# enriquecem as informa??es exibidas graficamente. Tanto o `ggplot2` quanto o R base têm uma imensidade de recursos 
# gráficos, contudo, o `ggplot2` torna a programaçao muito mais clara e intuitiva, mantendo a variedade de opções 
# do R base. 
# 
# 
# Um exemplo de como construir gráficos essencialmente idênticos com a função plot do R básico e a função ggplot do 
# pacote ggplot2 (fonte: https://statsinthewild.com/2019/08/22/ggplot2-vs-base-r-graphics-an-example/):
#   
#   R básico

CO2.QC <- subset(CO2, Type == "Quebec" & Treatment == "chilled")
CO2.QN <- subset(CO2, Type == "Quebec" & Treatment == "nonchilled")
CO2.MC <- subset(CO2, Type == "Mississippi" & Treatment == "chilled")
CO2.MN <- subset(CO2, Type == "Mississippi" & Treatment == "nonchilled")
xrange <- range(CO2$conc)
yrange<-range(CO2$uptake)
par(mfrow = c(1, 2))
plot(CO2.QC$uptake ~ CO2.QC$conc, pch = 19, lty = 2, lwd = 3, cex = 1.5, las = 1,
     ylim = yrange, xlim = xrange, col = "purple", xlab = "Concentration",
     ylab = "Uptake")
par(new=T)
plot(CO2.QN$uptake ~ CO2.QN$conc, pch = 20, lty = 2, lwd = 3, cex = 1.5, las = 1,
     ylim = yrange, xlim = xrange, col = "lightblue", main = "Quebec", xlab = "",
     ylab = "")
legend("bottomright", title = "Treatment", c("Chilled", "Nonchilled"),
       pch = c(19, 20), col = c("purple", "lightblue"))
plot(CO2.MC$uptake ~ CO2.MC$conc, pch = 19, lty = 2, lwd = 3, cex = 1.5, las = 1,
     ylim = yrange, xlim = xrange, col = "orange", xlab = "Concentration",
     ylab = "Uptake")
par(new=T)
plot(CO2.MN$uptake ~ CO2.MN$conc, pch = 20, lty = 2, lwd = 3, cex = 1.5,
     ylim = yrange, las = 1,
     xlim = xrange, col = "red", main = "Mississippi", xlab = "", ylab = "")
legend("topleft", title = "Treatment", c("Chilled", "Nonchilled"), pch = c(19,20),
       col = c("orange", "red"))
par(mfrow = c(1, 1), las = 1)

# ggplot2

library(ggplot2)

ggplot(aes(x = conc, y = uptake, color = Treatment), data = CO2) + geom_point() + facet_grid( ~ Type) + 
  xlab("Concentration") + ylab("Uptake") + labs(color = "Trt") + scale_color_manual(values = c("purple","blue"))

# Fica claro que a programação com o `ggplot2` é muito mais simples e fácil de entender. 
# Vamos quebrar cada argumento de um gráfico com o `ggplot2`. 

## Os argumentos do ggplot2

# **A base de dados** 
#   
# O primeiro argumento para um gráfico com `ggplot2` é a base de dados a partir da qual vamos gerar nossa figura. 
# O argumento `data` pode ser indicado explicitamente ou implicitamente. Ao usarmos a função `ggplot(dados)` o R nos 
# retorna uma moldura vazia, com o layout básico do `ggplot2`. 

library(gapminder)

ggplot(data = gapminder)


# **A função aes()** 
  
# Agora vamos preencher o nosso gráfico com as variáveis que queremos explorar. Por exemplo, suponha que queremos 
# plotar a relação entre expectativa de vida e tamanho da população. Para isso, devemos mapear como essas variáveis 
# serão dispostas no gráfico. Isto é, devemos escolher quais variáveis irão compor os eixos x e y. Fazemos isso com a 
# função `aes()`(de aestethics), inserindo-a dentro da função `ggplot()`, após especificarmos a base de dados.

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))


# Note que agora temos mais do que uma simples moldura vazia. O R agora nos dá uma moldura com os eixos x e y 
# correspondendo às variáveis de nosso interesse, contendo a amplitude de valores destas. Contudo, ainda não temos 
# um gráfico pois precisamos escolher um tipo de exibição para nossos dados. 

# O próximo passo é adicionarmos o tipo de gráfico que queremos (diagrama de pontos, linhas, boxplot etc.). 
# Fazemos isso com o argumento da família `geom_`. 

###geom_point
# 
# Se quisermos plotar um diagrama de pontos, devemos usar a função `geom_point()`. Vamos usar essa função sem 
# qualquer argumento adicional, por enquanto. Note que essa função é chamada separadamente, usando o sinal de `+`.

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point()

# Podemos ajustar nosso gráfico usando uma transformação logarítimica, uma vez que a distribuição de PIB per capita é 
# bastante concentrada em valores menores do gráfico. Pra isso usamos a função `scale_x_log10()`.

# options(scipen = 99999)

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() +
  scale_x_log10()

# Nomeando os eixos X e Y

# Para nomearmos os eixos X e Y, usamos as funções `xlab()` e `ylab()` com os respectivos nomes dos eixos entre 
# aspas dentro dos par?nteses. 


ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() +
  scale_x_log10() + xlab("GDP per Capita (Log)") + ylab("Life Expectancy")

# **Boxplots**
#   
# Boxplots são bons gráficos para descrevermos a distribuição de variáveis. Os boxplots incluem os valores mínimo e 
# máximo de uma distribuição, além de seus três quartis. Para montarmos um boxplot usamos a função `geom_boxplot()`.


library(poliscidata)

world_desigualdade <- dplyr::filter(world, !is.na(regime_type3) & !is.na(gender_unequal))

ggplot(world_desigualdade, aes(x = regime_type3, y = gender_unequal)) + geom_boxplot() +
  xlab("Tipo de Regime") + ylab("Índice de Desigualdade de Gênero")




# Lembrando que podemos usar o pipe`:
  
library(dplyr)
library(poliscidata)

world %>%
  filter(!is.na(regime_type3) & !is.na(gender_unequal)) %>% 
  ggplot(aes(x = regime_type3, y = gender_unequal)) + geom_boxplot() +
  xlab("Tipo de Regime") + ylab("Índice de Desigualdade de Gênero")




# **geom_line**
  
gapminder %>%
  filter(country == "Brazil") %>%
  ggplot(aes(x = year, y = gdpPercap)) + geom_line()


# **geom_histogram**
  

library(reshape2)

tips %>%
  ggplot(aes(x = total_bill)) + geom_histogram()



# Podemos usar o argumento `binwidth` para ajustar a espessura das colunas.



tips %>%
  ggplot(aes(x = total_bill)) + geom_histogram(binwidth = .7)



# Com os argumentos "color" e "fill" inserimos a cor do delineamento e do preenchimento do histograma. 



library(poliscidata)

world %>%
  ggplot(aes(x = spendeduc)) + geom_histogram(color="black", fill="grey", binwidth = .7) 


# Usamos o argumento "y = ..density.." quando queremos o eixo y em frequência relativa.

world %>%
  ggplot(aes(x = spendeduc, y = ..density..)) + geom_histogram(color="red", fill="green", binwidth = .2) 



# **geom_bar**
  


world %>%
  filter(!is.na(regime_type3))%>%
  ggplot(aes(x = regime_type3)) + geom_bar()



# Se quisermos barras com frequências relativas em vez de absolutas, usamos o argumento `y ..prop..` 
# dentro da função `aes`. 


mtcars %>%
  ggplot(aes(x = gear, y = ..prop..)) + geom_bar()



# Também podemos inverter os eixos x e y com a função `coord_flip()`.



mtcars %>%
  ggplot() + geom_bar(aes(x = gear)) + coord_flip()




# **geom_text**
  
# Essa função usa os labels das observações como pontos de um gráfico. Para isso, precisamos definir a variável que 
# servirá como label dentro da função `aes` usando o argumento `label`. Usamos o argumento `check_overlap = TRUE` 
# para evitar que as legendas se sobreponham, tornando-as ilegíveis. O argumento `size` define o tamanho da fonte.  



gapminder %>%
  filter(continent == "Africa" & year == 2007) %>%
  ggplot(aes(x = pop, y = gdpPercap, label = country)) + geom_text(check_overlap = TRUE, size = 3) 



# **color**
  
# Podemos adicionar analisar subgrupos por meio de cores em nossos diagramas de pontos usando o argumento `color`. 
# Esse argumento tem função semelhante ao argumento `label` quando usamos `geom_text`. Ele define quais categorias 
# serão atribuídas a determinadas cores. 



world %>% 
  filter(!is.na(gender_unequal_rank) & !is.na(gdp08) & !is.na(democ)) %>% 
  ggplot(aes(x = gdp08, y = gender_unequal_rank, color = democ)) + geom_point() +
  scale_x_log10() + xlab("PIB per capita (2008) ") + ylab("Índice de desigualdade \n de gênero")


library(gapminder)

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) + geom_point() + scale_x_log10()



# **shape**
  
# O argumento `shape` altera a forma dos pontos em um diagrama de pontos. Esse argumento pode ser usado dentro da 
# função `geom_point()` (quando apenas queremos mudar a forma dos pontos do diagrama) ou dentro da função 
# `aes()` (quando queremos diferenciar categorias).  



mtcars %>%
  ggplot(aes(x = mpg, y = hp)) + geom_point(shape = 11) 

mtcars %>%
  ggplot(aes(x = mpg, y = hp, shape = factor(cyl))) + geom_point()



# **size**
  
# Também podemos diferenciar observações em um gráfico por meio do tamanho dos seus pontos. O argumento `size` deve 
# ser usado com variáveis contínuas. Por exemplo, se quisermos diferenciar o tamanho da população dos países na 
# relação PIB X expectativa de vida, podemos fazer:
  
  


gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) + geom_point() +
  scale_x_log10()



# Múltiplos gráficos

# Se quisermos elaborar gráficos múltiplos, usamos a função `facet_wrap()`. Devemos inserir o sinal `~` e a variável 
# que irá separar os gráficos. Por exemplo, suponha que queremos plotar a relação entre PIB per capita e expectativa 
# de vida em gráficos separados para cada continente. Lembre-se de que a variável dentro de `facet_wrap()`deve ser 
# discreta. 



library(gapminder)

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  scale_x_log10() + facet_wrap(~ continent)



# É possível ajustar o número de linhas e colunas dentro de `facet_wrap()` usando os argumentos `nrow` e `ncol`. 


gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  scale_x_log10() + facet_wrap(~ continent, nrow = 3)

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  scale_x_log10() + facet_wrap(~ continent, ncol = 4)


gapminder %>%
  filter(year >= 1992) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  scale_x_log10() + facet_grid(continent ~ year) +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 45, hjust = .5, vjust = .5, face = "plain"))

# Salvando gráficos como arquivos

#.jpeg

jpeg("gapminder_grafico.jpeg", height = 500, width =  700, units = "px")

gapminder %>%
  filter(year >= 1992) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  scale_x_log10() + facet_grid(continent ~ year) +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 45, hjust = .5, vjust = .5, face = "plain"))

dev.off()

#.png

png("gapminder_grafico.png", height=500, width=700, units = "px")

gapminder %>%
  filter(year >= 1992) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  scale_x_log10() + facet_grid(continent ~ year) +
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 45, hjust = .5, vjust = .5, face = "plain"))

dev.off()

