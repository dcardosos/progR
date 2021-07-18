library(tidyverse)
library(corrplot)
library(heatmaply)
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

dados <- read_csv('dados.csv')

dados %>% 
  select(-Período) %>%
  rename(
    y = CIDP,
    x1 = HMOD,
    x2 = HSS) %>%
  cor() ->
  corr

## correlation matrix
corr %>% 
  corrplot(type = 'lower', order = 'hclust',
           tl.col = 'black')

corr %>%
  heatmap(symm = TRUE)

corr %>%
  heatmaply_cor(
    k_col = 2,
    k_row = 2
  )
  

## multicolinearidade: algumas variáveis preditoras estão correlacionadas 
## a outras variáveis preditoras
## pode aumentar a variância dos coeficientes de regressão, tornando-os
## instáveis 
## dificulda o entendimento dos efeitos isoaldos de x1 e x2 em y

## como identificar? 
## sinais inesperados nos coeficientes, boa correlação com Y mas com
## problemas no t-Student
dados %>% 
  select(-Período) %>%
  rename(
    y = CIDP,
    x1 = HMOD,
    x2 = HSS) ->
  data


(modelo <- lm(y ~ x1+x2, data = data))

## Como usamos apenas uma amostra de observações para
## estimar uma equação de regressão, podemos esperar que
## os coefi cientes de regressão variem se selecionarmos ou-
##   tra amostra de observações e estimarmos outra equação
## de regressão. Não queremos considerar repetidas amos-
##   tras; assim, precisamos de um teste empírico para ver se
## o coefi ciente de regressão estimado tem algum valor real
## (i.e., é diferente de zero?) ou se poderíamos esperar que
## ele se iguale a zero em outra amostra.

## teste de hipótese para cada coeficiente: t-Student
## H0: o coeficiente de cada variável independente é igual a zero
## H1: o coeficiente de cada variável independente é diferente de zero
## se o p-value (Pr(>|t|)) < 0.05, sim
## se o t-value < 1.96, sim

## F-statistic: há evidências de que pelo menos uma variável no modelo está
## relacionada com a variável dependente?
## H0: os coeficientes de todas as variáveis independentes são iguais a zero.
## H1: existe pelo menos um deles diferente de zero.
## se o p-value < 0.05, então sim, é relevante

## R squared: explicam qual % da variabilidade do do y?
summary(modelo)

### reject x1, because t-value < 1,96 and p-value > 0,05
## new model

(new_model <- lm(y ~ x2, data = data))
summary(new_model)
