library(tidyverse)
library(corrr)

dados <- readr::read_delim('input', 
                           delim = '\t', 
                           locale = locale(decimal_mark = ','))  

dados %>%
  rename(
    qualidade_produto = x1,
    solucao_reclamacoes = x2,
    profundidade_amplitude_produto = x3,
    imagem_forca_venda = x4,
    processo_encomenda_cobranca = x5,
    velocidade_entrega = x6,
    satisfacao_geral = y) ->
  data 

## simple correlation
(data %>%
  select(-id) %>%
  corrr::correlate() ->
  correlation)

## network plot
correlation %>%
  corrr::network_plot(colors = c('green'))

## focus specific terms 
correlation %>%
  corrr::focus(
    qualidade_produto:velocidade_entrega,
    mirror = FALSE) # TRUE não filtra as linhas

correlation %>%
  corrr::focus(satisfacao_geral)

## geom_bar correlation
correlation %>%
  corrr::focus(satisfacao_geral) %>%
  mutate(rowname = reorder(term, satisfacao_geral)) %>%
  ggplot(aes(rowname, satisfacao_geral)) +
  geom_col(aes(fill = satisfacao_geral >= 0)) +
  coord_flip() # vira o grafico


## shave, upper triangle
correlation %>%
  corrr::shave()

## to excel
correlation %>%
  corrr::shave() %>%
  corrr::fashion() %>%
  readr::write_excel_csv('correlation-matrix.csv')

## rplot correlation

correlation %>%
  corrr::rearrange(
    method = 'MDS', 
    absolute = FALSE) %>%
  corrr::shave() %>%
  corrr::rplot(
    shape = 19, 
    colors = c('purple', 'green'))


## linear regression

dados %>%
  select(-id) %>%
  lm(y ~ x1+x2+x3+x4+x5+x6, data = .) %>%
  summary()

### retirando o x6, menor t-value
dados %>%
  select(-id) %>%
  lm(y ~ x1+x2+x3+x4+x5, data = .) %>%
  summary()

### retirando o x5, menor t-value
dados %>%
  select(-id) %>%
  lm(y ~ x1+x2+x3+x4, data = .) %>%
  summary() 