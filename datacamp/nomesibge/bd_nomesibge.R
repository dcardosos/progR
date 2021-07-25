library(tidyverse)
basedosdados::set_billing_id('nome-ibge')


query <- c( 
  "
  SELECT nome, SUM(quantidade_nascimentos_ate_2010) as soma_quantidade
  FROM `basedosdados.br_ibge_nomes_brasil.quantidade_municipio_nome_2010`
  GROUP BY nome
  ")

basedosdados::read_sql(query1, page_size = 100000) -> 
  da_nomes


da_nomes %>% 
  mutate(across(where(bit64::is.integer64), as.integer)) %>% 
  top_n(10, soma_quantidade) %>% 
  ggplot(aes(nome, soma_quantidade, fill = nome)) +
  geom_bar(stat = 'identity') + 
  theme_minimal() +
  labs(
    x = 'Nomes',
    y = 'Quantidade',
    title = 'Os 10 nomes mais frequentes até 2010',
    caption = 'Fonte: `IBGE: Censo 2010`')




query1 <-
  "
  WITH quant_nascimentos AS (
    SELECT id_municipio, nome, SUM(quantidade_nascimentos_ate_2010) as soma_quantidade
    FROM `basedosdados.br_ibge_nomes_brasil.quantidade_municipio_nome_2010`
    GROUP BY id_municipio, nome)
    
  SELECT t2.municipio, t2.sigla_uf, t1.nome, t1.soma_quantidade, 
  FROM quant_nascimentos t1
  JOIN `basedosdados.br_bd_diretorios_brasil.municipio` t2
  ON t1.id_municipio = t2.id_municipio  
"


basedosdados::read_sql(query1, page_size = 100000) -> 
  da_municipios


top10_municipios <- function(name){
  
  da_municipios %>%
    mutate(across(where(bit64::is.integer64), as.integer)) %>%
    filter(nome == name) %>% 
    top_n(10, soma_quantidade) %>% 
    ggplot(aes(reorder(municipio, soma_quantidade), soma_quantidade)) + 
    geom_bar(stat = 'identity', fill = 'dark green') +
    theme_minimal() +
    labs(
      x = 'Municipios',
      y = 'Quantidade',
      title = glue::glue('Os 10 municípios que mais possuem "{name}" até 2010')) +
    theme(plot.title = element_text(lineheight = 2))
}

top10_nome <- function(nome_municipio){
  
  da_municipios %>%
    mutate(across(where(bit64::is.integer64), as.integer)) %>%
    filter(municipio == nome_municipio) %>% 
    top_n(10, soma_quantidade) %>% 
    ggplot(aes(reorder(nome, soma_quantidade), soma_quantidade)) + 
    geom_bar(stat = 'identity', fill = 'dark green') +
    theme_minimal() +
    labs(
      x = 'Nomes',
      y = 'Quantidade',
      title = glue::glue('Os 10 nomes mais frequentes em {nome_municipio} até 2010')) +
    theme(plot.title = element_text(lineheight = 2))
}

barplot_estado <- function(name) {
  
  da_municipios %>%
    mutate(across(where(bit64::is.integer64), as.integer)) %>%
    filter(nome == name) %>%
    group_by(sigla_uf) %>% 
    summarise(total_estado = sum(soma_quantidade)) %>% 
    ggplot(aes(reorder(sigla_uf, total_estado), total_estado)) +
    geom_bar(stat = 'identity') +
    theme_minimal() +
    labs(
      x = 'Estado',
      y = 'Quantidade',
      title = glue::glue('Quantide de {name} por Estado até 2010'))
}


barplot_estado('Alice')


## popularidade por estado
pop_nome <- function(.estado, .top_n = 10) {
  
  da_municipios %>%
    mutate(across(where(bit64::is.integer64), as.integer)) %>%
    filter(sigla_uf == .estado) %>% 
    mutate(total_estado = sum(soma_quantidade)) %>%
    group_by(nome) %>% 
    summarise(fracao = sum(soma_quantidade) / total_estado, 
              .groups = 'drop') %>% 
    distinct(nome, .keep_all = TRUE) %>% 
    top_n(.top_n, fracao) %>% 
    ggplot(aes(reorder(nome, fracao), fracao)) +
    geom_bar(stat = 'identity', fill = 'dark blue') +
    theme_minimal() +
    labs(
      x = 'Nome',
      y = 'Popularidade',
      title = glue::glue('Os {.top_n} nomes mais populares no estado de {.estado}'),
      subtitle = 'Dados agregados até 2010',
      caption = 'Fonte: `Base dos Dados: br_ibge_nomes_brasil.quantidade_municipio_nome_2010`')
    
}


aplicacao <- function(estados) {
  
  tibble(
    estados = estados,
    graficos = purrr::map(.x = estados, 
                          .f = ~ pop_nome(.x))) %$% 
    reduce(graficos, `+`)  
  
}

aplicacao(c('SP', 'RJ', 'MG', 'RO'))  
