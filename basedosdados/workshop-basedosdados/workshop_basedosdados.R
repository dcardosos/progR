library(basedosdados)
library(tidyverse)
library(bit64)

basedosdados::set_billing_id('exemplo-curso-r')

## basedosdados
### pagesize == fatiar os resultados da query e vai processar subgrupo 
### por subgrupo, aí executa pagina por pagina
### custo: executar mto mais operações do que vc precisa  

basedosdados::read_sql(
    "SELECT * FROM `basedosdados.br_inep_ideb.escola`", 
    page_size = 100000)

## salvar no computador
basedosdados::download(
    "SELECT * FROM `basedosdados.br_inep_ideb.escola`",
    path = 'data/ideb.csv',
    page_size = 100000)

# uma pergunta
# .Last.value %>% pull(resultados)

tibble(
    query = c(
        "SELECT * FROM `basedosdados.br_inep_ideb.escola`",
        "SELECT ana.id_municipio, ana.indice_sem_atend 
         FROM `basedosdados.br_ana_atlas_esgotos.municipio` as ana",
        "SELECT * FROM `basedosdados.br_ibge_pib.municipio`")) %>%
    mutate(resultados = map(query, basedosdados::read_sql, page_size= 100000)) -> 
    queries

(queries %>% 
    pull(resultados) %>% 
    reduce(left_join) ->
    painel)


painel %>%
    mutate(across(where(is.integer64), as.integer)) %>%
    group_by(ano) %>%
    filter(ano > 2004, !is.na(ideb)) %>% 
    dplyr::summarise(
        media_ideb = mean(ideb, na.rm = TRUE)) %>%
    ggplot(aes(ano, media_ideb)) +
    geom_line(size = 1.2, color = 'green') +
    theme_minimal() +
    labs(
        x = 'Ano',
        y = 'Média de Ideb nacional',
        title = 'Evolução do Ideb nacional ao decorrer dos anos',
        caption = 'Fonte: `basedosdados`')

painel %>%
    mutate(across(where(is.integer64), as.integer)) %>%
    group_by(ano) %>%
    filter(ano > 2004, !is.na(ideb)) %>% 
    dplyr::summarise(
        media_ideb = mean(ideb, na.rm = TRUE)) %>%
    ggplot(aes(ano, media_ideb)) +
    geom_line(size = 1.2, color = 'green') +
    theme_minimal() +
    labs(
        x = 'Ano',
        y = 'Média de Ideb nacional',
        title = 'Evolução do Ideb nacional desde 2005',
        caption = 'Fonte: `basedosdados`')



### indice_sem_atend só tem em 2013
painel %>%
    mutate(across(where(is.integer64), as.integer)) %>% 
    filter(ano == 2013) %>%
    group_by(id_municipio, rede) %>%
    summarise(
        descobertura = indice_sem_atend,
        ideb_medio = mean(ideb, na.rm = TRUE),
        .groups = 'drop') %>%
    distinct(id_municipio, rede, .keep_all = TRUE) %>%
    ggplot(aes(x = descobertura, y = ideb_medio, color = rede)) +
    geom_point() +
    theme_minimal() +
    geom_smooth(method = 'lm')

### não faz sentido a análise, mas serviu de aprendizado
painel %>%
    mutate(across(where(is.integer64), as.integer),
        date_glue = glue::glue('{ano}0101'),
        date = lubridate::ymd(date_glue)) %>% 
    group_by(sigla_uf, date) %>%
    filter(date > 2004, !is.na(pib)) %>%
    dplyr::summarise(
        media_pib = mean(pib, na.rm = TRUE),
        .groups = 'drop') %>%
    ggplot(aes(date, media_pib)) +
    geom_line(aes(color = sigla_uf)) +
    theme_minimal()
 
