library(magrittr)
library(ggplot2)

# Read data -----------------------------------------------------------
dados <- readr::read_csv('data/indicadores.csv', skip = 2, col_select = -1,
                         locale = readr::locale(decimal_mark = ","),
                         col_names = FALSE,
                         col_types = list(
                           X2 = readr::col_character(),
                           X3 = readr::col_character(),
                           X4 = readr::col_character(),
                           X5 = readr::col_double(),
                           X6 = readr::col_double(),
                           X7 = readr::col_double()))

# Rename cols -----------------------------------------------------------
dados %>% 
  dplyr::rename(
    'setor_economico'= X2,
    'empresa' = X3,
    'descricao' = X4,
    'indice_pre_adocao' = X5,
    'indice_pos_adocao' = X6,
    'variacao' = X7) ->
  da

# Group by --------------------------------------------------------------
## média amostral
da %>% 
  dplyr::group_by(descricao) %>%  
  dplyr::rename('indicador' = descricao) %>% 
  dplyr::summarise(
    media_pre = mean(indice_pre_adocao),
    media_pos = mean(indice_pos_adocao),
    variacao = media_pre - media_pos)

## mediana amostral
da %>% 
  dplyr::group_by(descricao) %>%  
  dplyr::rename('indicador' = descricao) %>% 
  dplyr::summarise(
    mediana_pre = stats::median(indice_pre_adocao),
    mediana_pos = stats::median(indice_pos_adocao),
    variacao = mediana_pre - mediana_pos)

## desvio padrão amostral
da %>% 
  dplyr::group_by(descricao) %>%  
  dplyr::rename('indicador' = descricao) %>% 
  dplyr::summarise(
    std_pre = sd(indice_pre_adocao),
    std_pos = sd(indice_pos_adocao),
    variacao = std_pre - std_pos)

## Box plot
da %>% 
  dplyr::select(descricao, indice_pre_adocao, indice_pos_adocao) %>% 
  tidyr::pivot_longer(c(indice_pre_adocao, indice_pos_adocao), 
                      names_to = "periodo", 
                      values_to = "indice") %>% 
  ggplot(aes(x = periodo, y = indice, color = descricao)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = 'Período',
    y = 'Valor') +
  scale_y_log10()

## Wilcox test - geral
da %>% 
  dplyr::group_by(descricao) %>%
  dplyr::select(indice_pre_adocao, indice_pos_adocao) %>% 
  dplyr::group_nest() %>% 
  dplyr::mutate(
    wilcox = purrr::map(.x = data,
                         .f = 
                           ~ stats::wilcox.test(.x$indice_pre_adocao,
                                                .x$indice_pos_adocao,
                                                paired = TRUE))) %$%
  wilcox %>% 
  purrr::set_names(unique(da$descricao))
