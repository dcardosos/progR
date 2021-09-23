# LENDO OS DADOS -----------------------------------------
library(magrittr)
dados <- readr::read_csv('planilhas.csv',
                         col_types = list(IDADE = readr::col_integer()))

dplyr::glimpse(dados)

# FORMATANDO OS DADOS ------------------------------------
dados %>%
  dplyr::rename(
    'LESÕES' = Lesões,
    'FRATURAS' = "FRATURAS ASSOCIADAS",
    'DATA_TRAUMA' = "DATA TRAUMA",
    'DATA_CIRURGIA' = "DATA CIRURGIA",
    'DATA_ALTA' = "DATA ALTA",
    'TTO_NÃO_CIRURGICO' = "TTO NÃO-CIRÚRGICO",
    'HISTÓRICO' = "HIST. MÉDICA") %>% 

  dplyr::mutate_at(c('RAÇA', 'ETIOLOGIA', 'RESOLUÇÃO', 'LESÕES', 'FRATURAS', 'HISTÓRICO'), 
                   stringr::str_to_lower) %>% 
  
  dplyr::mutate(ETIOLOGIA = ifelse(
    test = stringr::str_detect(ETIOLOGIA, "doméstica"),
    yes = "Agressão doméstica",
    no = ETIOLOGIA)) %>%
  dplyr::mutate(ETIOLOGIA = ifelse(
    test = stringr::str_detect(ETIOLOGIA, "física"),
    yes = "Agressão física",
    no = ETIOLOGIA)) %>%
  dplyr::mutate(ETIOLOGIA = ifelse(
    test = stringr::str_detect(ETIOLOGIA, "fisica"),
    yes = "Agressão física",
    no = ETIOLOGIA)) %>% 
  dplyr::mutate(ETIOLOGIA = ifelse(
    test = stringr::str_detect(ETIOLOGIA, "doméstico"),
    yes = "Acidente doméstico",
    no = ETIOLOGIA)) %>%
  dplyr::mutate(ETIOLOGIA = ifelse(
    test = stringr::str_starts(ETIOLOGIA, "acidente com animal"),
    yes = "Acidente com animal",
    no = ETIOLOGIA)) %>% 
  dplyr::mutate(RAÇA = stringr::str_to_lower(RAÇA)) %>% 
  dplyr::mutate(RAÇA = ifelse(
    test = RAÇA == "pardo",
    yes = "parda",
    no = RAÇA)) %>% 
  dplyr::mutate(RAÇA = ifelse(
    test = RAÇA %in% c("/", "-"),
    yes = NA,
    no = RAÇA)) %>% 
  dplyr::mutate(RAÇA = ifelse(
    test = RAÇA == "amalero",
    yes = "amarela",
    no = RAÇA)) %>% 
  dplyr::mutate(CIRÚRGICO = stringr::str_replace_all(CIRÚRGICO, "X", "1")) %>% 
  dplyr::mutate(CIRÚRGICO = as.integer(CIRÚRGICO)) %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character), 
      .fns = ~ dplyr::na_if(., "/")),
    dplyr::across(
      where(is.character), 
      .fns = ~ dplyr::na_if(., "-"))) %>%
  dplyr::mutate(
    dplyr::across(DATA_TRAUMA:DATA_ALTA, ~ as.Date(., "%d/%m/%Y"))) %>% 
  
  readr::write_excel_csv('dados.csv')

