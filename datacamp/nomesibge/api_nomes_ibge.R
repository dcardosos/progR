library(tidyverse)
library(magrittr)
library(patchwork)

## `get_data` function ------------------------------------------------------

get_data <- function(.nomes) {
  
  api <- 'https://servicodados.ibge.gov.br/api/v2/censos/nomes/' 
  
  if (length(.nomes) == 1){
    
    consulta <- glue::glue(api, .nomes) %>% 
      jsonlite::fromJSON(.) 
    
  } else {
    
    inputs <- .nomes %>% 
      paste0(collapse = '|')
    
    consulta <- glue::glue(api, inputs) %>% 
      jsonlite::fromJSON(.) 
    
  }
  
  if(!is.data.frame(consulta)) {
    
    rlang::abort('There is an invalid name!')
    
  }
  
  consulta
  
}

## `clean_data` function ------------------------------------------------------

clean_data <- function(consulta, .nomes) {
  
  consulta %$% 
    res %>% 
    purrr::set_names(.nomes) %>% 
    tibble::enframe() %>% 
    mutate(
      limpo = map(
        .x = value,
        .f = function(df){
          
          df %>% 
            mutate(
              periodo = readr::parse_number(periodo) %>% 
                map_chr(~ ifelse(
                  test = nchar(.x) <= 5,
                  yes = stringr::str_sub(.x, 1, 4),
                  no = stringr::str_sub(.x, 5, 8))))}
      ))
  
}

## `viewer_plot` ggplot function ---------------------------------------------

sep_plot <- function(df){
  
  df %>% 
    mutate(
      graficos = map2(
        .x = limpo,
        .y = name,
        .f = 
          ~ ggplot(.x, aes(x = periodo, y = frequencia, group = 1)) +
          geom_line(size = 1.2) +
          ylim(0, 200000) +
          theme_minimal() +
          labs(
            x = 'Década',
            y = 'Frequência',
            title = glue::glue('Frequência de nascimentos com o nome {stringr::str_to_title(.y)} por década'),
            subtitle = 'Consulta via `API Nomes - IBGE`',
            caption = 'Fonte: IBGE'))) %$%
    
    reduce(graficos, `+`)
}


## `agg_plot` function -------------------------------------------------------

agg_plot <- function(df, .nomes) {
  
  df %>% 
    purrr::pluck('limpo') %>% 
    purrr::reduce(~ left_join(..., by = 'periodo')) %>%  
    purrr::set_names(c('periodo', .nomes)) %>%
    pivot_longer(
      cols = all_of(.nomes),
      values_to = 'frequencia',
      names_to = 'nome') %>%
    mutate(periodo = lubridate::date(glue::glue('{periodo}-01-01'))) %>% 
    ggplot(aes(periodo, frequencia, color = nome)) +
    geom_line() +
    theme_minimal() +
    scale_y_log10() +
    labs(
      x = 'Década',
      y = 'Frequência',
      title = glue::glue('Frequência de nascimentos por década'),
      subtitle = 'Consulta via `API Nomes - IBGE`',
      caption = 'Fonte: IBGE')
}

## api function --------------------------------------------------------------
api_nomes <- function(.nomes, .output = 'tibble'){
  
  ## modo de aplicação ---------------------
  
  acceptable_outputs <- c('tibble', 'sep_plot', 'agg_plot')
  
  if (! .output %in% acceptable_outputs){
    
    msg <- glue::glue("
      Supplied `.output` argument: {.output}
      Acceptable `.output` arguments {purrr::reduce(acceptable_outputs, paste, sep = ',' )}")
    
    rlang::abort(msg)
  }
  
  list(
    'tibble' = function(x) { x %$% limpo },
    'sep_plot' = function(x) { sep_plot(x) },
    'agg_plot' = function(x) { agg_plot(x, .nomes = .nomes) }) %>% 
    purrr::pluck(.output) ->
    output_formatter
  
  ## lógica da função --------------------
  
  get_data(.nomes) %>% 
    clean_data(.nomes) %>% 
    output_formatter()
  
}

## pensar rlang pra erro desse tipo
api_nomes(c('aasg', 'douglas'))

## nomear os tibbles quando pedimos apenas eles
## tibble_list ou apenas tibble?

## exemplo de uso -----------------------------------------------------------

api_nomes(c('douglas', 'raissa', 'gustavo', 'marisa', 'alice'),
          .output = 'agg_plot')
