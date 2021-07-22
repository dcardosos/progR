library(tidyverse)
library(lubridate)

(tibble(
  ano = c(ymd(20190101), ymd(20200101)),
  receita_liquida = c(5285176,  4085486),
  apparel = c( 4125950, 3121052),
  fashiontronics = c(925899,  801837)) ->
  data)

data %>%
  ggplot(aes(ano, receita_liquida)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(
    x = 'Ano',
    y = 'Receita Líquida'
  )

  
  library(magrittr) # needed for %>% if dplyr is not attached

"http://pastebin.com/raw.php?i=L8cEKcxS" %>%
  read.csv(sep = ",") %>%
  tidyr::pivot_longer(cols = c(Food, Music, People.1),
                      names_to = "variable",
                      values_to = "value") %>%
  dplyr::group_by(variable, value) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(value = factor(
    value,
    levels = c("Very Bad", "Bad", "Good", "Very Good"))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(variable, n)) +
  geom_bar(aes(fill = value), stat = 'identity')
