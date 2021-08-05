library(tidyverse)
library(gapminder)

# Understanding your data
glimpse(gapminder)

# arrange and filter
gapminder %>%
  group_by(year, continent) %>%  
  summarise(medianGdpPercap = median(gdpPercap),
            .groups = 'drop') %>% 
  arrange(desc(medianGdpPercap)) %>%
  filter(continent == 'Africa')

# count
gapminder %>% 
  count(continent, sort = TRUE)

gapminder %>% 
  group_by(year) %>% 
  count(continent, wt = pop, sort = TRUE)

# summarise
gapminder %>%
  group_by(year) %>% 
  summarise(
    gdp_percap_medio = median(gdpPercap),
    life_exp_medio = median(lifeExp))

# top_n verb
gapminder %>%
  filter(year == 2007) %>% 
  group_by(continent) %>%
  top_n(1, gdpPercap)

gapminder %>%
  filter(year == 2007) %>% 
  group_by(continent) %>%
  top_n(3, gdpPercap)

# combine select and rename
gapminder %>%
  select(lifeExp, ano = year, pais = country)

# transmute: select + mutate, dropa as outras colunas
gapminder %>%
  transmute(!c(lifeExp, pop)) # isso da erro

gapminder %>%
  transmute(year, continent, pib = gdpPercap * pop)

# ---------------------------------------

# babynames projects
url <- 'https://www.nrscotland.gov.uk/files//statistics/babies-names/20/babies-first-names-all-names-all-years.csv'

babynames <- readr::read_csv(url)

# most common in year
babynames %>%
  select(!c(rank, position, sex)) %>%
  rename(
    year = yr,
    name = FirstForename) %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>% 
  mutate(
    fraction = number / year_total) %>% 
  group_by(year) %>% 
  top_n(1, fraction) %>%
  arrange(desc(year))


## fracao maxima dos nomes em determinado ano - popularidade
babynames %>%
  select(!c(rank, position, sex)) %>%
  rename(
    year = yr,
    name = FirstForename) %>%
  group_by(name) %>% 
  mutate(
    max_n = max(number),
    sum_N = sum(number)) %>% 
  ungroup() %>% 
  mutate(fraction_max = number / max_n) %>% 
  filter(name %in% c('David', 'John', 'Mark')) %>% 
  ggplot(aes(year, fraction_max, color = name)) + 
  geom_line()

popularidade <- function(name) {
    
  babynames %>%
    group_by(yr) %>%
    mutate(year_total = sum(number)) %>%
    ungroup() %>% 
    mutate(fraction = number / year_total) %>%
    filter(FirstForename %in% name, number > 2) %>% 
    ggplot(aes(yr, fraction, color = FirstForename)) +
    geom_line() +
    theme_minimal() +
    labs(
      x = 'Ano',
      y = 'Quantidade pelo Total',
      title = glue::glue(
        'Evolução da popularidade do nome na Escócia'))
}

popularidade('Douglas')
popularidade(c('Douglas', 'Alice', 'Olivia', 'David'))

# lag function
dplyr::lag(c(1,2,3))