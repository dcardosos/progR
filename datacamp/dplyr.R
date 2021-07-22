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
  filter(name == 'David')
  


  