library(tidyverse)
library(gapminder)

# Summarize medianGdpPercap within each continent within each year: by_year_continent
# Plot the change in medianGdpPercap in each continent over time

gapminder %>%
  group_by(continent, year) %>%
  summarise(
    medianGdpPercap = median(gdpPercap, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = medianGdpPercap, color = continent)) +
  geom_point() +
  expand_limits(y = 0)

# Summarize the median GDP and median life expectancy per continent in 2007
# Use a scatter plot to compare the median GDP and median life expectancy

gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(
    medianLifeExp = median(lifeExp),
    medianGdpPercap = median(gdpPercap)) %>%
  ggplot(aes(x = medianGdpPercap, y = medianLifeExp, color= continent)) +
  geom_point() +
  expand_limits(y = 0)

# Summarize the median gdpPercap by year, then save it as by_year
# Create a line plot showing the change in medianGdpPercap over time

gapminder %>% 
  group_by(year) %>% 
  summarise(medianGdpPercap = median(gdpPercap)) %>%
  ggplot(aes(x = year, y = medianGdpPercap)) +
  geom_line() +
  expand_limits(y = 0)

### by_year_continent
gapminder %>% 
  group_by(year, continent) %>% 
  summarise(medianGdpPercap = median(gdpPercap), .groups = 'drop') %>%
  ggplot(aes(x = year, y = medianGdpPercap, color = continent)) +
  geom_line() +
  expand_limits(y = 0)

# Summarize the median gdpPercap by continent in 1952
# Create a bar plot showing medianGdp by continent
gapminder %>% 
  filter(year == 1952) %>% 
  group_by(continent) %>% 
  summarise(medianGdpPercap = median(gdpPercap)) %>%
  ggplot(aes(x = continent, y = medianGdpPercap)) +
  geom_bar(stat = 'identity')

# Filter for observations in the Oceania continent in 1952
# Create a bar plot of gdpPercap by country
gapminder %>%
  filter(year == 1952, continent == 'Oceania') %>%
  ggplot(aes(x = country, y = gdpPercap)) +
  geom_bar(stat = 'identity')

# Create a histogram of population (pop_by_mil)
gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000) %>% 
  ggplot(aes(pop_by_mil)) + 
  geom_histogram(bins = 50)

# Create a histogram of population (pop), with x on a log scale
gapminder %>%
  filter(year == 1952) %>%
  ggplot(aes(pop)) + 
  geom_histogram(bins = 50) +
  scale_x_log10()

# Create a boxplot comparing gdpPercap among continents
gapminder %>%
  filter(year == 1952) %>% 
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    x = 'Continent',
    y = 'GDP Percap',
    title = "Comparing GDP per capita across continents") +
  theme_minimal()

