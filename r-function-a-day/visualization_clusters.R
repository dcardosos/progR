library(tidyverse)
library(palmerpenguins)
library(ggforce)
library(concaveman)

ggplot(
  drop_na(penguins), 
  aes(flipper_length_mm, body_mass_g)) + 
  geom_mark_hull(aes(fill = species, label = species), con.cap = 0) +
  geom_point()

ggplot(
  drop_na(penguins), 
  aes(flipper_length_mm, body_mass_g)) +
  geom_mark_ellipse(aes(fill = species, label = species), con.cap = 0) +
  geom_point()
