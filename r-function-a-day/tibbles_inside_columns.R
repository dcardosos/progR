library(tidyverse)

(df <- tibble(
  user_id = 1:2,
  demographics = tibble(age = c(19,24), sex = c('m', 'f')),
  purchase = tibble(items = c('watch', 'glasses'), price = c('120', '45'))
  ))

df %>%
  unpack(cols = c(demographics, purchase))
