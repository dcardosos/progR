library(tidyverse)
library(magrittr)

(letter <- sample(c(sample(letters, 6), rep(NA_character_, 3))))

letter %<>%             # self-assignment
  replace_na('douglas')

letter
