library(tidyverse)

irr <- function(ct, irr, t, c0){
  
  Ct / (1 + IRR)^t - C0 = 0
}

CO = Ct * 1/(1 + IRR)^t

CO / Ct = 1/(1 + IRR)^t

(CO / Ct)^(1/t) = 1 / (1 + IRR)


tibble(
  year = 0:5,
  cf = c(-500, 150, 170, 178, 250, 300)) %>%
  mutate(
    IRR = 
  )
