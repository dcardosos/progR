library(tidyverse)

gera <- function(x, n, aumento){
  
  i <- 0
  vetor <- c()
  while(i < n){
    vetor <- c(vetor, x)
    x <- x + aumento
    i <- i + 1
  }
  vetor
}


tibble(
  debt = seq(0, 1, .05),
  Ke = gera(.13, length(debt), 0.002),
  Kd = gera(.066, length(debt), 0.008),
  Kd_IR = map_dbl(
    .x = Kd, ~ .x * (1 - .34)),
  WACC = pmap_dbl(
    list(debt, Kd_IR, Ke), function(x, y, z) (x * y) + (z * (1 - x)))) ->
  
  tabela


plot(x = tabela$debt, y = tabela$Ke, 
     ylim = c(0.04, 0.2), type = 'l', col = 'red')

lines(x = tabela$debt, y =tabela$WACC, col = 'blue', lty = 1)
lines(x = tabela$debt, y = tabela$Kd_IR, col = 'green', lty = 1)

legend("topright", 
       legend = c("Ke", "WAAC", "KD_IR"), 
       col = c("red","blue", "green"),
       lty = 1,
       text.width = strwidth("AAAAAA"))

