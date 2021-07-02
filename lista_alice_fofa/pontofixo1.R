pontofixo <- function(p0, fns, tol = 0.000001, max.iter = 50){
  
  k <- 1
  
  while (k <= max.iter){
    
    p <- fns(p0)
    
    if ((abs((p - p0)) < tol) | ((abs(p - p0))/abs(p) < tol) | (abs(g(p) - p0) < tol)){
      
      cat('Na iteração', k, 'o valor de x é:', p, '\n')
      break
    }
    
    k <- k + 1
    
    p0 <- p
  }
  
  print('O método falhou após', k, 'iterações')
  
}

g <- function(x){return((1/2) * sqrt((10 - (x^3))))}
g1 <- function(x){return(x - x^3 - 4*(x^2) + 10)}
pontofixo(1.5, g)
pontofixo(1.5, g1)
