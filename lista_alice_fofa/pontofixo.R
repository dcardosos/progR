fixedpoint <- function(ftn, x0, tol = 0, max.iter = 100) {

  xold <- x0
  xnew <- ftn(xold)
  iter <- 1
  cat("At iteration 1 value of x is:", xnew, "\n")
  
  while ((abs(xnew-xold) > tol) && (iter < max.iter)) {
    xold <- xnew;
    xnew <- ftn(xold);
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", xnew, "\n")
  }
  
  if (abs(xnew-xold) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
    return(xnew)
  }
}

g3 <- function(x){return((1/2) * sqrt((10 - (x^3))))}
fixedpoint(ftn = g3, x0 = 1.5)
