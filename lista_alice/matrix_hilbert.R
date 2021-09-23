n=5
h1 = matrix(nrow=n,ncol=n)

for (r in 1:n) {
  for (c in 1:n)
    h1[r,c] = 1/(r+c-1)
}  
