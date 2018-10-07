#author M. Adzka Sari'ul Fahmi R.
#excercise
#no1
rule10 <- function(x){
  return(1/(2*sqrt(x)))
}


library(Ryacas)
x <- Sym("x")
Simplify(deriv(sqrt(3*x), x))

#no2
#2.1
satu <- function(n,x){
  return(n*x^(x-1))
}

library(Ryacas)
x <- Sym("x")
Simplify(deriv(2*x^5, x))

#2.2
dua <- function(n,x){
  return(n*x^(x-1))
}

library(Ryacas)
x <- Sym("x")
Simplify(deriv(x^2+4, x))

#2.3
tiga <- function(n,x){
  return(n*x^(x-1))
}

library(Ryacas)
x <- Sym("x")
Simplify(deriv(x^5-6*x^7, x))
