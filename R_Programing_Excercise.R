#Tugas R Programing Series
# Nama : M. ADZKA SARI'UL FAHMI R. (17523181) | MUHAMMAD ABDI HUMANIKA (17523155)
#Limit
#no1
f <- function(x) {
  fx <- (1 - cos(x)) / x
  return(fx)
}
x <- Sym("x")
Limit(f(x), x, 0)

#no2
f <- function(h){
  fx <- ((2*(h-3)^2)-18) / (h)
  return(fx)
}
h <- Sym("h")
Limit(f(h), h, 0)

#no3
f <- function(t){
  fx <- (t-sqrt(3*t+4)) / (4-t)
  return(fx)
}
t <- Sym("t")
Limit(f(t), t, 4)

#==================================
#differentiation
#no1
rule1 <- function(x){
  return(1/(2*sqrt(x)))
}


library(Ryacas)
x <- Sym("x")
Simplify(deriv(sqrt(x)*(x+1), x))

#no2
rule2 <- function(x){
  return(n*x^(x-1))
}


library(Ryacas)
x <- Sym("x")
Simplify(deriv((2*x^2-3)/sqrt(x), x))

#no3
rule3 <- function(x){
  return(1/(2*sqrt(x)))
}


library(Ryacas)
x <- Sym("x")
Simplify(deriv((x-1)/(x+1), x))

#no4
f1 <- function(x){
  hasil <- (2 * x + (x + 1))/(2 * root(x, 2))
  return(hasil)
}
f1(4)

f2 <- function(x){
  hasil <- (8 * x^2 + (-2 * x^2 + 3))/(2 * (root(x, 2) * x))
  return(hasil)
}
f2(8)

f3 <- function(x){
  hasil <- 2/(x^2 + 2 * x + 1)
  return(hasil)
}
f3(2^3)

#===============================
#Integration
#no1
integrand <- function(x){
  return (2 * x^3)
}

integrate(f = integrand, lower = 0, upper = 3)

library(Ryacas)
x <- Sym ("x")
Integrate(2 * x^3, x)


#no2
integrand <- function(x){
  return(1-5 * x^4)
}

integrate(f = integrand, lower = -1, upper = 2)

library(Ryacas)
x <- Sym ("x")
Integrate( 1-5 * x^4, x)


#no3
integrand <- function(x){
  return(x^4 - 3*x^2 + 5)
}

integrate(f = integrand, lower = -2, upper = 2)

library(Ryacas)
x <- Sym ("x")
Integrate( x^4 - 3*x^2 + 5 )


#no4
integrand <- function(x){
  return( x^2 + ( (1) / 2 * sqrt (x) ) )
}

integrate(f = integrand, lower = 1, upper = 4 )

library(Ryacas)
x <- Sym ("x")
Integrate(  x^2 + ( (1) / 2 * sqrt (x) ),x) 


#no5
integrand <- function(x){
  return ( (2 - 3*x)^2 )
}

integrate( f = integrand, lower = 0 , upper = 2 )

library(Ryacas)
x <- Sym("x")
Integrate( (2 - 3*x)^2, x)

