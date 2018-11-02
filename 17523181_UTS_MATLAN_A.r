#NO1
A <- matrix(data = c(1:12), nrow=4, ncol=3)
print(A)
B <- matrix(data = c(1:15), nrow=3, ncol=5)
print(B)

#(a)
aa <- A%*%B
print(aa)

#(b)
bb <- t(B)%*%t(A)
print(bb)

#(c)
print(A)
C <- A[-2,-3]
C <- C[-3,]
print(C)

#(d)
dd <- det(C)
print(dd)

#(e)
ee <- solve(C)
print(ee)

#===========================
#NO2
library(matlib)
ma <- matrix(c(1,2,3,4,5,2,1,2,3,4,3,2,1,2,3,4,3,2,1,2,5,4,3,2,1),5,5,TRUE)
jw <- c(7,-1,-3,5,17)

showEqn(ma,jw)

echelon(ma,jw,TRUE,FALSE)

#===========================
#NO3
D <- c(1:10)
print(D)
E <- c(1,3,5,7,9)
print(E)

#(a)
aa <- union(D,E)
print(aa)

#(b)
bb <- union(intersect(D,E),E)
print(bb)

#(c)
cc <- setdiff(D,E)
print(cc)

#(d)
dd <- intersect(union(D,E),intersect(D,E))
print(dd)


#===========================
#NO4
#(a)
f <- function(x){
  result <- x^2+4*x+5
  return(result)
}

g <- function(x){
  result <- x-1
  return(result)
}

#(b)
f <- function(x){
  result <- x^2+4*x+5
  return(result)
}

input <- -20:20
plot(input, sapply(input, f),type = "l", xlab = "x", ylab = "f(x)")

#(c)
fog <- function(x){
  hasil <- f(g(x))
  return(hasil)
}
fog(2)

#===========================
#NO5
#(a)
aa <- function(x){
  return(1/(2*sqrt(x)))
}
library(Ryacas)
x <- Sym("x")
Simplify(deriv((5*x+1)/sqrt(x)+1,x))

#(b)
bb <- function(x){
  return(1/(2*sqrt(x)))
}
library(Ryacas)
x <- Sym("x")
Simplify(deriv(5*x^-2-x+1/4*x^-1,x))

#(c)
cc <- function(x,n){
  return(n*x^(n-1))
}
library(Ryacas)
x <- Sym("x")
Simplify(deriv(1/(x+1),x))

#===========================
#NO6
#(a)
aa <- function(x){
  fx <- ((x+h)^3-x^3/h)
  return(fx)
}
library(Ryacas)
x <- Sym("x")
Limit(aa(x),h,0)

#(b)
bb <- function(x){
  fx <- sqrt(x^2)-x
  return(fx)
}
library(Ryacas)
x <- Sym("x")
Limit(bb(x),x,0)


#===========================
#NO7
sv <- function(x){
  fx <- sin(2*x)*((x^2)/(5*x^2 + 2))
  return(fx)
}
p1 <- 3.14/2
p2 <- 2*3.14

input <- p1:p2
plot(input, sapply(input, sv),type = "l", xlab = "x", ylab = "f(x)")