f1 <- function(x){
  result <- x^3-5
  return(result)
}
f1(2)

f2 <- function(x){
  result <- sqrt(x)
  return(result)
}
f2(6)
#=========================================

#exercise atas

#no 1.1
f3 <- function(x){
  d <- x^3+x^2-6
  return(d)
}
f3(2)

#no 1.2
f4<-function(x,y){
  result<-x*y*(y-x)
  return(result)
}
f4(2,10)

#no 1.3
f5<-function(m,n){
  x<-sqrt(m/n)+m-2*n
  return(x)
}
f5(16,4)

#no 2.1
h<- matrix(data = 1:9,3,3,TRUE)
i<- matrix(data = 10:18,3,3,TRUE)
f6<-function(a,b){
  hasil<-(a+b)%*%a%*%b
  return(hasil)
}
f6(h,i)

#no 2.2
h<- matrix(data = 4:9,2,2,TRUE)
i<- matrix(data = 10:18,2,2,TRUE)
f7<- function(m,n){
  hasil<-det(m)%*%n-m%*%n
  return(hasil)
}
f7(h,i)

#no 2.3
h<- matrix(data = 1:4,2,2,TRUE)
f8<- function(x){
  hasil<-solve(x)%*%x-2*x
  return(hasil)
}
f8(h)

#==================================

#excercise bawah 

#f(x)=sin(x)
f9<- function(x){
  hasil<- sin(x)
  return(hasil)
}
input <- 0:10
plot(input, sapply(input, f9), type = "l",xlab = "x",ylab = "f(x)")

#f(x)=log(x)
#log tanpa basis
f9<- function(x){
  hasil<- log(x)
  return(hasil)
}
input <- 0:100
plot(input, sapply(input, f9), type = "l",xlab = "x",ylab = "f(x)")

#log dengan basis 10
f9<- function(x){
  hasil<- log10(x)
  return(hasil)
}
input <- 0:10
plot(input, sapply(input, f9), type = "l",xlab = "x",ylab = "f(x)")

#akarx - 2
f10<- function(x){
  hasil<- sqrt(x)-2
  return(hasil)
}
input<- 0:10
plot(input, sapply(input, f10),type = "l", xlab = "x",ylab = "f(x)")

#x-2akar
f11<- function(x){
  hasil<- sqrt(x-2)
  return(hasil)
}
input<- 2:10
plot(input, sapply(input, f11),type = "l", xlab = "x",ylab = "f(x)")


#=============================
#plotting

#Constant Function
f12<- function(x){
  hm <- 5
  return(hm)
}

input <- 0:10
plot(input, sapply(input, f12), type = "l", xlab = "x", ylab = "f(x)")

#Linear Function
f13<- function(x){
  hm<- x*+10-5
  return(hm)
}

input<- 0:10
plot(input, sapply(input, f13), type = "l", xlab = "x", ylab = "f(x)")

#Quadratic Function
f14<- function(x){
  hm<- 3*x^2+2*x+2
  return(hm)
}

input<- -20:20
plot(input, sapply(input, f14), type = "l", xlab = "x", ylab = "f(x)" )

#Polynomial Functions
f15<- function(x){
  hm<- 4*x^3-3*x^2+2*x+2
  return(hm)
}

input<- seq(-20, 20, 0.1)
plot(input, sapply(input, f15), type = "l", xlab = "x", ylab = "f(x)" )

#Rational Function
f16<- function(x){
  hmm<- 2/x
  return(hmm)
}

input<- seq(1, 20, 0.1)
plot(input, sapply(input, f16), type = "l", xlab = "x", ylab = "f(x)" )
