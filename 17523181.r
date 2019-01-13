#no1 
nomer1 <- read.csv(file.choose(), header=TRUE)
View(nomer1)

model <- lm(xi ~ yi, data = nomer1)
summary(model)

#no2 c
predict(model, data.frame(yi = 55))

#no3
nomer3 <- read.csv(file.choose(), header=TRUE)
View(nomer3)
#no4

#no5

#no6 c
#no7 d

#no8 b

#no9 a

#no10 c

#no11
f3 <- function(x){
  return(x^2-6)
}
composite.trapezoid <- function(f, a, b, n) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- (b - a) / n
  
  j <- 1:n - 1
  
  xj <- a + j * h
  
  approx <- (h / 2) * (f(a) + 2 * sum(f(xj)) + f(b))
  
  return(approx)
}
composite.trapezoid(f3, 0, 1, 4)

#no13
h <- 0.1
x <- seq(0,1, by=h)
f <- function(x){
  return(x^2)
}
f0 < f(x[1])
fi <- sapply(x[2:10], f)
fn <- f(x[length(x)])
trap <- function(f0, fi, fn, h){
  L <- h/2*(f0+2*sum(fi)-1+fn)
  return(L)
}
trap(f0, fi, fn,h)