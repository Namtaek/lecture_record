#1.1 Normal percentiles
qnorm(0.95, 0, 1)
qnorm(c(0.75, 0.50, 0.25))

#1.2 Chisquare density curve
curve(dchisq(x, 1))

#1.3 Gamma density
curve(dgamma(x, 1, 1))
curve(dgamma(x, shape = 2, 1), add = T)
curve(dgamma(x, shape = 3, 1), add = T)
?dgamma

#1.4 binomial property
#choose(n, k) combination임
dbinom(c(0:12), 12, 1/3)
prob <- function(x){
  x = 0:12
  p = 1/3
  com = choose(12, x)
  pro = p^x
  oth.pro = (1 - p)^(12 - x)
  return(com * pro * (oth.pro))
}
prob(0:12)
dbinom(c(0:12), 12, 1/3)

#1.5 binomial cdf
?cumsum
cum <- function(x){
  x = 0:12
  p = 1/3
  com = choose(12, x)
  pro = p^x
  oth.pro = (1 - p)^(12 - x)
  prob = com * pro * (oth.pro)
  return(cumsum(prob(x)))
}
cum(x)[7]

pbinom(6, 12, 1/3)
1- pbinom(6, 12, 1/3)
pbinom(6, 12, 1/3, lower.tail = F)
       
#1.6 President height
winner = c(185, 182, 182, 188, 188, 188, 185, 185, 177,
           182, 182, 193, 183, 179, 179, 175)
opponent = c(175, 193, 185, 187, 188, 173, 180, 177, 183,
             185, 180, 180, 182, 178, 178, 173)
plot(winner, opponent)

#1.7 simulated horsekick data
rpois(1000, 0.61)
table(rpois(1000, 0.61))
x <- c(0, 1, 2, 3, 4)
n <- c(568, 311, 107, 10 ,4)
p <- n / sum(n)
mu <- sum(x * p)
vari <- sum(n *(x - mu)^2)/999
vari
func <- exp(-mu) * mu^x / factorial(x)
func
dpois(x, mu)

#1.8 horsekick continued
ppois(4, 0.61)
p <- dpois(0:4, 0.61)
p
cdf <- ppois(0:4, 0.61)
cdf
emp.cdf <- cumsum(func)[1:5]
emp.cdf
cbind(p ,cdf, emp.cdf)

#1.9 custom standard deviation function
temps = c(51.9, 51.8, 51.9, 53)
NROW(temps)
var.n = function(x){
  v = var(x)
  n = NROW(x)
  v * (n - 1) / n
}
var(temps)
var.n(temps)
sd(temps)

sd.n = function(x){
  v = var(x)
  sqrt(v)
}
sd.n(temps)

#1.10 Euclidean norm function
dis = function(x){
  sig <- t(x) * x
  sq <- sqrt(sum(sig))
  return(sq)
}

di = function(x, y){
  k <- x - y
  sig <- k %*% k
  sq <- sqrt(sig)
  return(sq)
}    
di(x,y)


#1.11 numerical integration
f = function(x){
  exp(-x^2) / (1 + x^2)
}
integrate(f, lower = 0, upper = Inf)

#1.12 bivariate normal
x <- matrix(rnorm(20), 10, 2)
x
a <- x[1:10]
b <- x[11:20]
apply(x, 1, dis) 

#1.13 lunatics data

#1.14 Tearing factor of paper
pressure
press <- c(rep(35.0, 4), rep(49.5, 4), rep(70.0, 4), rep(99.0, 4), rep(140.0, 4))
Tear <- c(112, 119, 117, 113, 108, 99, 112, 118, 120, 106, 102, 109,
          110, 101, 99, 104, 100, 102, 96, 101)
paper <- data.frame(Tear, press)

#1.15 vectorized operation
temps = c(51.9, 51.8, 51.9, 53)
CT = c(48, 48.2, 48, 48.7)
#length가 다르면 어케되는지 알자나