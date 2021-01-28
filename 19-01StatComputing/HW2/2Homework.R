

#Streaky hitting in baseball (11.5절)의 내용을 수정하여 한게임에 출전하여 1번 이상
#안타를 칠 확률이 h인 야구선수가 k게임이 출장하였을 때
#[1] hitting streak의 분포와
#[2] 최장 hitting streak이 s 보다 클 확률
#을 시뮬레이션을 이용하여 추정하는 프로그램을 작성하고 결과(여러 가지의 h, k, s 값에
#대한)와 함께 제출하시오.


streak = function(n, k, h){
  result = rep(0, k)
  for (i in 1:n){
    y = rbinom(k, 1, h)
    where = c(0, y, 0) == 0
    n = length(y)
    loc.zeros = (0:(n + 1))[where]
    streak.lengths = diff(loc.zeros) - 1
    streak.lengths = streak.lengths[streak.lengths > 0]
    for(t in streak.lengths){
      result[t] = result[t]+1}
  }
  result
}


y = streak(10000, 100, 0.6) 
plot(which(y != 0), y[y != 0])



longest.streak <- function(x) {
  where = c(0, x, 0) == 0
  n = length(x)
  loc.zeros = (0 : (n + 1))[where]
  streak.lengths = diff(loc.zeros) - 1
  streak.lengths = streak.lengths[streak.lengths > 0]
  max(streak.lengths)
}
random.streak=function(k, h){
  y = rbinom(k, 1, h)
  mixed.up.y = sample(y)
  longest.streak(mixed.up.y)
}
L <- replicate(100000, random.streak(k, h))
plot(table(L))

prob.bigger.s <- function(k, h, s) {
  replicate(10000, random.streak(k, h) > s)
}
fors <- prob.bigger.s(k, h, s)

L <- replicate(100000, random.streak(500, 0.5)) # k = 500, h = 0.5
plot(table(L))
fors <- prob.bigger.s(500, 0.5, 10)  # s = 10
table(fors) 
mean(fors)

L <- replicate(100000, random.streak(500, 0.3)) # k = 500, h = 0.3
plot(table(L))
fors <- prob.bigger.s(500, 0.3, 5) # s = 5
table(fors) 
mean(fors)

L <- replicate(100000, random.streak(500, 0.7)) # k = 500, h = 0.7
plot(table(L))
fors <- prob.bigger.s(500, 0.7, 20) # s = 20
table(fors) 
mean(fors)

L <- replicate(100000, random.streak(500, 0.8)) # k = 500, h = 0.8
plot(table(L))
fors <- prob.bigger.s(500, 0.8, 25) # s = 25
table(fors) 
mean(fors)

L <- replicate(100000, random.streak(500, 0.8)) # k = 500, h = 0.8
plot(table(L))
fors <- prob.bigger.s(500, 0.8, 30) # s = 30
table(fors) 
mean(fors)

L <- replicate(100000, random.streak(500, 0.8)) # k = 500, h = 0.8
plot(table(L))
fors <- prob.bigger.s(500, 0.8, 38) # s = 38
table(fors) 
mean(fors)

# 13.1 (Late to class?). Suppose the travel times for a particular student from
# home to school are normally distributed with mean 20 minutes and standard
# deviation 4 minutes. Each day during a five-day school week she leaves home
# 30 minutes before class. For each of the following problems, write a short
# Monte Carlo simulation function to compute the probability or expectation
# of interest.

# a. Find the expected total traveling time of the student to school for a fiveday
# week. Find the simulation estimate and give the standard error for the
# simulation estimate.

expected <- function(n) {
  travel.time = rnorm(n, 20, 4)
  mean.time <- mean(travel.time)
  sum.time <- sum(travel.time)
  sehat <- sum((travel.time - mean.time)^2) / (n - 1)
  sum.time
}
expected(5)

set.seed(3)
simul.expected.all <- replicate(10000, expected(5))
head(simul.expected.all)
simul.expected.total.mean <- mean(simul.expected.all)
simul.expected.total.mean

total.sehat <- sqrt(sum((simul.expected.all - simul.expected.total.mean)^2) / 10000)
total.sehat

# b. Find the probability that the student is late for at least one class in the
# five-day week. Find the simulation estimate of the probability and the
# corresponding standard error.

set.seed(4)
prob.thirty <- function(n) {
  travel.time = rnorm(n, 20, 4)
  prob <- sum(travel.time > 30) / n
  prob
}
prob.thirty(5)

simul.late.cases <- replicate(100000, prob.thirty(5))
simul.late.prob.mean <- mean(late.cases)
simul.late.prob.mean
late.sehat <- sqrt(sum((simul.late.cases - simul.late.prob.mean)^2) / 100000)
late.sehat

# c. On average, what will be the longest travel time to school during the fiveday
# week? Again find the simulation estimate and the standard error.

longest.time <- function(n) {
  travel.time = rnorm(n, 20, 4)
  max(travel.time)
}
simul.longest.cases <- replicate(100000, longest.time(5))
head(simul.longest.cases)
simul.longest.mean <- mean(simul.longest.cases)
simul.longest.mean

longests.sehat <- sqrt(sum((simul.longest.cases - simul.longest.mean)^2) / 100000)
longests.sehat

# 13.2 (Confidence interval for a normal mean based on sample quantiles).
# Suppose one obtains a normally distributed sample of size n=20 but
# only records values of the sample median M and the first and third quartiles
# Q1 and Q3.

# a. Using a sample of size n=20 from the standard normal distribution, simulate
# the sampling distribution of the statistic
# S = M / (Q3 - Q1)
# Store the simulated values of S in a vector.

x <- rnorm(20, 0, 1)
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
sam.med <- median(x)
S <- sam.med / (Q3 - Q1)
S

dis.stat <- function(n) {
  x <- rnorm(n, 0, 1)
  Q1 <- quantile(x, 0.25, names = F)
  Q3 <- quantile(x, 0.75, names = F)
  sam.med <- median(x)
  S <- sam.med / (Q3 - Q1)
  S
}
dis.stat(20)
simul.S <- replicate(10000, dis.stat(20))
head(simul.S)

# b. Find two values, s1,s2, that bracket the middle 90% probability of the
# distribution of S.

quantile(simul.S, c(0.05, 0.95))

# c. For a sample of size n = 20 from a normal distribution with mean μ and
# standard deviation σ, it can be shown that
# P(s1 < (M - mu) / (Q3 - Q1) < s2) = 0.90
# Using this result, construct a 90% confidence interval for the mean μ

s1 <- -0.3719357
s2 <- 0.3962538 
CI.for90 <- c(-s2 * (Q3 - Q1) + sam.med, -s1 * (Q3 - Q1) + sam.med)

# d. In a sample of 20, we observe (Q1,M,Q3) = (37.8,51.3,58.2). Using your
# work in parts (b) and (c), find a 90% confidence interval for the mean μ.


s1 <- -0.3719357
s2 <- 0.3962538
Q1 <- 37.8
sam.med <- 51.3
Q3 <- 58.2

CI.for90.b <- c(-s2 * (Q3 - Q1) + sam.med, -s1 * (Q3 - Q1) + sam.med)
CI.for90.b



# 13.3 (Comparing variance estimators). Suppose one is taking a sample
# y1, ...,yn from a normal distribution with mean μ and variance σ2.

# a. It is well known that the sample variance
# S = sigma (yi - ybar)^2 / (n - 1)
# is an unbiased estimator of σ2. To confirm this, assume n=5 and perform
# a simulation experiment to compute the bias of the sample variance S.

samp.var.bias <- function(n, mu, s) {
  y <- rnorm(n, mu, s)
  S <- sum((y - mean(y))^2) / (n - 1)
  s^2 - S
}
simul.bias <- replicate(100000, samp.var.bias(5, 0, 5))
mean(simul.bias)

simul.bias <- replicate(100000, samp.var.bias(5, 10, 10))
mean(simul.bias)

# b. Consider the alternative variance estimator
# Sc = sigma (yi - ybar)^2 / c
# where c is a constant. Suppose one is interested in finding the estimator
# Sc that makes the mean squared error
# MSE = E[(Sc - σ2)^2]
# as small as possible. Again assume n=5 and use a simulation experiment
# to compute the mean squared error of the estimators S3,S5,S7,S9 and
# find the choice of c (among {3, 5, 7, 9}) that minimizes the MSE.

samp.var.bias <- function(n, mu, s, c) {
  y <- rnorm(n, mu, s)
  Sc <- sum((y - mean(y))^2) / c
  mean((Sc - s^2)^2)
}
S3 <- mean(replicate(10000, samp.var.bias(5, 0, 5, 3)))
S5 <- mean(replicate(10000, samp.var.bias(5, 0, 5, 5)))
S7 <- mean(replicate(10000, samp.var.bias(5, 0, 5, 7)))
S9 <- mean(replicate(10000, samp.var.bias(5, 0, 5, 9)))
S.vec <- c(S3 = S3, S5 = S5, S7 = S7, S9 = S9)
S.vec
S.vec[which.min(S.vec)]

# 13.5 (Metropolis-Hastings algorithm for the poly-Cauchy distribution).
# Suppose that a random variable y is distributed according to the
# poly-Cauchy density
# g(y) = prod [1 / {π * (1+(y−ai)^2)}]
# where a=(a1, ...,an) is a vector of real-valued parameters. Suppose that n=6
# and a = (1,2,2,6,7,8).

# a. Write a function to compute the log density of y. (It may be helpful to use
# the function dcauchy that computes the Cauchy density.)

log.g <- function(y, a) {
  a <- c(1, 2, 2, 6, 7, 8)
  log(prod(dcauchy(y, a, 1)))
}


polcauchy = function(y){
  a=c(1,2,2,6,7,8)
  result = NULL
  for(x in y){
    dc = prod(dcauchy(x, location = a))
    result = c(result, dc)}
  result
}
curve(polcauchy(x),from=0,to=10)

# b. Use the function metrop.hasting.rw to take a simulated sample of size
# 10,000 from the density of y. Experiment with different choices of the
# standard deviation C. Investigate the effect of the choice of C on the
# acceptance rate, and the mixing of the chain over the probability density.

metrop.hasting.rw = function(logf, current, C, iter, ...){
  S = rep(0, iter); n.accept = 0
  for(j in 1:iter){
    candidate = rnorm(1, mean=current, sd=C)
    prob = exp(logf(candidate, ...) - logf(current, ...))
    accept = ifelse(runif(1) < prob, "yes", "no")
    current = ifelse(accept == "yes", candidate, current)
    S[j] = current; n.accept = n.accept + (accept == "yes")
  }
  list(S=S, accept.rate=n.accept / iter)
}

mcmc.sample1 = metrop.hasting.rw(log.g, 1, 1, 10000)
mcmc.sample1$accept.rate
mcmc.sample2 = metrop.hasting.rw(log.g, 1, 0.1, 10000)
mcmc.sample2$accept.rate
mcmc.sample3 = metrop.hasting.rw(log.g, 1, 3, 10000)
mcmc.sample3$accept.rate
mcmc.sample4 = metrop.hasting.rw(log.g, 1, 5, 10000)
mcmc.sample4$accept.rate

# c. Using the simulated sample from a “good” choice of C, approximate the
# probability P(6<Y <8).

mcmc.sample4 = metrop.hasting.rw(log.g, 1, 5, 10000)
mcmc.sample4$accept.rate
plot(density(mcmc.sample4$S), lwd = 2, main = "", xlab = "M")

mean(mcmc.sample4$S > 6 & mcmc.sample4$S < 8)


# 13.6 (Gibbs sampling for a Poisson/gamma model). Suppose the vector
# of random variables (X,Y ) has the joint density function
# and we wish to simulate from this joint density.

# a. Show that the conditional density f(x|y) has a gamma density and identify
# the shape and rate parameters of this density.
# b. Show that the conditional density f(y|x) has a Poisson density.

# a와 b는 수식으로 증명했다

# c. Write a R function to implement Gibbs sampling when the constants are
# given by a = 1 and b = 1.
# d. Using your R function, run 1000 cycles of the Gibbs sampler and from
# the output, display (say, by a histogram) the marginal probability mass
# function of Y and compute E(Y ).

random.coin.gibbs = function(y, m){
  S = matrix(0, m, 2)
  dimnames(S)[[2]] = c("y", "x")
  for(j in 1:m){
    x = rgamma(1, y + 1, 2)
    y = rpois(1, x)
    S[j, ] = c(y, x)
  }
  return(S)
}

# d. Using your R function, run 1000 cycles of the Gibbs sampler and from
# the output, display (say, by a histogram) the marginal probability mass
# function of Y and compute E(Y ).

sim.values1 = random.coin.gibbs(y = 1, m = 1000)[101:1000, ]


sim.values1 = random.coin.gibbs(y = 1, m = 1000)
plot(sim.values1[ ,"y"])
hist(sim.values1[ ,"y"])
table(sim.values1[ ,"y"])
mean(sim.values1[ , "y"])

sim.values2 = random.coin.gibbs(y = 30, m = 1000)
plot(sim.values2[ ,"y"])
hist(sim.values2[ ,"y"])
table(sim.values2[ ,"y"])
mean(sim.values2[ , "y"])









