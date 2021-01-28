# Excersie 1

head(PlantGrowth)
summary(PlantGrowth)
str(PlantGrowth)
attach(PlantGrowth)
by(weight, group, summary)
par(mfrow = c(2,2))
boxplot(weight ~ group, horizontal = T)
stripchart(weight ~ group, veritcal = T)
fit <- lm(weight ~ group)
plot(fit, which = c(1, 2))

rand.oneway = function(response, group, R=199) {
  test = oneway.test(response ~ group)
  observed.F <- test$statistic
  stats = replicate(R, {
    random.labels = sample(group)
    oneway.test(response ~ random.labels)$statistic})
  p = sum(stats >= observed.F) / (R+1)
  test$method = "Randomization test for equal means"
  test$p.value = p
  return(test)
}
rand.oneway(weight, group, 1000)
par(mfrow = c(1, 1))

# Exercise 2
#no data

# Exercise 3
waste = read.table(
  file="wasterunup.txt",
  header=TRUE, na.strings="*")

set.seed(32)
test <- oneway.test(weight ~ group)
observed.F <- test$statistic
observed.F
random.labels <- sample(group)
do <- replicate(R, oneway.test(weight ~ random.labels)$statistic)
R = 1000
stat <- replicate(R, do)
head(stat)
p = sum(stat >= observed.F) / (R + 1)
p


# Exercise 4

head(airquality)
airquality <- airquality[, c(1, 5)]
library(lawstat)
levene.test(airquality$Ozone, airquality$Month)
# Welch metod 해라


# Exercise 5
web = read.table("webhits.txt", header=TRUE)
str(web)
web$month <- as.factor(c(rep(1, 9), rep(2,13), rep(3, 13)))
str(web)
rand.oneway = function(response, group, R=199) {
  test = oneway.test(response ~ group)
  observed.F <- test$statistic
  stats = replicate(R, {
    random.labels = sample(group)
    oneway.test(response ~ random.labels)$statistic})
  p = sum(stats >= observed.F) / (R+1)
  test$method = "Randomization test for equal means"
  test$p.value = p
  return(test)
}

rand.oneway(web$Hits, web$month)


# Exercise 6
rm(list = ls())

web = read.table("webhits.txt", header=TRUE)
rand.correlation = function(x, y, R=199) {
  ranks.x = rank(x)
  ranks.y = rank(y)
  observed.r = cor(ranks.x, ranks.y)
  stats = replicate(R, {
    random.ranks = sample(ranks.y)
    cor(ranks.x, random.ranks)
  })
  p.value = sum(stats >= observed.r) / (R + 1)
  hist(stats,freq=FALSE)
  list(observed.r = observed.r, p.value = p.value)
}
rand.correlation(web$Week, web$Hits)

#Exercise 8
rand.correlation = function(x, y, R=199) {
  observed.r = cor(x, y)
  stats = replicate(R, {
    random.y = sample(y)
    cor(x, random.y)
  })
  p.value = sum(stats >= observed.r) / (R + 1)
  list(observed.r = observed.r, p.value = p.value)
}
n = 50
rx <- runif(n)
ry <- runif(n)
rand.correlation(rx, ry)
