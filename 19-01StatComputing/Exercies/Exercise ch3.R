# 3.1 Fast food eating preference
fast <- scan(what = "character")
Wendys Mcdonalds Subway Subway Subway Wendys
Wendys Subway Wendys Subway Subway Subway
Subway Subway Subway
table(fast)
freq <- table(fast) / length(fast)
freq

plot(freq)
barplot(freq)

#3.2 Dice roll
die1 <- sample(6, 1000, replace = T)
die2 <- sample(6, 1000, replace = T)
die1.sum <- sum(die1)
die2.sum <- sum(die2)
tab.1 <- table(die1)
tab.2 <- table(die2)
t1 <- table(die1) / length(die1)
t2 <- table(die2) / length(die2)
t1
t2
die3 <- die1 + die2
table(die3)
table.sum <- tab.1 + tab.2
table.sum
t.s <- table.sum / 2000
t.s

#3.3 Does baseball hitting data follow a binomial distribution
k <- 0 : 4
p <- dbinom(k, 4, 0.312)
binom <- round(70 * p)
binom
names(binom) = k
binom
pujols <- c(17, 31, 17, 5)
pujols
names(pujols) <- k
prob <- c(p[1:3], 1 - sum(p[1:3]))
prob
data.frame(binom, pujols, Diff = pujols - binom)
chisq.test(pujols, prob)

k <- 0 : 5
p <- dbinom(k, 5, 0.312)
binom <- round(25 * p)
binom
names(binom) = k
binom
pujols <- c(5, 5, 4, 11)
names(pujols) <- k
pujols
prob <- c(p[1:3], 1 - sum(p[1:3]))
prob
data.frame(binom, pujols, Diff = pujols - binom)
chisq.test(pujols, prob)

#3.4 categorizing ages in the twin dataset
twn <- read.table("twins.dat.txt", header = T,
                  sep = ",", na.strings = ".")
table(twn$AGE)
max(twn$AGE)
c.age <- cut(twn$AGE, breaks = c(0, 30, 40, 50, 80))
table(c.age)
barplot(prop.table(table(c.age)))

#3.5 relating age and wage in the twin dataset
c.age <- cut(twn$AGE, breaks = c(0, 30, 40, 50, 80))
c.hrw <- cut(twn$HRWAGEL, breaks = c(0, 7, 13, 20, 150))
t1 <- table(c.age, c.hrw)
t1
diag(t1)
sum(diag(t1) / sum(t1))
plot(t1)
plot(prop.table(table(c.age, c.hrw)))

p <- prop.table(t1, 1)
p
barplot(t(p), ylab = "Proportion",
        legend.text = dimnames(p)$c.hrw)

#3.6 continued
t2 <- table(c.age, c.hrw)
t2
chi <- chisq.test(t2)
chi
print(chi)

r2 <- chi$residuals
chi$residuals
which(abs(chi$residuals) > 2)

mosaicplot(t2, shade = T)

#3.7 dice rolls continue
die1 <- sample(6, 1000, replace = T)
die2 <- sample(6, 1000, replace = T)
max.rolls <- pmax(die1, die2)
sum.rolls <- die1 + die2
table(max.rolls, sum.rolls)
plot(table(max.rolls, sum.rolls))

#3.8 Are the digits of pi random?
pidigits =
  read.table("http://www.itl.nist.gov/div898/strd/univ/data/PiDigits.dat",
             skip=60)
table(pidigits)
p <- table(pidigits)
barplot(prop.table(table(pidigits)))
barplot(table(pidigits))
chisq.test(p)
