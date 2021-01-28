# 4

wald = function(y, n, prob) {
  p = y / n
  z = qnorm(1 - (1 - prob) / 2)
  lb = p - z * sqrt(p * (1 - p) / n)
  ub = p + z * sqrt(p * (1 - p) / n)
  cbind(lb, ub)
}
wald(5, 20, 0.95)
y <- c(2, 4, 6, 8)
wald(y, 20, 0.90)

mc.coverage = function(p, n, prob, iter = 10000) {
  y = rbinom(iter, n, p)
  c.interval = wald(y, n, prob)
  mean((c.interval[ ,1] < p) & (p < c.interval[, 2]))
}
mc.coverage(0.15, 20, 0.9)
sqrt(0.8027 * (1 - 0.8027) / 10000)

many.mc.coverage = function(p.vector, n, prob) {
  sapply(p.vector, mc.coverage, n, prob)
}
curve(many.mc.coverage(x, 100, 0.90), from = 0.001, to = 0.999,
      xlab = "p", ylab = "Coverage Probability",
      main = paste("n=", 100, ", prob=", 0.90),
      ylim = c(0.7, 1))  
abline(h = 0.9)

# 여기부터
plus_four <- function(y, n, prob) {
  new.n = n + 4
  new.p = (y + 2) / new.n
  z = qnorm(1 - (1 - prob) / 2)
  lb = new.p - z * sqrt(new.p * (1 - new.p) / new.n)
  ub = new.p + z * sqrt(new.p * (1 - new.p) / new.n)
  cbind(lb, ub)
}

plus_four(5, 20, 0.95)
y <- c(2, 4, 6, 8)
plus_four(y, 20, 0.90)

mc.coverage2 <- function(p, n, prob, iter = 10000) {
  y = rbinom(iter, n, p)
  c.interval = plus_four(y, n, prob)
  mean((c.interval[ ,1] < p) & (c.interval[ ,2] > p))
}
mc.coverage2(0.15, 20, 0.90)
sqrt(0.9315 * (1 - 0.9315) / 10000)

many.mc.coverage2 <- function(p.vector, n, prob) {
  sapply(p.vector, mc.coverage2, n, prob)
}

curve(many.mc.coverage2(x, 100, 0.90), from = 0.001, to = 0.999,
      xlab = "p", ylab = "Coverage Probability",
      main = paste("n=", 100, ", prob=", 0.90),
      ylim = c(0.7, 1))
abline(h = 0.9)
