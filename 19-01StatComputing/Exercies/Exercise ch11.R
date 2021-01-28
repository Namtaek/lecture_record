p.peter.win = function(peter=60){
  repeat
  {
    peter = peter + sample(c(-1, 1), 1, replace = T, prob = c( 0.51, 0.49))
    if (peter == 100){
      return("Win")
      break}
    if (peter == 0){
      return("Lose")
      break}
  }
}
p.peter.win()
n.win = replicate(1000, p.peter.win())
a = prop.table(table(n.win))
a["Win"]


# Exercise 1
peter.paul()
# 1.1
w <- function(n = 20) {
  red <- sample(c(5, -5), size=20, replace=TRUE, prob=c(18 / 38, 20 / 38))
  sum(red)
}
w()
n.red <- replicate(100, w())
table(n.red)
sum(n.red > 0) / length(n.red)

# 1.2
sum(dbinom( 11:20, 20, 18 / 38, 20 / 38))

# 1.3
w <- function(n = 20) {
  winning <- sample(c(5, -5), size=20, replace=TRUE, prob=c(18 / 38, 20 / 38))
  cum.win <- cumsum(winning)
  return(sum(cumsum(winning) > 0))
}

r.w <- replicate(500, w())
r.w
table(r.w)
plot(table(r.w))
plot(prop.table(table(r.w)))
