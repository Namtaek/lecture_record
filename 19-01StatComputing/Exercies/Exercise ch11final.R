# 11.2

#a 
scramble.hats = function() {
  hats = rep(c(1, 2), each = 10)
  mixed.hats <- sample(hats)
  correct <- sum(hats == mixed.hats)
  return(correct)
}
scramble.hats()

#b
matches <- replicate(1000, scramble.hats())

#c
sum(matches >= 10) / length(matches) #approximate probability
mean(matches) #expected number of correct matches


# 11.3 birthday

# a
birth <- function(n) {
  samp.birth = sample(1:365, size = n, replace = T)
  length(unique(samp.birth))
}
birth(30)

# b

r <- replicate(1000, birth(30))

# c

mean(replicate(10000, birth(30) < 30))
mean(replicate(10000, birth(40) < 40))

1-sum(rep_birth==30)/10000 #이거는 뭐징

# d e

days <- 365:336
1- prod(days)/365^30
pbirthday(30, 365) # absolutely same

pbirthday(30)
pbirthday(40)

a <- rep(0, 365)
b <- for (i in 1:365) {
  appro = mean(replicate(1000, birth(i) < i))
  real = pbirthday(i)
  bias = (appro - real) < 0.01
  a[i] <- bias
}
mean(a)


# 11.4 streakness

#a
y <- c(0, 1, 0, 0, 0, 1, 0, 0, 1)
sum(abs(diff(y)))

switches <- function(vector) {
  sum(abs(diff(vector)))
}
dat = read.table("utley2006.txt", header=TRUE, sep="\t")
utley = as.numeric(dat$H > 0)
switches(utley)

#b
bi <- rbinom(160, 1, 1/2)


random.streak=function(y){
  mixed.up.y = sample(y)
  switches(mixed.up.y)
}
random.streak(utley)
random.streak(bi)

#c
switch.streak <- replicate(10000, random.streak(bi))
hist(switch.streak)
mean(switch.streak)





# 11.5 state quaters


# a 

unique.state <- function(n, m) {
  samp.state = sample(1:n, size = m, replace = T)
  length(unique(samp.state))
}
unique.state(50, 100)

#b

simul.state <- replicate(1000, unique.state(50, 100))
table(simul.state)
mean(simul.state >= 45)

  
#c

mean(simul.state)

#d

state2 <- function(k) {
  n = 0
  n.quaters = 0
  quaters = NULL
  repeat {
    n = n + 1
    samp.state = sample(1:50, 1, replace = T)
    quaters = c(quaters, samp.state)
    n.quaters = length(unique(quaters))
    if(n.quaters == 50) break
  }
  k * n
}

expected.cost = function(n.purchased) {
  mean(replicate(100, state2(n.purchased)))
}
expected.cost(2)





# d를 다르게~
all.quarters=function(){
  a=c();n=0
  repeat{
    a=c(a,sample(1:50,1))
    n=n+1
    if (length(unique(a))==50){
      return(n*2);break
    }
  }
}
all.quarters()
mean(replicate(1000, all.quarters()))


# 11. 6 student exclude

#a

mosteller <- function() {
  students = 1:10
  m = matrix(0, nrow=10, ncol=10)
  for(j in 1: 10){
  s = sample(students[-j], size=2)
  m[j, s] = c(1, 1)
  }
  k <- apply(m, 2, sum)
  sum(k == 0)
}
mosteller()


#b
simul.mos <- replicate(100, mosteller()) # array로 나옴

#c
table(simul.mos)

#d
mean(simul.mos)
# approximate probability == 0.40








