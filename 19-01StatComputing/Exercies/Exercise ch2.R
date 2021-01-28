#2.1 chickwts data
?chickwts
head(chickwts)
boxplot(weight ~ feed, chickwts)

#2.2 iris data
head(iris)
by(iris[, -5], iris$Species, FUN = function(x) {colMeans(x, na.rm = T)})
by(data = iris[, -5], iris$Species, colMeans, na.rm = T)

#2.3 mtcars data
pairs(mtcars)
boxplot(mtcars[, 3:4])   

#2.4 mammals data 몰라용
library(MASS)
head(mammals)
m <- mammals
m$ratio <- mammals$brain / mammals$body
m
?order
sort(m$ratio, decreasing = T)
attach(mammals)
r=brain/body
a=c(which.max(r),which.min(r))
cbind(mammals[a,],r=c("largest ratio","smallest ratio"))

mammals$ratio=brain/body
m=mammals[order(ratio),]
head(m,1);tail(m,1)
detach(mammals)

plot(log(m$ratio), log(m$body))

#2.6 lakehuron data
LakeHuron
plot(LakeHuron)
abline(h = mean(LakeHuron))
lines(lowess(LakeHuron))
diff(LakeHuron)
d <- diff(LakeHuron)
plot(d)
lines(lowess(d))
abline(h = 0, lty = 3)

#2.7 CLT with simulated data
randu
summary(randu)
colMeans(randu)
x <- runif(400)
y <- runif(400)
z <- runif(400)
mat <- cbind(x, y, z)
colMeans(mat)

#2.8 걍넘겨
#2.9 1970 vietnam draft lottery
#january mixed 12times, but december mixed just one time and so on. 
#so it is hard to say there is randomness

#2.10 old faithful histogram
hist(faithful$waiting, prob = T)
hist(faithful$waiting, freq = F)

#2.11 faithful continue
hist(faithful$waiting, prob = T)
lines(density(faithful$waiting))

#2.12 mammals data by brain size

#2.13 mammals data on original scale
plot(mammals$body, mammals$brain)
attach(mammals)
y = mammals[c("Cat", "Cow", "Human"), ]
points(y, cex = 5)
?points
#hard to interpret

#2.14 mammals cluster
d <- dist(log(mammals))
h <- hclust(d, method = "ward.D")
?hclust
plot(h)
h1 <- hclust(d, method = "complete")
plot(h1)

#2.15 identifying groups or cluster
h <- hclust(d, method = "single")
plot(h)
g <- cutree(h,5)
table(cutree(h,5))
table(g)       
g>2
mammals[g > 2, ]
