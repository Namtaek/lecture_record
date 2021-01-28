library(tidyverse)
library(quantreg)
library(glmnet)
library(mvtnorm)


set.seed(2019)
n = 60; p = 30
x = matrix(rnorm(n * p), ncol = p)
x = cbind(rep(1,n), x)

tau = 0.5
sd = 1
beta = matrix(c(rep(0.5, 7), rep(0, p - 6)))
obs_err = rnorm(n, sd = sd)
x[,2] = runif(n)
y = x %*% beta + matrix(tau * x[,2] * obs_err) + obs_err
beta_true = beta
beta_true[2] = beta[2] + tau * qnorm(tau)
beta_true[1] = beta[1] + qnorm(tau, sd = sd)


# Q1. Create a solution path of Lasso QR with tau=0.5 
# using lambda = seq(0.1, 3, 0.1) for the first seven components.
# 람다가 변함에 따라 어떻게 되는지 그림을 그려라

lambda = seq(0.1, 3, 0.1)

simul_beta <- matrix(0, nrow = length(lambda), ncol = length(beta))
for (i in seq_along(lambda)) {
  result <- rq(y ~ x[,-1], tau = tau, method = 'lasso', lambda = lambda[i])
  simul_beta[i, ] <- result$coefficients
}




plot(c(0, 3), c(-1.3, 1.3), type = 'n', main = 'solution path of Lasso QR for all beta',
     xlab = 'lambda', ylab = 'beta_hat')
abline(h = 0, lty = 2)

for (i in 1:(length(beta)-1)) {
  lines(x = lambda, y = simul_beta[, i+1], col = (i+1), lwd = 2)
}

plot(c(0, 3), c(-1.3, 1.3), type = 'n', main = 'solution path of Lasso QR for first seven beta',
     xlab = 'lambda', ylab = 'beta_hat')
abline(h = 0, lty = 2)
for (i in 1:7) {
  lines(x = lambda, y = simul_beta[, i+1], col = (i+1), lwd = 2)
}


# Q2. Write your code to perform 10 fold cross-validation to determine penalty parameters 
# (regularization parameters) for Lasso and apply it to the simulation model. 
# Does the determined lambda gives a descent result compared to other regularization parameter values?
# cv선택된 람다가 다른거보다 좋냐? 에러추정 플롯을 비교해보자! 
# 우리가 시퀀스를 나눈 람다와, cv로 선택된 람다를 그래프로 비교해보면 명확하겠지?

set.seed(2019)

lambda = seq(0.1, 3, 0.1)
some_lasso <- glmnet(x, y, family = 'gaussian', alpha = 0, lambda = lambda) # lambda가 큰거부터 정렬

some_lasso_coef <- matrix(0, nrow = 31, ncol = 30)
for (i in seq_along(lambda)) {
  some_lasso_coef[1, i] <- some_lasso$a0[i]  # beta zero
  some_lasso_coef[-1, i] <- some_lasso$beta[-1, i]  # each slope
}   # 각 열에 30개의 람다에 대한 lasso fit의 베타계수를 저장해둠
head(some_lasso_coef) 


some_lasso_rss <- NULL
for (i in seq_along(lambda)) {
  some_lasso_rss[i] <- sum((y - x %*% some_lasso_coef[, i])^2)
}


some_lasso_rss


lambda = seq(0.1, 3, 0.1)
cv_lasso <- cv.glmnet(x, y, family = "gaussian", alpha = 0, lambda = lambda)
opt_lasso_lambda <- cv_lasso$lambda.min  # 최적의 람다는 0.2
opt_lasso <- glmnet(x, y, family = 'gaussian', alpha = 0, lambda = opt_lasso_lambda)

opt_lasso_coef <- rep(NA, length(beta))
opt_lasso_coef[1] <- opt_lasso$a0
opt_lasso_coef[-1] <- opt_lasso$beta[-1]

opt_lasso_rss <- sum((y - x %*% opt_lasso_coef)^2)
opt_lasso_rss # 31.9607

# lasso에서 lambda가 큰순서대로 정렬되어있기 때문에 rev로 뒤집어 줘야!
plot(x = rev(lambda), y = some_lasso_rss, pch = 19, type = 'b', 
     xlim = c(0, 3), ylim = c(0, 100))

points(x = opt_lasso_lambda, y = opt_lasso_rss, col = 'red', pch = 19, cex = 2)

# Q3. As done in linear regression case, 
# we can consider adaptive lasso estimator for quantile regression. 
# Write your code for this.

# 10 fold cv
set.seed(1)
n = 60
tau = 0.5
k = 10
lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1:n

each_error <- matrix(0, nrow = length(lambda), ncol = k)  # 에러를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = lambda[j], method = 'lasso')
    each_beta <- each_lasso_rq$coefficients
    each_predicted_y <- x[ind_for, ] %*% each_beta
    each_error[j, i] <- quantile(abs(valid_y - each_predicted_y), 0.5) # 에러값 기록
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}

each_lambda_error <- apply(each_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_lambda_error)  # cv를 통한 error의 형태가 이런식으로 나타남! 
min_lambda_error <- min(each_lambda_error)
min_lambda_index <- which(each_lambda_error == min_lambda_error)
min_lambda_index 

min_lambda_lasso <- lambda[min_lambda_index]  
min_lambda_lasso

# optimum lasso rq fit

opt_lasso_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = min_lambda_lasso, method = 'lasso')
beta_lasso_rq <- opt_lasso_rq_fit$coefficients
sum((beta_true[-1] - beta_lasso_rq[-1])^2)  # 1.43

beta_lasso_rq
# 원래 라쏘에서는 정확하게 0으로 찍어주지만, 현재 다른 베타들과 다르게 너무 작은 값들이 존재한다.
# 아마 quantile regression에서 계산하는 것의 한계인 것 같다. 
# 소수점 8자리 밑으로 나오는 값들은 모두 0으로 봐도 무방하지만, 
# adaptive 라쏘를 적용할때 좀더 편하게 페널티를 고려하기 위해, 이 상태로 놓아 두는 것이 편할 것이다.

# 그러면 이제 adaptive lasso를 위한 페널티가 될 베타틸데를 구했다. 
# lasso 베타의 역수가 베타 틸데가 될 것이다.
# 이제 다시 adpative lasso의 최적의 람다 값을 찾아야 하는데,
# 시드를 다르게 해서 람다값을 찾을 것이다.
# adpative lasso는 계층적인 알고리즘을 가지고 있기 때문에, cv도 계층적으로 짜야하겠지만,
# 우리 관측수가 많지 않으므로, seed를 다르게 하겠다. 
# 따라서 overfitting의 가능성을 배제할 수 없다는 점은 고려하자.

# 전체적인 과정은 이전과 거의 동일하다.

set.seed(2)  # seed를 다르게 설정
n = 60
tau = 0.5
k = 10
lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1:n

weight_beta <- beta_lasso_rq %>% abs  # 베타 절대값의 역수만큼 weight을 줘야하니 바꿔준다.
weight_beta <- weight_beta[2:length(weight_beta)]  # 베타0의 가중치는 0이 되어야하니 제외

each_ad_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    penalty <-c(0, lambda[j] / weight_beta)  # 베타 0에 대한 lambda_0를 0으로 만들어준다.
    # 각각 람다에 대해 lasso beta로 나눈 값을 고려하고 있다.
    each_adlasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = penalty, method = 'lasso')
    each_ad_beta <- each_adlasso_rq$coefficients
    each_ad_predicted_y <- x[ind_for, ] %*% each_ad_beta
    each_ad_error[j, i] <- quantile(abs(valid_y - each_ad_predicted_y), 0.5)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}

each_adlambda_error <- apply(each_ad_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold rss 기록
plot(each_adlambda_error)  # cv를 통한 error의 형태가 이런식으로 나타남! 약 300쯤에서 최소임을 확인가능
min_adlambda_error <- min(each_adlambda_error)
min_adlambda_index <- which(each_adlambda_error == min_adlambda_error)
min_adlambda_index  

min_adlambda_lasso <- lambda[min_adlambda_index]  #.
min_adlambda_lasso

# optimum adaptive lasso rq fit

opt_adlasso_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = min_adlambda_lasso, method = 'lasso')
beta_adlasso_rq <- opt_adlasso_rq_fit$coefficients

length(which(abs(beta_adlasso_rq) > 10^(-5))) # 1개만 0으로 수렴

# error for Adaptive Lasso Quantile Regression
sum((beta_true - beta_adlasso_rq)^2)  # 4.82

# 하지만 이는 다른 모델과 비교할 때의 한계가 있다. 
# 모델들이 모두 에러를 최소화한는 방향으로 만들어졌기 때문에, 
# test데이터를 통해 비교해야 하는데 test 데이터가 없다는점은 고려하자.



# Q4. We can also consider weighted lasso estimator 
# for quantile regression by using SCAD derivative function.
# Write your code for this, and compare this estimator with the adaptive lasso estimator. 
# For comparisons, you can choose lambda using cross-validation.


# 테일러 근사
scad_deriv = function(x,lam,gam=3.7){
  (abs(x) <= lam)*lam +
    (abs(x)>lam)*(abs(x)<gam*lam)*(gam*lam - abs(x))/(gam-1)
}

# 웨이트로 스캐드 미분함수를 넣는다. 베타는 3번에서 구한 lasso estimator를 넣는다.


set.seed(2)  
ind <- 1:n
ind_all <- 1:n
lambda = seq(0.01, 10, by = 0.01)

each_scad_error <- matrix(0, nrow = length(lambda), ncol = k)  # rss를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    weight = c(0, scad_deriv(abs(beta_lasso_rq[-1]), lam = lambda[j]))
    # 베타 0에 대한 lambda_0를 0으로 만들어준다.
    # 각각 람다에 대해 스캐드 미분함수를 고려한다.
    each_scad_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = weight, method = 'lasso')
    each_scad_beta <- each_scad_rq$coefficients
    each_scad_predicted_y <- x[ind_for, ] %*% each_scad_beta
    each_scad_error[j, i] <- quantile(abs(valid_y - each_scad_predicted_y), 0.5)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}


each_scad_error <- apply(each_scad_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_scad_error)  # cv를 통한 error의 형태가 이런식으로 나타남! 
min_scad_error <- min(each_scad_error)
min_scad_index <- which(each_scad_error == min_scad_error)
min_scad_index  

min_scad_lasso <- lambda[min_scad_index]  #
min_scad_lasso

# optimum weighted lasso rq fit with scad derivative funtion

opt_weight = c(0, scad_deriv(abs(beta_lasso_rq[-1]), lam = min_scad_lasso))
opt_scad_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = opt_weight, method = 'lasso')

beta_scad_rq <- opt_scad_rq_fit$coefficients
length(which(abs(beta_scad_rq) > 10^(-5)))
# 6개가 0으로 수렴


# error for weighted lasso rq fit with scad derivative funtion
sum((beta_true - beta_scad_rq)^2) # 1.59


# Q5. Consider the following quantile regresion model with tau = 0.25 and tau = 0.75, respectively. 
# Obatin estimation errors for the selected penalty parameters via 10 fold cross-validation for Lasso, 
# adaptive Lasso, and weighte Lasso (using SCAD derivative). 
# And compare them. 


# 0.25일때!
set.seed(2019)
n = 60; p = 30
x = matrix(rnorm(n * p), ncol = p)
x = cbind(rep(1,n), x)

tau = 0.25
sd = 1
beta = matrix(c(rep(0.5, 7), rep(0, p - 6)))
obs_err = rnorm(n, sd = sd)
x[,2] = runif(n)
y = x %*% beta + matrix(tau * x[,2] * obs_err) + obs_err
beta_true = beta
beta_true[2] = beta[2] + tau * qnorm(tau)
beta_true[1] = beta[1] + qnorm(tau, sd = sd)

# lasso에 대해서
set.seed(2)
k = 10
lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1:n

each_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = lambda[j], method = 'lasso')
    each_beta <- each_lasso_rq$coefficients
    each_predicted_y <- x[ind_for, ] %*% each_beta
    each_error[j, i] <- quantile(abs(valid_y - each_predicted_y), 0.25)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}

each_lambda_error <- apply(each_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_lambda_error)  # cv를 통한 error의 형태가 이런식으로 나타남! 
min_lambda_error <- min(each_lambda_error)
min_lambda_index <- which(each_lambda_error == min_lambda_error)
min_lambda_index  

min_lambda_lasso <- lambda[min_lambda_index]
min_lambda_lasso

# optimum lasso rq fit

opt_lasso_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = min_lambda_lasso, method = 'lasso')
beta_optlasso_rq <- opt_lasso_rq_fit$coefficients
length(which(abs(beta_optlasso_rq) > 10^(-5))) 

sum((beta_true - beta_optlasso_rq)^2)  #2.615134




## adaptive lasso rq와 scad rq에 쓰일 베타틸데(lasso qr)을 구하기 위해, lasso qr을 피팅한다. 
# 이때 시드는 1로 잡아서 하자.

set.seed(1)
lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1:n

each_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = lambda[j], method = 'lasso')
    each_beta <- each_lasso_rq$coefficients
    each_predicted_y <- x[ind_for, ] %*% each_beta
    each_error[j, i] <- quantile(abs(valid_y - each_predicted_y),0.25)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}

each_lambda_error <- apply(each_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_lambda_error)  # cv를 통한 error의 형태가 이런식으로 나타남! 
min_lambda_error <- min(each_lambda_error)
min_lambda_index <- which(each_lambda_error == min_lambda_error)
min_lambda_index  

min_lambda_lasso <- lambda[min_lambda_index] 
min_lambda_lasso

lasso_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = min_lambda_lasso, method = 'lasso')
beta_lasso_rq <- lasso_rq_fit$coefficients


# adaptive lasso qr

set.seed(2)  # seed를 다르게 설정
lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1:n

weight_beta <- beta_lasso_rq %>% abs  # 베타 절대값의 역수만큼 weight을 줘야하니 바꿔준다.
weight_beta <- weight_beta[2:length(weight_beta)]  # 베타0의 가중치는 0이 되어야하니 제외

each_ad_error <- matrix(0, nrow = length(lambda), ncol = k)  # error 를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    penalty <-c(0, lambda[j] / weight_beta)  # 베타 0에 대한 lambda_0를 0으로 만들어준다.
    # 각각 람다에 대해 lasso beta로 나눈 값을 고려하고 있다.
    each_adlasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = penalty, method = 'lasso')
    each_ad_beta <- each_adlasso_rq$coefficients
    each_ad_predicted_y <- x[ind_for, ] %*% each_ad_beta
    each_ad_error[j, i] <- quantile(abs(valid_y - each_ad_predicted_y),0.25)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}

each_adlambda_error <- apply(each_ad_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_adlambda_error)  # cv를 통한 error 의 형태가 이런식으로 나타남! 약 300쯤에서 최소임을 확인가능
min_adlambda_error <- min(each_adlambda_error)
min_adlambda_index <- which(each_adlambda_error == min_adlambda_error)
min_adlambda_index  
min_adlambda_lasso <- lambda[min_adlambda_index] 
min_adlambda_lasso

# optimum adaptive lasso rq fit

opt_adlasso_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = min_adlambda_lasso, method = 'lasso')
beta_adlasso_rq <- opt_adlasso_rq_fit$coefficients

length(which(abs(beta_adlasso_rq) > 10^(-5))) 


# error for Adaptive Lasso Quantile Regression
sum((beta_true - beta_adlasso_rq)^2) # 3.398


# SCAD tau = 0.25
set.seed(2)  
ind <- 1:n
ind_all <- 1:n
lambda = seq(0.01, 10, by = 0.01)

each_scad_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    weight = c(0, scad_deriv(abs(beta_lasso_rq[-1]), lam = lambda[j]))
    # 베타 0에 대한 lambda_0를 0으로 만들어준다.
    # 각각 람다에 대해 스캐드 미분함수를 고려한다.
    each_scad_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = weight, method = 'lasso')
    each_scad_beta <- each_scad_rq$coefficients
    each_scad_predicted_y <- x[ind_for, ] %*% each_scad_beta
    each_scad_error[j, i] <- quantile(abs(valid_y - each_scad_predicted_y), 0.25)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}


each_scad_error <- apply(each_scad_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_scad_error)  # cv를 통한 error의 형태가 이런식으로 나타남! 
min_scad_error <- min(each_scad_error)
min_scad_index <- which(each_scad_error == min_scad_error)
min_scad_index  

min_scad_lasso <- lambda[min_scad_index]  
min_scad_lasso

# optimum weighted lasso rq fit with scad derivative funtion

opt_weight = c(0, scad_deriv(abs(beta_lasso_rq[-1]), lam = min_scad_lasso))
opt_scad_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = opt_weight, method = 'lasso')

beta_scad_rq <- opt_scad_rq_fit$coefficients
length(which(abs(beta_scad_rq) > 10^(-5)))



# error for weighted lasso rq fit with scad derivative funtion
sum((beta_true - beta_scad_rq)^2) # 2.615




sum((beta_true - beta_optlasso_rq)^2) # 2.615134
sum((beta_true - beta_adlasso_rq)^2) # 3.421521
sum((beta_true - beta_scad_rq)^2) # 2.615134

# 이경우 라쏘와 스캐드는 거의 동일한 결과값을 반환한다. 이는 우리 코드의 문제가 아니다.
# 처음 주어진 베타값은 0.5로 상당히 작은 편이다. 그러나 우리에게 선택되는 람다는
# 0.23같이 작은 람다가 선택되기도 하지만, scad에서는 147가 최적의 람다로 선택된다.
# 결국 베타값이 머무는 범위보다 람다값이 훨씬 크기때문에, 이경우 라쏘와 스캐드는 동일한 결과를 산출한다.


## tau = 0.75로

set.seed(2019)
n = 60; p = 30
x = matrix(rnorm(n * p), ncol = p)
x = cbind(rep(1,n), x)

tau = 0.75
sd = 1
beta = matrix(c(rep(0.5, 7), rep(0, p - 6)))
obs_err = rnorm(n, sd = sd)
x[,2] = runif(n)
y = x %*% beta + matrix(tau * x[,2] * obs_err) + obs_err
beta_true = beta
beta_true[2] = beta[2] + tau * qnorm(tau)
beta_true[1] = beta[1] + qnorm(tau, sd = sd)

# lasso에 대해서
set.seed(2)
k = 10
lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1:n

each_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = lambda[j], method = 'lasso')
    each_beta <- each_lasso_rq$coefficients
    each_predicted_y <- x[ind_for, ] %*% each_beta
    each_error[j, i] <- quantile(abs(valid_y - each_predicted_y), 0.75)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}

each_lambda_error <- apply(each_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_lambda_error)  # cv를 통한 error의 형태가 이런식으로 나타남!
min_lambda_error <- min(each_lambda_error)
min_lambda_index <- which(each_lambda_error == min_lambda_error)
min_lambda_index 

min_lambda_lasso <- lambda[min_lambda_index]  
min_lambda_lasso

# optimum lasso rq fit

opt_lasso_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = min_lambda_lasso, method = 'lasso')
beta_optlasso_rq <- opt_lasso_rq_fit$coefficients

sum((beta_true - beta_optlasso_rq)^2) # 4.68



## for adaptive lasso qr and scad qr
set.seed(1)
lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1:n

each_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = lambda[j], method = 'lasso')
    each_beta <- each_lasso_rq$coefficients
    each_predicted_y <- x[ind_for, ] %*% each_beta
    each_error[j, i] <- quantile(abs(valid_y - each_predicted_y), 0.75)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}

each_lambda_error <- apply(each_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_lambda_error)  # cv를 통한 error의 형태가 이런식으로 나타남! 
min_lambda_error <- min(each_lambda_error)
min_lambda_index <- which(each_lambda_error == min_lambda_error)
min_lambda_index  

min_lambda_lasso <- lambda[min_lambda_index]  
min_lambda_lasso

lasso_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = min_lambda_lasso, method = 'lasso')
beta_lasso_rq <- lasso_rq_fit$coefficients
beta_lasso_rq


#adaptive lasso qr with tau = 0.75

set.seed(2)  # seed를 다르게 설정
lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1:n

weight_beta <- beta_lasso_rq %>% abs  # 베타 절대값의 역수만큼 weight을 줘야하니 바꿔준다.
weight_beta <- weight_beta[2:length(weight_beta)]  # 베타0의 가중치는 0이 되어야하니 제외

each_ad_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    penalty <-c(0, lambda[j] / weight_beta)  # 베타 0에 대한 lambda_0를 0으로 만들어준다.
    # 각각 람다에 대해 lasso beta로 나눈 값을 고려하고 있다.
    each_adlasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = penalty, method = 'lasso')
    each_ad_beta <- each_adlasso_rq$coefficients
    each_ad_predicted_y <- x[ind_for, ] %*% each_ad_beta
    each_ad_error[j, i] <- quantile(abs(valid_y - each_ad_predicted_y), 0.75)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}

each_adlambda_error <- apply(each_ad_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_adlambda_error)  # cv를 통한 error의 형태가 이런식으로 나타남!
min_adlambda_error <- min(each_adlambda_error)
min_adlambda_index <- which(each_adlambda_error == min_adlambda_error)
min_adlambda_index  

min_adlambda_lasso <- lambda[min_adlambda_index] 
min_adlambda_lasso

# optimum adaptive lasso rq fit

opt_adlasso_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = min_adlambda_lasso, method = 'lasso')
beta_adlasso_rq <- opt_adlasso_rq_fit$coefficients

length(which(abs(beta_adlasso_rq) > 10^(-5))) 


# error for Adaptive Lasso Quantile Regression
sum((beta_true - beta_adlasso_rq)^2) # 7.48


# SCAD tau = 0.75
set.seed(2)  
ind <- 1:n
ind_all <- 1:n
lambda = seq(0.01, 10, by = 0.01)

each_scad_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (i in 1:k) {
  ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
  ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
  ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
  ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
  train_x <- x[ind_each, ]
  train_y <- y[ind_each]
  valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
  valid_y <- y[ind_for]
  for (j in seq_along(lambda)) {
    weight = c(0, scad_deriv(abs(beta_lasso_rq[-1]), lam = lambda[j]))
    # 베타 0에 대한 lambda_0를 0으로 만들어준다.
    # 각각 람다에 대해 스캐드 미분함수를 고려한다.
    each_scad_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = weight, method = 'lasso')
    each_scad_beta <- each_scad_rq$coefficients
    each_scad_predicted_y <- x[ind_for, ] %*% each_scad_beta
    each_scad_error[j, i] <- quantile(abs(valid_y - each_scad_predicted_y), 0.75)
  }
  print(ind_for)
  print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
}


each_scad_error <- apply(each_scad_error, 1, mean) # 각각 람다 시퀀스에 대한 10fold error 기록
plot(each_scad_error)  # cv를 통한 error의 형태가 이런식으로 나타남!
min_scad_error <- min(each_scad_error)
min_scad_index <- which(each_scad_error == min_scad_error)
min_scad_index  

min_scad_lasso <- lambda[min_scad_index]  
min_scad_lasso

# optimum weighted lasso rq fit with scad derivative funtion

opt_weight = c(0, scad_deriv(abs(beta_lasso_rq[-1]), lam = min_scad_lasso))
opt_scad_rq_fit <- rq(y ~ x[,-1], tau = tau, lambda = opt_weight, method = 'lasso')

beta_scad_rq <- opt_scad_rq_fit$coefficients
length(which(abs(beta_scad_rq) > 10^(-5)))

# error for weighted lasso rq fit with scad derivative funtion
sum((beta_true - beta_scad_rq)^2) # 4.686



sum((beta_true - beta_optlasso_rq)^2) # 4.68
sum((beta_true - beta_adlasso_rq)^2) # 7.48
sum((beta_true - beta_scad_rq)^2) # 4.686


# tau = 0.75 에서 전체적으로 추정오차 제곱합이 크다. 그중에서 adaptive lasso의 추정오차가 가장 크다.
# 그리고 여기에서도 스캐드와 라쏘의 결과는 동일하게 나타난다.

# tau = 0.5 일때와 이외에 0.75, 0.25일때의 결과를 비교하면, 
# 베타 추정의 측면에서 0.5일때의 에러가 더 적다. 
# 결국 추정을 하는데에 있어서 제일 좋은 tau는 0.5라고 말할 수 있다.





# Q6. Suppose that design matrix X is generated from AR(1)-covariance structure 
# with a correlation parameter sigma between 0 and 1. 
# By varying a simga value (use seq(0.1,0.9,0.1)), 
# compare the performances of Lasso estimators in terms of estimation error. 
# Consider the quantile level tau = 0.25, 0.5, 0.75, respectively.


sigma <- seq(0.1, 0.9, 0.1)
corr_list <- list(NULL)
corr_mat <- matrix(0, nrow = p, ncol = p)
for (i in seq_along(sigma)) {
  rho <- sigma[i]
  for (j in 1:p) {
    for (k in 1:p) {
      difference <- abs(j - k)
      if (difference == 0) {
        corr_mat[j,k] <- 1
      } else {
        corr_mat[j,k] <- rho^difference
      }
    }
  }
  corr_list[[i]] <- corr_mat %>% round(6)
}

# AR1 covariance structure에 기반한 상관계수행렬을 만들었다. 주대각성분은 1이고, 변수간 거리가 늘어날 수록
# 상관관계가 약해지도록 만들었다.


lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1
k = 10
x_list <- list(NULL)
y_list <- list(NULL)
each_error_list <- list(NULL)
each_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (l in seq_along(sigma)) {
  set.seed(2019)  # 같은 시드에 대해서 상관관계 행렬만 다르게 해서 샘플링한다.
  ind <- 1:n
  ind_all <- 1:n
  n = 60; p = 30
  x <- rmvnorm(n = n, mean = rep(0, p), sigma = corr_list[[l]])
  x = cbind(rep(1,n), x)
  tau = 0.5
  sd = 1
  beta = matrix(c(rep(0.5, 7), rep(0, p - 6)))
  obs_err = rnorm(n, sd = sd)
  x[,2] = runif(n)
  y = x %*% beta + matrix(tau * x[,2] * obs_err) + obs_err
  beta_true = beta
  beta_true[2] = beta[2] + tau * qnorm(tau)
  beta_true[1] = beta[1] + qnorm(tau, sd = sd)
  x_list[[l]] <- x # 상관관계 행렬마다 바뀌는 우리의 데이터를 리스트로 저장한다.
  y_list[[l]] <- y
  for (i in 1:k) {
    ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
    ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
    ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
    ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
    train_x <- x[ind_each, ]
    train_y <- y[ind_each]
    valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
    valid_y <- y[ind_for]
    for (j in seq_along(lambda)) {
      each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = lambda[j], method = 'lasso')
      each_beta <- each_lasso_rq$coefficients
      each_predicted_y <- x[ind_for, ] %*% each_beta
      each_error[j, i] <- quantile(abs(valid_y - each_predicted_y), 0.5)
    }
    #print(ind_for)
    #print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
  }
  each_error_list[[l]] <- each_error
  print(l)# l번 잘 작동하는지 확인하기 위한 작업
}


each_lambda_error_list <- list(NULL)
min_lambda_error_list <- list(NULL)
min_lambda_index_list <- list(NULL)
min_lambda_lasso_list <- list(NULL)
for (i in seq_along(sigma)) {
  each_lambda_error_list[[i]] <- apply(each_error_list[[i]], 1, mean)
  min_lambda_error_list[[i]] <- min(each_lambda_error_list[[i]])
  min_lambda_index_list[[i]] <- which(each_lambda_error_list[[i]] == min_lambda_error_list[[i]])
  min_lambda_lasso_list[[i]] <- lambda[min_lambda_index_list[[i]]]
}

lambda_lasso_vector <- min_lambda_lasso_list %>% unlist
lambda_lasso_vector
# 2.20 2.27 2.20 1.98 1.94 1.86 1.95 1.53 1.17
plot(lambda_lasso_vector, type = 'b')
# 람다가 작아지는 경향이 나타난다.

# optimum lasso rq fit
sigma_lasso_fit <- NULL
error_vector <- rep(0, length(lambda_lasso_vector))
for (i in seq_along(lambda_lasso_vector)) {
  x_data <- x_list[[i]]
  y_data <- y_list[[i]]
  sigma_lasso_fit <- rq(y_data ~ x_data[, -1], tau = tau, lambda_lasso_vector[i], method = 'lasso')
  beta_sigma_lasso_rq <- sigma_lasso_fit$coefficients
  error_vector[i] <- sum((beta_true - beta_sigma_lasso_rq)^2)
  print(i)
}

names(error_vector) <- c('0.1', '0.2', '0.3', '0.4', '0.5', '0.6', '0.7', '0.8', '0.9')
error_vector

#   0.1      0.2      0.3      0.4      0.5      0.6      0.7      0.8      0.9 
# 1.377606 1.419574 1.529862 1.631743 1.791691 2.039142 2.756524 4.064099 5.572387 

plot(error_vector, type = 'b')
# 변수간 상관관계가 클수록 추정오차가 점점 증가하는 것을 보여준다!

################
#### tau = 0.25

lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1
k = 10
x_list <- list(NULL)
y_list <- list(NULL)
each_error_list <- list(NULL)
each_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (l in seq_along(sigma)) {
  set.seed(2019)  # 같은 시드에 대해서 상관관계 행렬만 다르게 해서 샘플링한다.
  ind <- 1:n
  ind_all <- 1:n
  n = 60; p = 30
  x <- rmvnorm(n = n, mean = rep(0, p), sigma = corr_list[[l]])
  x = cbind(rep(1,n), x)
  tau = 0.25  #### tau = 0.25
  sd = 1
  beta = matrix(c(rep(0.5, 7), rep(0, p - 6)))
  obs_err = rnorm(n, sd = sd)
  x[,2] = runif(n)
  y = x %*% beta + matrix(tau * x[,2] * obs_err) + obs_err
  beta_true = beta
  beta_true[2] = beta[2] + tau * qnorm(tau)
  beta_true[1] = beta[1] + qnorm(tau, sd = sd)
  x_list[[l]] <- x # 상관관계 행렬마다 바뀌는 우리의 데이터를 리스트로 저장한다.
  y_list[[l]] <- y
  for (i in 1:k) {
    ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
    ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
    ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
    ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
    train_x <- x[ind_each, ]
    train_y <- y[ind_each]
    valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
    valid_y <- y[ind_for]
    for (j in seq_along(lambda)) {
      each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = lambda[j], method = 'lasso')
      each_beta <- each_lasso_rq$coefficients
      each_predicted_y <- x[ind_for, ] %*% each_beta
      each_error[j, i] <- quantile(abs(valid_y - each_predicted_y), 0.25)
    }
    #print(ind_for)
    #print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
  }
  each_error_list[[l]] <- each_error
  print(l) # l번 잘 작동하는지 확인하기 위한 작업
}


each_lambda_error_list <- list(NULL)
min_lambda_error_list <- list(NULL)
min_lambda_index_list <- list(NULL)
min_lambda_lasso_list <- list(NULL)
for (i in seq_along(sigma)) {
  each_lambda_error_list[[i]] <- apply(each_error_list[[i]], 1, mean)
  min_lambda_error_list[[i]] <- min(each_lambda_error_list[[i]])
  min_lambda_index_list[[i]] <- which(each_lambda_error_list[[i]] == min_lambda_error_list[[i]])
  min_lambda_lasso_list[[i]] <- lambda[min_lambda_index_list[[i]]]
}

lambda_lasso_vector <- min_lambda_lasso_list %>% unlist
lambda_lasso_vector
#7.67 6.24 6.64 9.64 4.85 4.94 4.57 9.64 9.34
plot(lambda_lasso_vector, type = 'b')
# 같은경향이 tau = 0.25에서도 나타난다. 물론 중간에 갑자기 튀어오른 값이 있지만, 이상현상이라고 생각할 수 있다.


# optimum lasso rq fit
sigma_lasso_fit <- NULL
error_vector <- rep(0, length(lambda_lasso_vector))
for (i in seq_along(lambda_lasso_vector)) {
  x_data <- x_list[[i]]
  y_data <- y_list[[i]]
  sigma_lasso_fit <- rq(y_data ~ x_data[, -1], tau = tau, lambda_lasso_vector[i], method = 'lasso')
  beta_sigma_lasso_rq <- sigma_lasso_fit$coefficients
  error_vector[i] <- sum((beta_true - beta_sigma_lasso_rq)^2)
  print(i)
}
# Error in eval(predvars, data, env) : not that many frames on the stack
names(error_vector) <- c('0.1', '0.2', '0.3', '0.4', '0.5', '0.6', '0.7', '0.8', '0.9')
error_vector

#   0.1      0.2      0.3      0.4      0.5      0.6      0.7      0.8      0.9 
#1.361156 1.391118 1.467933 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000 
# 모델이 작동하지 않는 경우, 그냥 0으로 기록했다.

plot(error_vector, type = 'b')
# 시그마가 커질수록, 모델이 작동을 안한다.


#############
### tau = 0.75

lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1
k = 10
x_list <- list(NULL)
y_list <- list(NULL)
each_error_list <- list(NULL)
each_error <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
for (l in seq_along(sigma)) {
  set.seed(2019)  # 같은 시드에 대해서 상관관계 행렬만 다르게 해서 샘플링한다.
  ind <- 1:n
  ind_all <- 1:n
  n = 60; p = 30
  x <- rmvnorm(n = n, mean = rep(0, p), sigma = corr_list[[l]])
  x = cbind(rep(1,n), x)
  tau = 0.75  #### tau = 0.75
  sd = 1
  beta = matrix(c(rep(0.5, 7), rep(0, p - 6)))
  obs_err = rnorm(n, sd = sd)
  x[,2] = runif(n)
  y = x %*% beta + matrix(tau * x[,2] * obs_err) + obs_err
  beta_true = beta
  beta_true[2] = beta[2] + tau * qnorm(tau)
  beta_true[1] = beta[1] + qnorm(tau, sd = sd)
  x_list[[l]] <- x # 상관관계 행렬마다 바뀌는 우리의 데이터를 리스트로 저장한다.
  y_list[[l]] <- y
  for (i in 1:k) {
    ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
    ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
    ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
    ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
    train_x <- x[ind_each, ]
    train_y <- y[ind_each]
    valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
    valid_y <- y[ind_for]
    for (j in seq_along(lambda)) {
      each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = lambda[j], method = 'lasso')
      each_beta <- each_lasso_rq$coefficients
      each_predicted_y <- x[ind_for, ] %*% each_beta
      each_error[j, i] <- quantile(abs(valid_y - each_predicted_y),0.75)
    }
    #print(ind_for)
    #print(ind)  # print를 통해 직접 짠 cv_fold함수가 잘 작동하는지를 확인한다. 문제가 없다.
  }
  each_error_list[[l]] <- each_error
  print(l) # l번 잘 작동하는지 확인하기 위한 작업
}


each_lambda_error_list <- list(NULL)
min_lambda_error_list <- list(NULL)
min_lambda_index_list <- list(NULL)
min_lambda_lasso_list <- list(NULL)
for (i in seq_along(sigma)) {
  each_lambda_error_list[[i]] <- apply(each_error_list[[i]], 1, mean)
  min_lambda_error_list[[i]] <- min(each_lambda_error_list[[i]])
  min_lambda_index_list[[i]] <- which(each_lambda_error_list[[i]] == min_lambda_error_list[[i]])
  min_lambda_lasso_list[[i]] <- lambda[min_lambda_index_list[[i]]]
}

lambda_lasso_vector <- min_lambda_lasso_list %>% unlist
lambda_lasso_vector
#  7.50 6.67 6.69 6.27 7.03 6.65 6.82 5.14 9.41
plot(lambda_lasso_vector, type = 'b')



# optimum lasso rq fit
sigma_lasso_fit <- NULL
error_vector <- rep(0, length(lambda_lasso_vector))
for (i in seq_along(lambda_lasso_vector)) {
  x_data <- x_list[[i]]
  y_data <- y_list[[i]]
  sigma_lasso_fit <- rq(y_data ~ x_data[, -1], tau = tau, lambda_lasso_vector[i], method = 'lasso')
  beta_sigma_lasso_rq <- sigma_lasso_fit$coefficients
  error_vector[i] <- sum((beta_true - beta_sigma_lasso_rq)^2)
  print(i)
}
# Error in eval(predvars, data, env) : not that many frames on the stack
names(error_vector) <- c('0.1', '0.2', '0.3', '0.4', '0.5', '0.6', '0.7', '0.8', '0.9')
error_vector

#   0.1      0.2      0.3      0.4      0.5      0.6      0.7      0.8      0.9 
# 2.910968 3.043536 3.076815 2.912499 3.123237 3.488799 3.850182 4.828573 0.000000 
# 모델이 작동하지 않는 경우, 그냥 0으로 기록했다.

plot(error_vector, type = 'b')
# 이번에도 비슷한 경향이 발생했다. 상관관계가 0.9일때 모델이 작동하지 않는다. 
# 뿐만아니라 전체적으로 추정오차가 증가하는 경향을 보인다.


# 6번의 결론.
# 변수간 상관관계가 클수록 lasso quantile regression은 잘 작동하지 않고,
# 이는 tau가 0.5가 아닐때 더 큰 문제로 나타난다.
# 또한 전체적으로 상관관계가 클수록 추정오차가 증가하는 경향을 보인다.



# Q7. Consider the model used in Q5 but errors follow from normal mixture distribution.
# Specifically, errors follow 0.9 N(0,1) + 0.1 N(0, variance), 
# where variance = 2,4,9,16, respectively.  # variance가 커질수록 예측력이 떨어지겠지??
# For mixture distribution refer to 
# https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/NormalMix
# Consider tau = 0.5 and obtain estimation errors for the quantile regression estimator 
# using Lasso, Adaptive Lasso, and weighted Lasso (using SCAD derivative), respectively. 
# Since this quantile regression model is homoscedasticity, 
# underlying "beta_true" corresponds to underlying coefficients for the regular linear regression. 
# Apply linear regression using Lasso, adaptive Lasso, and weighte Lasso (using SCAD derivative), 
# respectively, and obtain estimation errrors.
# Between quantile regression and linear regression estimators, 
# which one is better, 
# i.e. yields a smaller estimation error?

stdv <- c(sqrt(2), 2, 3, 4)

###### for lasso

#lasso qr
lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1
k = 10
x_list <- list(NULL)
y_list <- list(NULL)
each_rq_error_list <- list(NULL)
each_linear_error_list <- list(NULL)
each_error_rq <- matrix(0, nrow = length(lambda), ncol = k)  # rss를 각각 람다에 대해 저장하는 매트릭스
each_error_linear <- matrix(0, nrow = length(lambda), ncol = k)
true <- list(NULL)
for (l in seq_along(stdv)) {
  ind <- 1:n
  ind_all <- 1:n
  set.seed(2019) # seed 지정
  n = 60; p = 30
  x = matrix(rnorm(n * p), ncol = p)
  x = cbind(rep(1,n), x)
  tau = 0.5
  beta = matrix(c(rep(0.5, 7), rep(0, p - 6)))
  obs_err = 9/10 * rnorm(n, sd = 1) + 1/10 * rnorm(n, mean = 0, sd = stdv[l])
  x[,2] = runif(n)
  y = x %*% beta + matrix(tau * x[,2] * obs_err) + obs_err
  beta_true = beta
  beta_true[2] = beta[2] + tau * qnorm(tau)
  beta_true[1] = beta[1] + qnorm(tau, sd = sd)
  true[[l]] <- beta_true
  x_list[[l]] <- x # 
  y_list[[l]] <- y # 에러가 바뀔때마다 내용을 저장
  for (i in 1:k) {
    ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
    ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
    ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
    ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
    train_x <- x[ind_each, ]
    train_y <- y[ind_each]
    valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
    valid_y <- y[ind_for]
    for (j in seq_along(lambda)) {
      each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = lambda[j], method = 'lasso')
      each_beta_rq <- each_lasso_rq$coefficients
      each_predicted_y <- x[ind_for, ] %*% each_beta_rq
      each_error_rq[j, i] <- quantile(abs(valid_y - each_predicted_y),0.5)
      
      each_lasso_linear <- glmnet(train_x[, -1], train_y, lambda = lambda[j], alpha = 1)
      each_beta_linear <- rbind(each_lasso_linear$a0, each_lasso_linear$beta)
      each_linear_predicted_y <- x[ind_for, ] %*% each_beta_linear
      each_error_linear[j,i] <- sum((valid_y - each_linear_predicted_y)^2)
    }
  }
  each_rq_error_list[[l]] <- each_error_rq
  each_linear_error_list[[l]] <- each_error_linear
  print(l)
}

each_lam_rq_error_list <- list(NULL)
each_lam_linear_error_list <- list(NULL)
min_rq_error_list <- list(NULL)
min_linear_error_list <- list(NULL)
min_rq_index_list <- list(NULL)
min_linear_index_list <- list(NULL)
min_rq_lasso_list <- list(NULL)
min_linear_lasso_list <- list(NULL)
for (i in seq_along(stdv)) {
  each_lam_rq_error_list[[i]] <- apply(each_rq_error_list[[i]], 1, mean)
  each_lam_linear_error_list[[i]] <- apply(each_linear_error_list[[i]], 1, mean)
  min_rq_error_list[[i]] <- min(each_lam_rq_error_list[[i]])
  min_linear_error_list[[i]] <- min(each_lam_linear_error_list[[i]])
  min_rq_index_list[[i]] <- which(each_lam_rq_error_list[[i]] == min_rq_error_list[[i]])
  min_linear_index_list[[i]] <- which(each_lam_linear_error_list[[i]] == min_linear_error_list[[i]])
  min_rq_lasso_list[[i]] <- lambda[min_rq_index_list[[i]]]
  min_linear_lasso_list[[i]] <- lambda[min_linear_index_list[[i]]]
}

lambda_rq_vector <- min_rq_lasso_list %>% unlist
lambda_linear_vector <- min_linear_lasso_list %>% unlist
lambda_rq_vector # 4.23 3.11 3.06 2.97
lambda_linear_vector # 0.10 0.10 0.11 0.11

variance_rq_laaso_fit <- NULL
variance_linear_lasso_fit <- NULL
beta_variance_lasso_rq <- list(NULL)
beta_variance_linear <- list(NULL)
error_rq_vector <- rep(0, length(lambda_rq_vector))
error_linear_vector <- rep(0, length(lambda_linear_vector))
for (i in seq_along(lambda_rq_vector)) {
  x_data <- x_list[[i]]
  y_data <- y_list[[i]]
  variance_rq_lasso_fit <- rq(y_data ~ x_data[, -1], tau = tau, lambda = lambda_rq_vector[i], method = 'lasso')
  beta_variance_lasso_rq[[i]] <- variance_rq_lasso_fit$coefficients
  error_rq_vector[i] <- sum((true[[i]] - beta_variance_lasso_rq[[i]])^2)
  
  variance_linear_lasso_fit <- glmnet(x_data[, -1], y_data, lambda = lambda_linear_vector[i], alpha = 1)
  beta_variance_linear[[i]] <- rbind(variance_linear_lasso_fit$a0, variance_linear_lasso_fit$beta)
  error_linear_vector[i] <- sum((true[[i]] - beta_variance_linear[[i]])^2)
}

error_rq_vector
#  0.8364276 0.9519339 1.2584946 1.4379704
error_linear_vector
# 0.4308296 0.4702891 0.5572955 0.6777991

plot(y = error_linear_vector, x = stdv, type = 'b', lwd = 3, col = 'blue', 
     ylim = c(0, 3))
lines(error_rq_vector, x = stdv, lwd = 3, col = 'red')
points(error_rq_vector, x = stdv, cex = 1.5, col = 'red', pch =16)
# quantile lasso의 추정오차가 더 크다.



## adaptive lasso에 대하여

# beta_variance_lasso_rq
# beta_variance_linear
# 이 두 베타계수 벡터를 adaptive lasso의 베타틸데로 사용하겠다.

lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1
k = 10
x_list <- list(NULL)
y_list <- list(NULL)
each_rq_error_list <- list(NULL)
each_linear_error_list <- list(NULL)
true <- list(NULL)
each_error_rq <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
each_error_linear <- matrix(0, nrow = length(lambda), ncol = k)
for (l in seq_along(stdv)) {
  ind <- 1:n
  ind_all <- 1:n
  set.seed(2019) # seed 지정
  n = 60; p = 30
  x = matrix(rnorm(n * p), ncol = p)
  x = cbind(rep(1,n), x)
  tau = 0.5
  beta = matrix(c(rep(0.5, 7), rep(0, p - 6)))
  obs_err = 9/10 * rnorm(n, sd = 1) + 1/10 * rnorm(n, mean = 0, sd = stdv[l])
  x[,2] = runif(n)
  y = x %*% beta + matrix(tau * x[,2] * obs_err) + obs_err
  beta_true = beta
  beta_true[2] = beta[2] + tau * qnorm(tau)
  beta_true[1] = beta[1] + qnorm(tau, sd = sd)
  true[[l]] <- beta_true
  x_list[[l]] <- x # 
  y_list[[l]] <- y # 에러가 바뀔때마다 내용을 저장
  for (i in 1:k) {
    ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
    ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
    ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
    ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
    train_x <- x[ind_each, ]
    train_y <- y[ind_each]
    valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
    valid_y <- y[ind_for]
    for (j in seq_along(lambda)) {
      rq_beta_tilde <- beta_variance_lasso_rq[[l]] %>% abs
      rq_penalty <- c(0, lambda[j] / rq_beta_tilde[-1])
      each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = rq_penalty, method = 'lasso')
      each_beta_rq <- each_lasso_rq$coefficients
      each_predicted_y <- x[ind_for, ] %*% each_beta_rq
      each_error_rq[j, i] <- quantile(abs(valid_y - each_predicted_y), 0.5)
      
      
      linear_beta_tilde <- beta_variance_linear[[l]] %>% abs
      linear_beta_tilde <- ifelse(linear_beta_tilde == 0, 1e-08, linear_beta_tilde)
      # 베타가 0일 경우, 가중치가 무한으로 잡히는걸 방지해주기 위해
      linear_penalty <- c(0, lambda[j] / linear_beta_tilde[-1])
      each_lasso_linear <- glmnet(train_x[, -1], train_y, 
                                  lambda = linear_penalty, alpha = 1)
      each_beta_linear <- rbind(each_lasso_linear$a0, each_lasso_linear$beta)
      each_linear_predicted_y <- x[ind_for, ] %*% each_beta_linear
      each_error_linear[j,i] <- sum((valid_y - each_linear_predicted_y)^2)
    }
  }
  each_rq_error_list[[l]] <- each_error_rq
  each_linear_error_list[[l]] <- each_error_linear
  print(l)
}

each_lam_rq_error_list <- list(NULL)
each_lam_linear_error_list <- list(NULL)
min_rq_error_list <- list(NULL)
min_linear_error_list <- list(NULL)
min_rq_index_list <- list(NULL)
min_linear_index_list <- list(NULL)
min_rq_lasso_list <- list(NULL)
min_linear_lasso_list <- list(NULL)
for (i in seq_along(stdv)) {
  each_lam_rq_error_list[[i]] <- apply(each_rq_error_list[[i]], 1, mean)
  each_lam_linear_error_list[[i]] <- apply(each_linear_error_list[[i]], 1, mean)
  min_rq_error_list[[i]] <- min(each_lam_rq_error_list[[i]])
  min_linear_error_list[[i]] <- min(each_lam_linear_error_list[[i]])
  min_rq_index_list[[i]] <- which(each_lam_rq_error_list[[i]] == min_rq_error_list[[i]])
  min_linear_index_list[[i]] <- which(each_lam_linear_error_list[[i]] == min_linear_error_list[[i]])
  min_rq_lasso_list[[i]] <- lambda[min_rq_index_list[[i]]]
  min_linear_lasso_list[[i]] <- lambda[min_linear_index_list[[i]]]
}

lambda_rq_vector <- min_rq_lasso_list %>% unlist
lambda_linear_vector <- min_linear_lasso_list %>% unlist
lambda_rq_vector # 0.64 1.10 1.07 0.99
lambda_linear_vector # 0.01 0.01 0.01 0.02

variance_rq_laaso_fit <- NULL
variance_linear_lasso_fit <- NULL
beta_variance_lasso_rq <- list(NULL)
beta_variance_linear <- list(NULL)
error_rq_vector <- rep(0, length(lambda_rq_vector))
error_linear_vector <- rep(0, length(lambda_linear_vector))
for (i in seq_along(lambda_rq_vector)) {
  x_data <- x_list[[i]]
  y_data <- y_list[[i]]
  variance_rq_lasso_fit <- rq(y_data ~ x_data[, -1], tau = tau, lambda = lambda_rq_vector[i], method = 'lasso')
  beta_variance_lasso_rq[[i]] <- variance_rq_lasso_fit$coefficients
  error_rq_vector[i] <- sum((true[[i]] - beta_variance_lasso_rq[[i]])^2)
  
  variance_linear_lasso_fit <- glmnet(x_data[, -1], y_data, lambda = lambda_linear_vector[i], alpha = 1)
  beta_variance_linear[[i]] <- rbind(variance_linear_lasso_fit$a0, variance_linear_lasso_fit$beta)
  error_linear_vector[i] <- sum((true[[i]] - beta_variance_linear[[i]])^2)
}

error_rq_vector
# 2.235412 1.187215 1.636302 2.072584
error_linear_vector
# 1.557832 1.613845 1.726111 1.575780

plot(y = error_linear_vector, x = stdv, type = 'b', lwd = 3, col = 'blue',
     ylim = c(0, 3))
lines(error_rq_vector, x = stdv, lwd = 3, col = 'red')
points(error_rq_vector, x = stdv, cex = 1.5, col = 'red', pch =16)
# quantile regression에서는 첫 값을 제외하곤 분산이 커질 수록 추정오차가 증가하는 경향이 있다.


######  
# SCAD

lambda = seq(0.01, 10, by = 0.01)
ind <- 1:n
ind_all <- 1
k = 10
x_list <- list(NULL)
y_list <- list(NULL)
each_rq_error_list <- list(NULL)
each_linear_error_list <- list(NULL)
true <- list(NULL)
each_error_rq <- matrix(0, nrow = length(lambda), ncol = k)  # error를 각각 람다에 대해 저장하는 매트릭스
each_error_linear <- matrix(0, nrow = length(lambda), ncol = k)
for (l in seq_along(stdv)) {
  ind <- 1:n
  ind_all <- 1:n
  set.seed(2019) # seed 지정
  n = 60; p = 30
  x = matrix(rnorm(n * p), ncol = p)
  x = cbind(rep(1,n), x)
  tau = 0.5
  beta = matrix(c(rep(0.5, 7), rep(0, p - 6)))
  obs_err = 9/10 * rnorm(n, sd = 1) + 1/10 * rnorm(n, mean = 0, sd = stdv[l])
  x[,2] = runif(n)
  y = x %*% beta + matrix(tau * x[,2] * obs_err) + obs_err
  beta_true = beta
  beta_true[2] = beta[2] + tau * qnorm(tau)
  beta_true[1] = beta[1] + qnorm(tau, sd = sd)
  true[[l]] <- beta_true
  x_list[[l]] <- x # 
  y_list[[l]] <- y # 에러가 바뀔때마다 내용을 저장
  for (i in 1:k) {
    ind_all <- 1:n  # 1부터  n까지 인덱스가 있다.
    ind_for <- sample(ind, n/k, replace = F)  # 이중에 n/k = 6개를 비복원추출한다.
    ind <- setdiff(ind, ind_for)  # 한번 뽑혔으면 다음에 뽑히면 안된다.
    ind_each <- ind_all[-ind_for]  # 뽑힌 친구들을 제외한 train set의 인덱스
    train_x <- x[ind_each, ]
    train_y <- y[ind_each]
    valid_x <- x[ind_for, ]  # 뽑힌 친구들이 validation set의 인덱스로 활용
    valid_y <- y[ind_for]
    for (j in seq_along(lambda)) {
      rq_beta_tilde <- beta_variance_lasso_rq[[l]] %>% abs
      rq_weight = c(0, scad_deriv(rq_beta_tilde[-1], lam = lambda[j])) # 스캐드 미분함수의 가중치
      each_lasso_rq <- rq(train_y ~ train_x[, -1], tau = tau, lambda = rq_weight, method = 'lasso')
      each_beta_rq <- each_lasso_rq$coefficients
      each_predicted_y <- x[ind_for, ] %*% each_beta_rq
      each_error_rq[j, i] <- quantile(abs(valid_y - each_predicted_y),0.5)
      
      linear_beta_tilde <- beta_variance_linear[[l]] %>% abs
      linear_weight = c(0, scad_deriv(linear_beta_tilde[-1], lam = lambda[j]))
      each_lasso_linear <- glmnet(train_x[, -1], train_y, 
                                  lambda = linear_weight, alpha = 1)
      each_beta_linear <- rbind(each_lasso_linear$a0, each_lasso_linear$beta)
      each_linear_predicted_y <- x[ind_for, ] %*% each_beta_linear
      each_error_linear[j,i] <- sum((valid_y - each_linear_predicted_y)^2)
    }
  }
  each_rq_error_list[[l]] <- each_error_rq
  each_linear_error_list[[l]] <- each_error_linear
  print(l)
}

each_lam_rq_error_list <- list(NULL)
each_lam_linear_error_list <- list(NULL)
min_rq_error_list <- list(NULL)
min_linear_error_list <- list(NULL)
min_rq_index_list <- list(NULL)
min_linear_index_list <- list(NULL)
min_rq_lasso_list <- list(NULL)
min_linear_lasso_list <- list(NULL)
for (i in seq_along(stdv)) {
  each_lam_rq_error_list[[i]] <- apply(each_rq_error_list[[i]], 1, mean)
  each_lam_linear_error_list[[i]] <- apply(each_linear_error_list[[i]], 1, mean)
  min_rq_error_list[[i]] <- min(each_lam_rq_error_list[[i]])
  min_linear_error_list[[i]] <- min(each_lam_linear_error_list[[i]])
  min_rq_index_list[[i]] <- which(each_lam_rq_error_list[[i]] == min_rq_error_list[[i]])
  min_linear_index_list[[i]] <- which(each_lam_linear_error_list[[i]] == min_linear_error_list[[i]])
  min_rq_lasso_list[[i]] <- lambda[min_rq_index_list[[i]]]
  min_linear_lasso_list[[i]] <- lambda[min_linear_index_list[[i]]]
}

lambda_rq_vector <- min_rq_lasso_list %>% unlist
lambda_linear_vector <- min_linear_lasso_list %>% unlist
lambda_rq_vector # 4.23 3.11 3.06 2.97
lambda_linear_vector # 0.18 0.19 0.20 0.21

variance_rq_laaso_fit <- NULL
variance_linear_lasso_fit <- NULL
beta_variance_lasso_rq <- list(NULL)
beta_variance_linear <- list(NULL)
error_rq_vector <- rep(0, length(lambda_rq_vector))
error_linear_vector <- rep(0, length(lambda_linear_vector))
for (i in seq_along(lambda_rq_vector)) {
  x_data <- x_list[[i]]
  y_data <- y_list[[i]]
  variance_rq_lasso_fit <- rq(y_data ~ x_data[, -1], tau = tau, lambda = lambda_rq_vector[i], method = 'lasso')
  beta_variance_lasso_rq[[i]] <- variance_rq_lasso_fit$coefficients
  error_rq_vector[i] <- sum((true[[i]] - beta_variance_lasso_rq[[i]])^2)
  
  variance_linear_lasso_fit <- glmnet(x_data[, -1], y_data, lambda = lambda_linear_vector[i], alpha = 1)
  beta_variance_linear[[i]] <- rbind(variance_linear_lasso_fit$a0, variance_linear_lasso_fit$beta)
  error_linear_vector[i] <- sum((true[[l]] - beta_variance_linear[[i]])^2)
}

error_rq_vector
# 0.8364276 0.9519339 1.2584946 1.4379704
error_linear_vector
# 0.6378238 0.6912100 0.7783083 0.8776945

plot(y = error_linear_vector, x = stdv, type = 'b', lwd = 3, col = 'blue',
     ylim = c(0, 3))
lines(error_rq_vector, x = stdv, lwd = 3, col = 'red')
points(error_rq_vector, x = stdv, cex = 1.5, col = 'red', pch =16)
# SCAD에서는  quantile regression의 추정오차가 linear regression의 추정오차보다 더 적다.
# 또한 전체적으로 분산이 증가할수록 추정오차가 증가한다.

#7번의 결론
# 전체적으로 분산이 증가할때에 추정오차가 증가한다. 
# 또한 처음 가정에는 등분산이 가정되어있지 않지만, 
# tau = 0.5 이기 때문에 큰 영향이 없어 linear regressiong model에 오차가 크게 증가하지 않는 것으로 보인다.
