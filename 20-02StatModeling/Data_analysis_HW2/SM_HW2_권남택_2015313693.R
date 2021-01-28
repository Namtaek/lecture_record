## ---- message = FALSE-----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

library(gam) # 1번에서 사용
library(astsa) # 2번에서 사용

# 1. Consider the data in `house.csv’ file. 
# The our goal is to build the best model predicting house price based on some factors in a city.

## -------------------------------------------------------------------------------------------------
# 데이터 불러오기
house = data.table::fread('house-1.csv')

# 1-(1). Using the data modelling techniques (parametric modelling), 
# build your model and fit it to the data for attaining the lowest AIC value. To compute AIC in R, use AIC built-in function.

## -------------------------------------------------------------------------------------------------
# 기본 베이스라인 모델 구축
lm_fit1 = lm(price ~ age + dist + store, data = house)
summary(lm_fit1)
AIC(lm_fit1) # 3022


## -------------------------------------------------------------------------------------------------
# 회귀가정 확인
par(mfrow = c(2,2))
plot(lm_fit1)
par(mfrow = c(1,1))


## -------------------------------------------------------------------------------------------------
car::ncvTest(lm_fit1) # 등분산 기각 불가
car::durbinWatsonTest(lm_fit1) # 독립성 기각 불가
shapiro.test(lm_fit1$residuals) # 정규성 기각


## -------------------------------------------------------------------------------------------------
# 정규성의 확인
par(mfrow = c(1,2))
hist(house$price, main = 'Historgram of House Price')
hist(lm_fit1$residuals, main = 'Historgram of Residuals')
par(mfrow = c(1,1))


## -------------------------------------------------------------------------------------------------
# 고차항 고려
gam_fit1 = gam(price ~ s(age,5) + s(dist,5) + s(store, 5), data = house)

par(mfrow=c(1,3))
plot(gam_fit1)  
par(mfrow = c(1,1))


## -------------------------------------------------------------------------------------------------
# dist
gam_fit1_3 = gam(price ~ s(age,5) + dist + s(store, 5), data = house)
anova(gam_fit1, gam_fit1_3)

# store
gam_fit1_2 = gam(price ~ s(age,5) + s(dist,5) + store, data = house)
anova(gam_fit1, gam_fit1_2)


## -------------------------------------------------------------------------------------------------
# 고차항 적합 회귀
lm_fit2 = lm(price ~ age + I(age^2) + dist + I(dist^2) + I(dist^3) + store , data = house)
par(mfrow = c(2,2))
plot(lm_fit2)
par(mfrow = c(1,1))
AIC(lm_fit2) # 2945.848


## ---- warning = FALSE, message = FALSE------------------------------------------------------------
# date에 따른 price 비교
house %>% mutate(data = date %>% factor) %>% group_by(date) %>% summarise(mean_price = mean(price)) %>%  
  ggplot(aes(x = date, y = mean_price)) + geom_bar(stat = 'identity', alpha = 0.8) +
  geom_hline(yintercept = house$price %>% mean, color = 'royalblue3', size = 2, linetype = 'dashed', alpha = 0.8)


## -------------------------------------------------------------------------------------------------
# ANOVA로 확인
anova_result = aov(price ~ as.factor(date), data = house)
summary(anova_result)


## -------------------------------------------------------------------------------------------------
# month를 추가로
house = house %>% mutate(month = as.numeric(factor(date)))
lm_fit3 = lm(price ~ age + I(age^2) + dist + I(dist^2) + I(dist^3) + store + month, 
               data = house)
AIC(lm_fit3) # 2923.27


## -------------------------------------------------------------------------------------------------
# 정규성의 확인
par(mfrow = c(1,2))
hist(house$price, main = 'Historgram of House Price')
hist(lm_fit1$residuals, main = 'Historgram of Residuals')
par(mfrow = c(1,1))


## -------------------------------------------------------------------------------------------------
# 정규성이 해결되지 않으므로 GLM으로 잡아준다. Gamma 회귀의 canonical link
glm_fit1 = glm(price ~ age + I(age^2) + dist + I(dist^2) + I(dist^3) + store + month, 
               data = house, family = Gamma(link = 'inverse'))
AIC(glm_fit1)  # 2881.162


## -------------------------------------------------------------------------------------------------
# 위경도에 빠른 가격의 분포 확인
quant_cut = quantile(house$price, c(0.25, 0.5, 0.75, 1))
house %>% mutate(price_quantile = cut(price, c(0, quant_cut), c('low', 'medium-low', 'medium-high', 'high'))) %>% 
  ggplot(aes(x = lat, y = lon, color = price_quantile)) + geom_point(size = 3, alpha = 0.8)


## -------------------------------------------------------------------------------------------------
# 유의미해보이는 lat 변수 추가
glm_fit2 = glm(price ~ age + I(age^2) + dist + I(dist^2) + I(dist^3) + month + lat, 
               data = house, family = Gamma(link = 'inverse'))
AIC(glm_fit2)  # 2801.67


# 1-(2). Based on the model obtained from part (1), 
# interpret the relationship between house price and each input variable as detail as possible.


## -------------------------------------------------------------------------------------------------
summary(glm_fit2)

# 현재 사용한 Gamma Regression의 link는 inverse이다. 따라서 기본적으로 나오는 형태를 수식으로 표현하면 다음과 같다. 1/mu = XB
# 이는 mu의 역수에 대한 해당 결과들의 선형결합이다. 이를 양변에 역수를 취해주게 되면, 다음과 같은 형태가 나타난다. mu = 1/XB

# y=1/x의 그래프를 생각하면, x가 증가함에 따라 y는 감소함을 알 수 있다. 따라서 summary에서 양의 부호가 나오는 변수는, 
# 오히려 한 단위 증가함에 따라 평균적인 가격이 떨어진다고 해석할 수 있다. 

# 반면에 음의 부호를 가지는 변수는, 한 단위 증가함에 따라 평균적인 가격이 상승한다고 볼 수 있다. 
# 다만 이에 대해 정확한 단위로서 얼마가 증가한다고 말하는 것은 어렵다.

# 개별 변수에 대해서 해석해보려한다. 
# age의 경우 이차형태를 고려해주었는데, age에 대한 이차식 자체는 위로 볼록한 concave형태의 그래프이다. 
# 따라서 age가 올라감에 따라 대응하는 값이 상승하다가 어느순간 다시 감소하는 형태이다. 
# 그러므로 inverse link를 고려하게 되면, 건물의 연차가 올라갈수록 평균적인 가격이 감소하다가 어느순간 다시 증가한다고 해석할 수 있다.

# dist의 경우 삼차 형태를 고려해주었는데, 전체적으로 우상향 곡선이다. 
# 따라서 inverse link를 고려하게 되면, 가장 가까운 지하철역으로부터의 거리가 증가할수록, 
# 평균적인 가격은 하락한다고 해석할 수 있다. 중간에 증감이 변화하긴 하지만, 큰 틀에서는 해당 해석이 가능하다.

# month의 경우 부호가 음수이다. 따라서 링크를 고려했을 때, 
# 시간이 지남에 따라서 평균적인 가격이 증가하고 있다고 해석할 수 있다. 
# 각각의 month를 11개의 가변수로 고려해주지 않고, 전체적인 추세를 고려해주었기 때문에 과적합을 방지하려 했다.

# lat의 경우 부호가 음수이다. 따라서 링크를 고려했을 때, 위도가 상승함에 따라서 평균적인 가격이 증가하고 있다고 해석할 수 있다.

# 이 해석의 공통적인 부분은, 증감의 방향과 정도를 알 수는 있지만, 정확한 증감치를 제시할 수는 없다는 점과, 
# 다른 변수들이 적합되어 있을 때의 해석이라는 점이다.


## ---- echo = FALSE--------------------------------------------------------------------------------
rm(list = ls())


# 2. Consider the training data in pm25 tr.csv and the test data in pm25 te.csv. 
# Suppose that our interest is to predict pm 2.5 concentration based on some meteorological factors. 
# In the dataset, the output variable is pm25. 
# The training set has data measured from March, 1st to May, 20th and 
# the test set has data measured from May 21st to May 25th (next 5 days).

## -------------------------------------------------------------------------------------------------
train = data.table::fread('pm25_tr-1.csv')

# 2-(1). Using the data modelling techniques (parametric modelling), build your best prediction model from the training data. 
# [NOTE: You might need the transformation of variables or variable selection].

## -------------------------------------------------------------------------------------------------
# Iws의 형태 확인
train %>% head()
plot(train$Iws[1:500], type = 'l', ylab = 'Iws', lwd = 2, ylim = c(0,200))


## -------------------------------------------------------------------------------------------------
# Iws는 누적된 값이므로, 이를 풀어준다.
train = train %>% 
  mutate(Iws_lag = lag(Iws), 
         cbwd_lag = lag(cbwd),
         ws = ifelse(cbwd_lag == cbwd, Iws - Iws_lag, Iws))
train[1, 'ws'] = train[1, 'Iws']  # 첫번째 행이 결측치가 되므로 채워준다.
train = train %>% select(-Iws, -Iws_lag, -cbwd_lag)
train %>% head(3) # 확인했을때 문제 없다.


## -------------------------------------------------------------------------------------------------
# 변환된 값 확인
plot(train$ws[1:500], type = 'l', ylab = 'ws', lwd = 2, ylim = c(0, 200))


## -------------------------------------------------------------------------------------------------
# pm25의 ACF와 PACF 확인
acf2(train$pm25)


## -------------------------------------------------------------------------------------------------
# pm25의 24시점마다 주기, 즉 계절성 확인
plot(train$pm25[1:(24*14)], type = 'l', ylab = 'pm25')
for (i in 1:14) {
  abline(v = 24*i, lty = 3, col = 'blue', lwd = 1)
}


## -------------------------------------------------------------------------------------------------
# train 안에서 validation set 분리
n = train$pm25 %>% length
val = train[(n-119):n, ]
train_tune = train[1:(n-120), ]


## -------------------------------------------------------------------------------------------------
# 가장 기본적인 회귀모델 고려, 회귀 가정의 확인
lm_fit1 = lm(pm25 ~ factor(month) + DEWP + TEMP + PRES + cbwd + ws, data = train_tune)
par(mfrow = c(2,2))
plot(lm_fit1)
par(mfrow = c(1,1))


## -------------------------------------------------------------------------------------------------
# 회귀 가정 검정
shapiro.test(lm_fit1$residuals) # 정규성 기각
car::ncvTest(lm_fit1) # 등분산 기각
car::durbinWatsonTest(lm_fit1, max.lag = 5) # 독립성 기각


## -------------------------------------------------------------------------------------------------
# 정규성과 등분산성을 위한 y 변환
summary(car::powerTransform(train_tune$pm25))


## -------------------------------------------------------------------------------------------------
# 변환된 y에 대한 회귀. 정규성 비교적 완화
train_tune = train_tune %>% mutate(pm25_boxcox = pm25^0.13)
lm_fit2 = lm(pm25_boxcox ~ factor(month) + DEWP + TEMP + PRES + cbwd + ws, data = train_tune)

par(mfrow =c(2,2))
hist(train_tune$pm25, main = 'pm25 - Before BoxCox', breaks = 30)
hist(train_tune$pm25_boxcox, main = 'pm25 - After BoxCox', breaks = 30)
hist(lm_fit1$residuals, main = 'residuals - Before BoxCox', breaks = 30)
hist(lm_fit2$residuals, main = 'residuals - After BoxCox', breaks = 30)
par(mfrow = c(1,1))


## -------------------------------------------------------------------------------------------------
# 정규성은 기각하지만
shapiro.test(lm_fit2$residuals) # 정규성 기각


## -------------------------------------------------------------------------------------------------
# 정규성이 많이 완화되었음이 확인 가능하다.
par(mfrow = c(2,2))
plot(lm_fit2)
par(mfrow = c(1,1))


## -------------------------------------------------------------------------------------------------
# 등분산성은 기각 못하지만, 독립성은 여전히 기각
car::ncvTest(lm_fit2)
car::durbinWatsonTest(lm_fit2, max.lag = 5)


## -------------------------------------------------------------------------------------------------
# 고차항 고려 전에 표준화
DEWP_mean = mean(train_tune$DEWP); DEWP_sd = sd(train_tune$DEWP)
TEMP_mean = mean(train_tune$TEMP); TEMP_sd = sd(train_tune$TEMP)
PRES_mean = mean(train_tune$PRES); PRES_sd = sd(train_tune$PRES)
ws_mean = mean(train_tune$ws); ws_sd = sd(train_tune$ws)

train_tune = train_tune %>% mutate(DEWP = scale(DEWP) %>% as.vector, 
                                   TEMP = scale(TEMP) %>% as.vector, 
                                   PRES = scale(PRES) %>% as.vector, 
                                   ws = scale(ws) %>% as.vector)


## ---- message=F-----------------------------------------------------------------------------------
# gam으로 고차항 차수 고려
gam_fit1 = gam(pm25_boxcox ~ s(DEWP,5) + s(TEMP,5) + s(PRES,5) + s(ws,5) + cbwd, data = train_tune)
par(mfrow = c(2,2))
plot(gam_fit1)
par(mfrow = c(1,1))


## -------------------------------------------------------------------------------------------------
gam_fit2 = gam(pm25_boxcox ~ DEWP + s(TEMP,5) + s(PRES,5) + cbwd + s(ws,5), data = train_tune)

gam_fit3 = gam(pm25_boxcox ~ DEWP + s(TEMP,5) + PRES + cbwd + s(ws,5), data = train_tune)
anova(gam_fit2, gam_fit3)
gam_fit4 = gam(pm25_boxcox ~ DEWP + TEMP + s(PRES,5) + cbwd + s(ws,5), data = train_tune)
anova(gam_fit2, gam_fit4)


## ---- warning=FALSE, message=FALSE----------------------------------------------------------------
# 상관계수 플랏 확인
cor_mat = train_tune %>% select(DEWP, TEMP, PRES, ws) %>% cor
cor_mat[upper.tri(cor_mat)] = NA
melted_cormat = data.table::melt(cor_mat, na.rm=T) 

melted_cormat %>% ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value,2)), size = 5.5) + 
  scale_fill_gradient2('Pearson Corrleation', mid = "lightgrey", low = "blue", high = "red",
                       midpoint = 0, limit = c(-1,1))  +
  labs(x = "", y = "", title = "Correlation Plot")


## -------------------------------------------------------------------------------------------------
# 고차항과 교호작용항 넣은 새로운 회귀식 적합
lm_fit4 = lm(pm25_boxcox ~ factor(month) + DEWP + TEMP + I(TEMP^2) + PRES + I(PRES^2) + cbwd + ws + I(ws^2) + 
               DEWP:TEMP + TEMP:PRES, data = train_tune)

pred_lm_fit4 = predict(lm_fit4, val %>% mutate(DEWP = (DEWP - DEWP_mean)/DEWP_sd, 
                                               TEMP = (TEMP - TEMP_mean)/TEMP_sd, 
                                               PRES = (PRES - PRES_mean)/PRES_sd, 
                                               ws = (ws - ws_mean)/ws_sd)) 
sum(((val$pm25 - pred_lm_fit4^(1/0.13))^2)/length(pred_lm_fit4))  # 2971.757


## -------------------------------------------------------------------------------------------------
# 잔차의 ACF/PACF 확인
acf2(lm_fit4$residuals)


## -------------------------------------------------------------------------------------------------
# 오차의 AR(1) 구조를 직접 최적화를 통해 구현
train_dummy = train_tune %>% mutate(cbwdNE = ifelse(cbwd == 'NE', 1, 0),
                                    cbwdNW = ifelse(cbwd == 'NW', 1, 0),
                                    cbwdSE = ifelse(cbwd == 'SE', 1, 0),
                                    month3 = ifelse(month == 3, 1, 0),
                                    month4 = ifelse(month == 4, 1, 0),
                                    TEMP_quad = TEMP^2, PRES_quad = PRES^2, ws_quad = ws^2,
                                    DEWPxTEMP = DEWP*TEMP, TEMPxPRES = TEMP*PRES) #직접 최적화 윈해 가변수 넣어줌
X = cbind(1,train_dummy %>% select(DEWP, TEMP, TEMP_quad, PRES, PRES_quad, ws, ws_quad, month3, month4,
                                   cbwdNE, cbwdNW, cbwdSE, DEWPxTEMP, TEMPxPRES)) %>% as.matrix()
Y = train_tune$pm25_boxcox
n = length(Y)
S = diag(rep(1,n))    # initial covariance matrix
p = dim(X)[2]

mdif = 100000
beta.old = rep(100000, 15)
I = 0


## -------------------------------------------------------------------------------------------------
# 해를 수치적으로 구하는 과정, 일반화역행렬을 사용한 점이 교수님 코드와 다르다.
while(mdif > 0.00000001) {
  hat_mat = (t(X) %*% solve(S) %*% X) %>% eigen
  beta.new = as.vector((hat_mat$vectors %*% diag(hat_mat$values^(-1), p) %*% t(hat_mat$vectors)) %*%t(X) %*% solve(S) %*% Y)
  # 역행렬을 안정적으로 구할 수 있도록 일반화 역행렬을 넣어주었다.
  r = as.vector(Y - (X %*% beta.new))
  ar1 = sarima (r, 1,0,0, no.constant=T, details=F)
  alpha = ar1$fit$coef
  sigma2 = ar1$fit$sigma2
  
  mdif = max(abs(beta.new - beta.old))
  beta.old = beta.new
  I = I + 1
  print(paste('I is ', I, ' & mdif is ', mdif))
  
  # Construct covariance matrix
  S = matrix(nrow=n,ncol=n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      if (i == j) S[i,j] = 1
      if (i != j) S[i,j] = alpha^(abs(i-j))
    }
  }
  S = (sigma2 / (1-alpha^2)) * S
}


## -------------------------------------------------------------------------------------------------
# 잔차 플랏의 확인
S_eigen = S %>% eigen
residual_vector = (t(Y - (X %*% beta.new)) %*% (S_eigen$vectors %*% diag(S_eigen$values^(-1/2)) %*% t(S_eigen$vectors))) %>% as.vector()
residual_vector %>% plot  


## -------------------------------------------------------------------------------------------------
# durbin watson 통계량을 산출. 독립성 검정은 기각할 값이지만, 1.76으로 2에 꽤나 근접함.
residual_diff = residual_vector[2:length(residual_vector)] - residual_vector[1:(length(residual_vector)-1)]
dw_statistics = sum(residual_diff^2) / sum(residual_vector^2); dw_statistics


## -------------------------------------------------------------------------------------------------
# Prediction by GLS
val_dummy = val %>% mutate(DEWP = (DEWP - DEWP_mean)/DEWP_sd, TEMP = (TEMP - TEMP_mean)/TEMP_sd, 
                           PRES = (PRES - PRES_mean)/PRES_sd, ws = (ws - ws_mean)/ws_sd,
                           month3 = ifelse(month == 3, 1, 0), month4 = ifelse(month == 4, 1, 0),
                           cbwdNE = ifelse(cbwd == 'NE', 1, 0), cbwdNW = ifelse(cbwd == 'NW', 1, 0), cbwdSE = ifelse(cbwd == 'SE', 1, 0),
                           TEMP_quad = TEMP^2, PRES_quad = PRES^2, ws_quad = ws^2,
                           DEWPxTEMP = DEWP*TEMP, TEMPxPRES = TEMP*PRES)
X_val = cbind(1,val_dummy %>% select(DEWP, TEMP, TEMP_quad, PRES, PRES_quad, ws, ws_quad, month3, month4,
                                     cbwdNE, cbwdNW, cbwdSE, DEWPxTEMP, TEMPxPRES)) %>% as.matrix()
y_val = val$pm25
sum((y_val - (X_val %*% beta.new)^(1/0.13))^2)/length(y_val) # 2749

# Prediction by LSE
pred_lm_fit4 = predict(lm_fit4, val %>% mutate(DEWP = (DEWP - DEWP_mean)/DEWP_sd, 
                                               TEMP = (TEMP - TEMP_mean)/TEMP_sd, 
                                               PRES = (PRES - PRES_mean)/PRES_sd, 
                                               ws = (ws - ws_mean)/ws_sd)) 
sum(((val$pm25 - pred_lm_fit4^(1/0.13))^2)/length(pred_lm_fit4))  # 2971.757

###### GLS가 더 좋은 성능이었다. GLS으로 최종적 모델링을 진행하겠다. ######


## -------------------------------------------------------------------------------------------------
# GLS와 LSE의 예측값 비교
plot(val$pm25, type = 'l', lwd = 2, main = 'Comparison for GLS(blue) & LSE(red)', ylab = 'pm25')
lines((X_val %*% beta.new)^(1/0.13), lwd = 2, lty = 2, col = 'blue')  # GLS fit
lines(pred_lm_fit4^(1/0.13), lwd = 2, lty = 2, col = 'red')  # LSE fit


## -------------------------------------------------------------------------------------------------
# 전체 train set에 모델링 진행
DEWP_mean = mean(train$DEWP); DEWP_sd = sd(train$DEWP)
TEMP_mean = mean(train$TEMP); TEMP_sd = sd(train$TEMP)
PRES_mean = mean(train$PRES); PRES_sd = sd(train$PRES)
ws_mean = mean(train$ws); ws_sd = sd(train$ws)

train = train %>% mutate(pm25_boxcox = pm25^0.13,
                         DEWP = scale(DEWP) %>% as.vector, 
                         TEMP = scale(TEMP) %>% as.vector, 
                         PRES = scale(PRES) %>% as.vector, 
                         ws = scale(ws) %>% as.vector)

train_dummy = train %>% mutate(cbwdNE = ifelse(cbwd == 'NE', 1, 0),
                                    cbwdNW = ifelse(cbwd == 'NW', 1, 0),
                                    cbwdSE = ifelse(cbwd == 'SE', 1, 0),
                                    month3 = ifelse(month == 3, 1, 0),
                                    month4 = ifelse(month == 4, 1, 0),
                                    TEMP_quad = TEMP^2, PRES_quad = PRES^2, ws_quad = ws^2,
                                    DEWPxTEMP = DEWP*TEMP, TEMPxPRES = TEMP*PRES) #직접 최적화 윈해 가변수 넣어줌
X = cbind(1,train_dummy %>% select(DEWP, TEMP, TEMP_quad, PRES, PRES_quad, ws, ws_quad, month3, month4,
                                   cbwdNE, cbwdNW, cbwdSE, DEWPxTEMP, TEMPxPRES)) %>% as.matrix()

Y = train$pm25_boxcox
n = length(Y)
S = diag(rep(1,n))    # initial covariance matrix
p = dim(X)[2]

mdif = 100000
beta.old = rep(100000, 15)
I = 0

while(mdif > 0.00000001) {
  hat_mat = (t(X) %*% solve(S) %*% X) %>% eigen
  beta.new = as.vector((hat_mat$vectors %*% diag(hat_mat$values^(-1), p) %*% t(hat_mat$vectors)) %*%t(X) %*% solve(S) %*% Y)
  # 역행렬을 안정적으로 구할 수 있도록 일반화 역행렬을 넣어주었다.
  r = as.vector(Y - (X %*% beta.new))
  ar1 = sarima (r, 1,0,0, no.constant=T, details=F)
  alpha = ar1$fit$coef
  sigma2 = ar1$fit$sigma2
  
  mdif = max(abs(beta.new - beta.old))
  beta.old = beta.new
  I = I + 1
  print(paste('I is ', I, ' & mdif is ', mdif))
  
  # Construct covariance matrix
  S = matrix(nrow=n,ncol=n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      if (i == j) S[i,j] = 1
      if (i != j) S[i,j] = alpha^(abs(i-j))
    }
  }
  S = (sigma2 / (1-alpha^2)) * S
}

#####################################################################################
# 2-(2). Compute the test MSE of your model obtained in part (1) using the test set.


## -------------------------------------------------------------------------------------------------
# test 불러오고, 기존의 전처리 반복
test = data.table::fread('pm25_te-1.csv')

# ws 만들기
test = test %>% 
  mutate(Iws_lag = lag(Iws), 
         cbwd_lag = lag(cbwd),
         ws = ifelse(cbwd_lag == cbwd, Iws - Iws_lag, Iws))
test[1, 'ws'] = 3.13  # train의 마지막 관측치 넣어줌
test = test %>% select(-Iws, -Iws_lag, -cbwd_lag)
test %>% head(4)


## -------------------------------------------------------------------------------------------------
# train에서 만든 모델에 값을 대입해 MSE계산
test_dummy = test %>%mutate(DEWP = (test$DEWP - DEWP_mean)/DEWP_sd, TEMP = (test$TEMP - TEMP_mean)/TEMP_sd, 
                           PRES = (test$PRES - PRES_mean)/PRES_sd, ws = (test$ws - ws_mean)/ws_sd,
                           month3 = ifelse(month == 3, 1, 0), month4 = ifelse(month == 4, 1, 0),
                           cbwdNE = ifelse(cbwd == 'NE', 1, 0), cbwdNW = ifelse(cbwd == 'NW', 1, 0), cbwdSE = ifelse(cbwd == 'SE', 1, 0),
                           TEMP_quad = TEMP^2, PRES_quad = PRES^2, ws_quad = ws^2,
                           DEWPxTEMP = DEWP*TEMP, TEMPxPRES = TEMP*PRES)
X_test = cbind(1,test_dummy %>% select(DEWP, TEMP, TEMP_quad, PRES, PRES_quad, ws, ws_quad, month3, month4,
                                     cbwdNE, cbwdNW, cbwdSE, DEWPxTEMP, TEMPxPRES)) %>% as.matrix()
y_test = test$pm25
sum((y_test - (X_test %*% beta.new)^(1/0.13))^2)/length(y_test) # 2056.816


# Test MSE = # 2056.816


## -------------------------------------------------------------------------------------------------
# 예측값 시각화
plot(test$pm25, type = 'l', lwd = 2, main = 'Test pm25 & prediction', ylab = 'pm25')
lines((X_test %*% beta.new)^(1/0.13), lwd = 2, lty = 2, col = 'red')  # LSE fit

