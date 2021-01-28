## ---- message = FALSE, warning = FALSE--------------------------------------------------------------------------------------
library(mice)
library(rms)
library(finalfit)
library(caret) # create CV
library(car) 
library(bestNormalize) # yeo-johnson transformation
library(gridExtra) # Visualization
library(glmnet)
library(data.table) # data loading
library(tidyverse) # data preprocess


## ---- message = FALSE, warning = FALSE--------------------------------------------------------------------------------------
train = fread('train.csv', data.table = F) 
train %>% dim # 2000행 25열


## ---------------------------------------------------------------------------------------------------------------------------
train = train %>% select(-V1) # 인덱스이므로 삭제, 


## ---- echo = FALSE----------------------------------------------------------------------------------------------------------
# 팩터형 변환
train$X2 = factor(train$X2)
train$X3 = factor(train$X3)
train$X4 = factor(train$X4)
train$X5 = factor(train$X5)
train$X6 = factor(train$X6)
train$X7 = factor(train$X7)
train$X8 = factor(train$X8)
train$X9 = factor(train$X9)
train$X10 = factor(train$X10)
train$X11 = factor(train$X11)


## ---------------------------------------------------------------------------------------------------------------------------
# 컬럼명 부여
colnames(train) = c('credit', 'gender', 'edu', 'martial', 'age', 
                    'april_pay_rec', 'may_pay_rec', 'june_pay_rec', 'july_pay_rec', 'aug_pay_rec', 'sep_pay_rec',
                    'april_state', 'may_state', 'june_state', 'july_state', 'aug_state', 'sep_state', 
                    'april_pay_pre', 'may_pay_pre', 'june_pay_pre', 'july_pay_pre', 'aug_pay_pre', 'sep_pay_pre',
                    'default')
train_colnames = colnames(train)


## ---- eval = F--------------------------------------------------------------------------------------------------------------
## 
md.pattern(train)
# 특별한 패턴 파악 불가

## ---- eval = F--------------------------------------------------------------------------------------------------------------
## 
md.pairs(train)$mm
# 특별한 패턴 파악 불가


## ---------------------------------------------------------------------------------------------------------------------------
missing.clus = naclus(train %>% select(-credit, -default), method = 'average')
plot(missing.clus)
# 특별한 패턴 파악 불가
# 최종적으로 Imputation을 해도 된다는 결론


## ---------------------------------------------------------------------------------------------------------------------------
# impute할 대상 지정, Y와 X1은 제외. MICE 시행
impute_vec = c('', rep('cart', 22), '')

imputation_pred_mat = matrix(1, ncol = length(train_colnames), nrow = length(train_colnames))
rownames(imputation_pred_mat) = colnames(imputation_pred_mat) = train_colnames
diag(imputation_pred_mat) = 0
imputation_pred_mat[, 'default'] = 0

set.seed(42)
mult_imp = mice(train, m = 5, method = impute_vec, predictorMatrix = imputation_pred_mat, print = F, seed = 42)


## ---------------------------------------------------------------------------------------------------------------------------
# 잘 수렴했음
plot(mult_imp, c('gender', 'edu', 'martial'))
plot(mult_imp, c('age', 'april_pay_rec', 'may_pay_rec'))
plot(mult_imp, c('april_state', 'april_pay_pre', 'may_pay_rec'))


## ---------------------------------------------------------------------------------------------------------------------------
train_imputed = mult_imp %>% complete


## ---------------------------------------------------------------------------------------------------------------------------
# education과 martail 변수 확인
plot_edu = train_imputed %>% ggplot(aes(x = edu, fill = factor(default))) + geom_bar(position = 'dodge')
plot_martial = train_imputed %>% ggplot(aes(x = martial, fill = factor(default))) + geom_bar(position = 'dodge')
grid.arrange(plot_edu, plot_martial, nrow = 2)


## ---------------------------------------------------------------------------------------------------------------------------
# 재범주화
train_imputed$edu[which(train_imputed$edu %in% c(5, 6))] = 4
train_imputed$edu = train_imputed$edu %>% factor
train_imputed$martial[which(train_imputed$martial == 0)] = 3
train_imputed$martial = train_imputed$martial %>% factor


## ---------------------------------------------------------------------------------------------------------------------------
# payment record 변수 재범주화
train_imputed$april_pay_rec = ifelse(as.numeric(as.character(train_imputed$april_pay_rec)) < 0.1, 0, 1) 
train_imputed$may_pay_rec = ifelse(as.numeric(as.character(train_imputed$may_pay_rec)) < 0.1, 0, 1) 
train_imputed$june_pay_rec = ifelse(as.numeric(as.character(train_imputed$june_pay_rec)) < 0.1, 0, 1) 
train_imputed$july_pay_rec = ifelse(as.numeric(as.character(train_imputed$july_pay_rec)) < 0.1, 0, 1) 
train_imputed$aug_pay_rec = ifelse(as.numeric(as.character(train_imputed$aug_pay_rec)) < 0.1, 0, 1) 
train_imputed$sep_pay_rec = ifelse(as.numeric(as.character(train_imputed$sep_pay_rec)) < 0.1, 0, 1) 

train_imputed$num_late = train_imputed$april_pay_rec + train_imputed$may_pay_rec + 
  train_imputed$june_pay_rec + train_imputed$july_pay_rec + 
  train_imputed$aug_pay_rec + train_imputed$sep_pay_rec
train_imputed$num_late = factor(train_imputed$num_late)

train_imputed = train_imputed %>% select(-april_pay_rec, -may_pay_rec, -june_pay_rec, -july_pay_rec, -aug_pay_rec, -sep_pay_rec)


## ---------------------------------------------------------------------------------------------------------------------------
# age 변수 재범주화
train_imputed$age = train_imputed$age %>% as.character() %>% as.numeric()
train_imputed$age = ifelse(train_imputed$age < 29.5, '20', 
                                 ifelse(train_imputed$age < 39.5, '30', 
                                        ifelse(train_imputed$age < 49.5, '40', 
                                               ifelse(train_imputed$age < 59.5, '50', 'over 60')))) %>% as.factor


## ---- echo = FALSE----------------------------------------------------------------------------------------------------------
# yeo-johnson transformation, pca와 고차항 추가 고려
train_imputed_scale = train_imputed
train_imputed_scale$credit = yeojohnson(train_imputed_scale$credit)$x.t
train_imputed_scale$april_state = yeojohnson(train_imputed_scale$april_state)$x.t
train_imputed_scale$may_state = yeojohnson(train_imputed_scale$may_state)$x.t
train_imputed_scale$june_state = yeojohnson(train_imputed_scale$june_state)$x.t
train_imputed_scale$july_state = yeojohnson(train_imputed_scale$july_state)$x.t
train_imputed_scale$aug_state = yeojohnson(train_imputed_scale$aug_state)$x.t
train_imputed_scale$sep_state = yeojohnson(train_imputed_scale$sep_state)$x.t
train_imputed_scale$april_pay_pre = yeojohnson(train_imputed_scale$april_pay_pre)$x.t
train_imputed_scale$may_pay_pre = yeojohnson(train_imputed_scale$may_pay_pre)$x.t
train_imputed_scale$june_pay_pre = yeojohnson(train_imputed_scale$june_pay_pre)$x.t
train_imputed_scale$july_pay_pre = yeojohnson(train_imputed_scale$july_pay_pre)$x.t
train_imputed_scale$aug_pay_pre = yeojohnson(train_imputed_scale$aug_pay_pre)$x.t
train_imputed_scale$sep_pay_pre = yeojohnson(train_imputed_scale$sep_pay_pre)$x.t


## ---------------------------------------------------------------------------------------------------------------------------
# 상관계수 플랏
cor(train_imputed[, c(1, 6:17)]) %>% corrplot::corrplot(method = 'circle')


## ---------------------------------------------------------------------------------------------------------------------------
# for bill statement variables
pr = prcomp(train_imputed_scale[, c(6:11)])
screeplot(pr, type = 'l', npcs = 6, main = 'scree plot for bill statement variables')


## ---------------------------------------------------------------------------------------------------------------------------
# PC1 변수 사용
train_imputed_scale$state_pc = pr$x[,'PC1']
train_imputed_scale = train_imputed_scale %>% select(-april_state, -may_state, -june_state, -july_state, -aug_state, -sep_state)


## ---------------------------------------------------------------------------------------------------------------------------
# for previous payment variables
pr2 =  prcomp(train_imputed_scale[, c(6:11)])
screeplot(pr2, type = 'l', npcs = 6, main = 'scree plot for previous payment variables')


## ---------------------------------------------------------------------------------------------------------------------------
# PC1 변수 사용
train_imputed_scale$pay_pre_pc = pr2$x[,'PC1']
train_imputed_scale = train_imputed_scale %>% select(-april_pay_pre, -may_pay_pre, -june_pay_pre, 
                                                     -july_pay_pre, -aug_pay_pre, -sep_pay_pre)

## ---------------------------------------------------------------------------------------------------------------------------
# 최종 변수 확인
colnames(train_imputed_scale)


## ---------------------------------------------------------------------------------------------------------------------------
# CV 지정, 튜닝과정 생략
set.seed(42)
fold_index = createFolds(train_imputed_scale$default, k = 5)


## ---------------------------------------------------------------------------------------------------------------------------
# ridge fit
ridge_fit = glmnet(x = model.matrix(default ~ ., data = train_imputed_scale)[,-1], 
                   y = train_imputed_scale$default, family = 'binomial', alpha = 0, lambda = 0.02212)


## ---- echo = FALSE, warning = FALSE, message = FALSE------------------------------------------------------------------------
# test data에 같은 전처리
test = fread('test.csv', data.table = F) 
test = test %>% select(-V1)

colnames(test) = train_colnames

test$edu[which(test$edu %in% c(5, 6))] = 4
test$edu = test$edu %>% factor
test$martial[which(test$martial == 0)] = 3
test$martial = test$martial %>% factor

test$april_pay_rec = ifelse(as.numeric(as.character(test$april_pay_rec)) < 0.1, 0, 1) 
test$may_pay_rec = ifelse(as.numeric(as.character(test$may_pay_rec)) < 0.1, 0, 1) 
test$june_pay_rec = ifelse(as.numeric(as.character(test$june_pay_rec)) < 0.1, 0, 1) 
test$july_pay_rec = ifelse(as.numeric(as.character(test$july_pay_rec)) < 0.1, 0, 1) 
test$aug_pay_rec = ifelse(as.numeric(as.character(test$aug_pay_rec)) < 0.1, 0, 1) 
test$sep_pay_rec = ifelse(as.numeric(as.character(test$sep_pay_rec)) < 0.1, 0, 1) 


test$num_late = test$april_pay_rec + test$may_pay_rec + 
  test$june_pay_rec + test$july_pay_rec + 
  test$aug_pay_rec + test$sep_pay_rec
test$num_late = factor(test$num_late)
test = test %>% select(-april_pay_rec, -may_pay_rec, -june_pay_rec, -july_pay_rec, -aug_pay_rec, -sep_pay_rec)


test$credit = yeojohnson(test$credit)$x.t
test$april_state = yeojohnson(test$april_state)$x.t
test$may_state = yeojohnson(test$may_state)$x.t
test$june_state = yeojohnson(test$june_state)$x.t
test$july_state = yeojohnson(test$july_state)$x.t
test$aug_state = yeojohnson(test$aug_state)$x.t
test$sep_state = yeojohnson(test$sep_state)$x.t
test$april_pay_pre = yeojohnson(test$april_pay_pre)$x.t
test$may_pay_pre = yeojohnson(test$may_pay_pre)$x.t
test$june_pay_pre = yeojohnson(test$june_pay_pre)$x.t
test$july_pay_pre = yeojohnson(test$july_pay_pre)$x.t
test$aug_pay_pre = yeojohnson(test$aug_pay_pre)$x.t
test$sep_pay_pre = yeojohnson(test$sep_pay_pre)$x.t

test = test %>% mutate(state_pc = predict(pr, test[6:11])[, 1]) %>% 
  select(-april_state, -may_state, -june_state, -july_state, -aug_state, -sep_state)

test$age = ifelse(test$age < 29.5, '20', 
                  ifelse(test$age < 39.5, '30', 
                         ifelse(test$age < 49.5, '40', 
                                ifelse(test$age < 59.5, '50', 'over 60')))) %>% as.factor

test = test %>% mutate(pay_pre_pc = predict(pr2, test[6:11])[, 1]) %>% 
  select(-april_pay_pre, -may_pay_pre, -june_pay_pre, -july_pay_pre, -aug_pay_pre, -sep_pay_pre)


## ---------------------------------------------------------------------------------------------------------------------------
# 예측
pred_y = predict(ridge_fit, newx = model.matrix(default ~ ., 
                                                data = test)[,-1], type = 'response')
pred_y = ifelse(pred_y < 0.5, 0, 1)
sum(abs(test$default - as.numeric(as.character(pred_y)))) / length(pred_y)

# 최종 오분류율 0.2986
