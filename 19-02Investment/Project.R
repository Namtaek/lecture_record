# Presentation

# We use these R packages.
library(ggcorrplot)  # visualizing correlation matrix
library(httr)        # data crawling
library(rvest)       # data crawling
library(tidyquant)   # stock price data analysis
library(tidyverse)   # data preprocessing


KOR_price = read.csv('data/KOR_price.csv',
                     stringsAsFactors = FALSE)
dim(KOR_price) # daily return for 2076 stocks
colnames(KOR_price)[1] <- 'date'
KOR_price <-  # process to make montly return
  KOR_price %>% 
  separate(date, into = c('year', 'month', 'day'), sep = '-') %>%  
  unite(year_month, year, month, sep = '-') %>% 
  group_by(year_month) %>% 
  filter(day == max(day)) %>%   # only take monthly return
  separate(year_month, into = c('year', 'month'), sep = '-') %>%   # preprocess y-m-d form
  unite(date, year, month, day, sep = '-') %>%
  column_to_rownames(var = 'date')
KOR_price <- KOR_price[-dim(KOR_price)[1], ]  # last row is 2019-11-01, so remove

# make monthly return matrix
return_mat = Return.calculate(KOR_price)
return_mat <- return_mat[-1, ] # first row NA generate
# write.csv(return_mat, 'data/return_matrix_2076stocks.csv') # make excel file


# I want to make SML(regression model),
# r_i - r_f = alpha_i + beta_i * (r_m - r_f) + epsilon_i


symbols = c('102110.KS')  # take kospi 200 ETF
getSymbols(symbols)
market_prices = do.call(cbind,
                 lapply(symbols, function(x)Cl(get(x)))) # make price data frame
market_prices <- market_prices['2017-10::2019-10'] # required period
colnames(market_prices) <- 'Kospi200_ETF' # change column name
market_prices <- market_prices[complete.cases(market_prices)] # remove NA
market_prices <- as.data.frame(market_prices)  

market_prices <- 
  market_prices %>% 
  mutate(date = row.names(market_prices)) %>% 
  separate(date, into = c('year', 'month', 'day'), sep = '-') %>%   # i want to select ends day of each month
  unite(year_month, year, month, sep = '-') %>% 
  group_by(year_month) %>% 
  filter(day == max(day)) %>%   # closing prices of ends of month
  separate(year_month, into = c('year', 'month'), sep = '-') %>%   #preprocessing as y-m-d
  unite(date, year, month, day, sep = '-') %>%
  column_to_rownames(var = 'date')

market_return <- Return.calculate(market_prices) # calculate returns
market_return <- market_return[-1, ] # first row is NA, so remove it 
mean(market_return) # -0.0068
sd(market_return) # 0.0453
names(market_return) <- rownames(return_mat)
market_return <- market_return %>% round(4)
# write.csv(market_return, 'data/market_return.csv')


# make CD values for monthly return
CD = read.csv('data/CD_Monthly.csv', row.names = 1) %>% as.vector
CD
monthly_CD <- CD / 12
monthly_CD
# write.csv(monthly_CD, 'data/monthly_CD.csv')
mean(monthly_CD[,1]) # 0.00141

market_premium <- market_return - monthly_CD # calculate market risk premium
return_mat <- return_mat[, complete.cases(t(return_mat))] 
# some column has NA, maybe before published. For regression, remove NA.
dim(return_mat) # it has 1930 column (1930 stocks), 24 rows.

# find risk premium matirx of each stocks
risk_premium <- matrix(0, nrow = 24, ncol = 1930)
for (i in 1:1930) {
  for (j in 1:24) {
    risk_premium[j, i] <- return_mat[j, i] - monthly_CD[j, 1]
  }
}
colnames(risk_premium) <- colnames(return_mat)
row.names(risk_premium) <- row.names(return_mat)
risk_premium

# write.csv(risk_premium, 'data/risk_premium.csv')
# write.csv(market_premium, 'data/market_premium.csv')

# find overpriced stocks
index_overpriced <- rep(0, 1930) 
alpha_overpriced <- rep(0, 1930)
for (i in 1:1930) {
  each_premium <- as.vector(risk_premium[, i])
  fit <- lm(each_premium ~ market_premium[, 1])
  summary_fit <- summary(fit)
  if ((summary_fit$coefficients[1,1] < 0) & 
      (summary_fit$coefficients[1, 4] < 0.05) &
      (summary_fit$fstatistic[1] > qf(0.95, 1, 22))) {
    index_overpriced[i] <- 1 # find which alpha is smaller than zero statistically.
    alpha_overpriced[i] <- summary_fit$coefficients[1,1]
  } else {
    index_overpriced[i] <- 0
    alpha_overpriced[i] <- 0
  }
}
table(index_overpriced)
# 14 stocks are overpriced

overpriced <- which(index_overpriced == 1)  # to find whick stock is overpriced.

overpriced_names <-  # stock code
  colnames(return_mat[, overpriced]) %>% 
  str_remove('X') 

# select overpriced stocks, and arrange it

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
colnames(KOR_ticker)[c(2, 3, 4)] <- c('Code', 'Name', 'Industry')
colnames(KOR_value)[5] <- 'Code'

KOR_ticker$'Code' =
  str_pad(KOR_ticker$'Code', 6, 'left', 0)

overpriced_stock_capm <- 
  KOR_ticker %>% 
  filter(Code %in% overpriced_names) %>% 
  select(Code, Name, Industry) %>% 
  mutate(alpha = round(alpha_overpriced[overpriced], 3))

overpriced_stock_capm
overpriced_stock_capm$Name <-
  c('Samsung Life Insurance', 'Hanwha Life Insurance', 'Tong Yang Life Insurance Co.', 'DaeKyo',
    'Samyang Corp', 'Seah Holdings', 'Kolon', 'Jeju Bank', 'Dongguk S&C', 'STX Heavy Industries Co.',
    'Hansae MK', 'Charm Engeneering', 'Heung-A', 'Woongjin Energy')
overpriced_stock_capm$Industry <- 
  c('Finance', 'Finance', 'Finance', 'Service', 'Food & Beverage', 'Steel Metal', 'Finance', 'Finance',
    'Metal', 'Machine', 'Retail', 'Machine', 'Transportation Warehouse', 'Electronic')

# write.csv(overpriced_stock_capm, 'data/Overpriced_stock_capm.csv')


# find underpriced stock
index_underpriced <- rep(0, 1930) 
alpha_underpriced <- rep(0, 1930) 
beta_underpriced <- rep(0, 1930) 
for (i in 1:1930) {
  each_premium <- as.vector(risk_premium[, i])
  fit <- lm(each_premium ~ market_premium[, 1])
  summary_fit <- summary(fit)
  if ((summary_fit$coefficients[1, 3] > qt(0.975, 22)) & 
      (summary_fit$coefficients[1,1] > 0) &
      (summary_fit$fstatistic[1] > qf(0.95, 1, 22))) {
    index_underpriced[i] <- 1 
    # find which alpha is larger than zero with respect to hypothesis testing
    alpha_underpriced[i] <- summary_fit$coefficients[1,1]
    beta_underpriced[i] <- summary_fit$coefficients[2,1]
  } else {
    index_underpriced[i] <- 0
    beta_underpriced[i] <- 0
  }
}
table(index_underpriced) # 7 stocks are underpriced

underpriced <- which(index_underpriced == 1)  # to find whick stock is underpriced.
underpriced_names <-  # stock code
  colnames(return_mat[, underpriced]) %>% 
  str_remove('X') 

# select underpriced stocks, and arrange it
KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
colnames(KOR_ticker)[c(2, 3, 4)] <- c('Code', 'Name', 'Industry')
colnames(KOR_value)[5] <- 'Code'

KOR_ticker$'Code' =
  str_pad(KOR_ticker$'Code', 6, 'left', 0)

underpriced_stock_capm <- 
  KOR_ticker %>% 
  filter(Code %in% underpriced_names) %>% 
  select(Code, Name, Industry) 
underpriced_stock_capm <-  # combine Value matrix
  underpriced_stock_capm %>% 
  mutate(alpha = round(alpha_underpriced[underpriced], 3),
         beta = round(beta_underpriced[underpriced], 3)) %>% 
  inner_join(KOR_value, by = c('Code', 'Code')) %>% 
  arrange(Code)
underpriced_stock_capm$Name <- 
  c('THN', 'PC Direct', 'CS', 'S&S Tech', 'DY P&F', 'FineBeSteel', 'Megastudy Edu')
underpriced_stock_capm$Industry <- 
  c('Electronic', 'Retail', 'IT H/W', 'IT H/w', 'Machine & Equipment', 'Steel Metal', 'Services')
underpriced_stock_capm <- 
  underpriced_stock_capm %>% 
  mutate(PER = PER %>% round(3),
         PBR = PBR %>% round(3),
         PCR = PCR %>% round(3),
         PSR = PSR %>% round(3))
  
# write.csv(underpriced_stock_capm, 'data/Underpriced_stock_capm.csv')


# SML of S&S Tech

each_premium <- as.vector(risk_premium[, 635])
fit <- lm(each_premium ~ market_premium[, 1])
summary_fit <- summary(fit)
summary_fit

design_matrix <- cbind(each_premium, market_premium[ , 1]) %>% as.data.frame()
# V2 is market risk premium
ggplot(data = design_matrix, mapping = aes(x = V2, y = each_premium)) +
  geom_smooth(method = 'lm', se = F, lwd = 2) +
  geom_point(aes(col = 'red'), size = 3) +
  labs(title = 'SML of S&S Tech',
       x = 'Market Risk Premium',
       y = 'Risk Premium of S&S Tech') +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlim(-0.1, 0.1) + 
  ylim(-0.2, 0.3)


# take value

# There are NAs in PER and PCR. 
# It means those companies didn't make money or start-up company.
# Rather than remove companies that have NA values, 
# We want to use informations of values as much as possible.
# So, We didn't will use PBR and PSR values to find non-overpriced companies.

# We also use beta coefficient to find companies which are too high variability.

# We will filter the stocks which have non-small price ratio value and large beta coefficient.

Value_matrix <- underpriced_stock_capm[, complete.cases(t(underpriced_stock_capm))]
chosen_stock <- 
  Value_matrix %>% 
  filter(beta <= 2,
         PBR <= 2,
         PSR <= 2)
chosen_stock[,'Name']
chosen_stock
# 3 stocks (THN, PC Direct and FineBeSteel) are chosen.

# 3 stocks' return
underpriced_capm_return <- return_mat[, underpriced]
colnames(underpriced_capm_return) <- 
  colnames(underpriced_capm_return) %>% 
  str_remove('X')
colnames(underpriced_capm_return) <- 
  c('Megastudy Edu', 'S&S Tech', 'DY P&F', 'FineBeSteel', 'THN', 'CS', 'PC Direct')
three_stock_return <- underpriced_capm_return[, chosen_stock[, 'Name']] %>% round(4)
three_stock_return
# write.csv(three_stock_return, 'data/three_stock_return.csv')


# We want to identify correlation of each stocks.
# We don't want highly correlated stock, 
# because they can't make well-diversified portfolio and 
# three stocks' portfolio is relatively hard to interpret than two stocks' portfolio.

# calculate expected return
expected_return <- apply(three_stock_return, 2, mean, na.rm = T) %>% round(6)
expected_return

# covariance matrix
cov_mat <- cov(three_stock_return)
# correlation matrix
cor_mat <- cor(three_stock_return)

# Visualiztion for correlation matrix
ggcorrplot(cor_mat, 
           hc.order = T, 
           type = "lower", 
           outline.color = "white", 
           ggtheme = ggplot2::theme_gray(), 
           colors = c("#6D9EC1", "white", "#E46726"), 
           lab = T, 
           insig = "blank", lab_size = 7, 
           tl.cex = 16, tl.srt = 35)
# FineBeSteel has large correlations for each stocks, so remove FineBeSteel.



# SML of THN and PC Direct

two_stock_return <- three_stock_return[, c('THN', 'PC Direct')] 
# write.csv(two_stock_return, 'data/final_stock_return.csv')
THN <- two_stock_return[, 1]
THN_premium <- THN - monthly_CD[,1]
PCDirect <- two_stock_return[, 2]
PCDirect_premium <- PCDirect - monthly_CD[,1]

ggplot(mapping = aes(x = market_premium[,1], y = THN_premium)) +
  geom_smooth(method = 'lm', se = F, lwd = 2) +
  geom_point(aes(col = 'red'), size = 3) +
  labs(title = 'SML of THN',
       x = 'Market Risk Premium',
       y = 'Risk Premium of THN') +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlim(-0.1, 0.1) + 
  ylim(-0.2, 0.3)

ggplot(mapping = aes(x = market_premium[,1], y = PCDirect_premium)) +
  geom_smooth(method = 'lm', se = F, lwd = 2) +
  geom_point(aes(col = 'red'), size = 3) +
  labs(title = 'SML of PC Direct',
       x = 'Market Risk Premium',
       y = 'Risk Premium of PC Direct') +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlim(-0.1, 0.1) + 
  ylim(-0.2, 0.3)


THN_mean <- THN %>% mean %>% round(4)  # 0.0426
PCDirect_mean <- PCDirect %>% mean %>% round(4) # 0.0252
THN_risk <- sd(THN) %>% round(4)  # 0.1326
PCDirect_risk <- sd(PCDirect) %>% round(4)  # 0.0641
stock_cor <- cor(THN, PCDirect) %>% round(3) # 0.287
rf <- mean(monthly_CD[, 1]) %>% round(4) # 0.0014

v1 <- THN_mean - rf
v2 <- PCDirect_mean - rf

# find weight of tangent portfolio
opt_w <- ((v1*(PCDirect_risk^2) - (v2 * stock_cor * THN_risk * PCDirect_risk))) / 
  (v1 * (PCDirect_risk^2) + v2 * (THN_risk^2) - (v1 + v2) * (stock_cor * THN_risk * PCDirect_risk))
opt_w <- 0.259

# tangent portforlio for 2 stocks
w = seq(0, 1, len = 100)
means = w * THN_mean + (1 - w) * PCDirect_mean
var = THN_risk^2 * w^2 + PCDirect_risk^2 * (1 - w)^2 + 
  2 * w * (1 - w) * THN_risk * PCDirect_risk * stock_cor
risk = sqrt(var)
ind = !(risk > min(risk)) # to find minimum variance portfolio
ind2 = (means > means[ind]) # to find efficient frontier

# return and risk of tangent portfolio
meant = opt_w * THN_mean + (1 - opt_w) * PCDirect_mean # 0.0297
riskt = sqrt(THN_risk^2 * opt_w^2 + PCDirect_risk^2 * (1 - opt_w)^2 + 
  2 * opt_w * (1 - opt_w) * THN_risk * PCDirect_risk * stock_cor) # 0.0661

sharpe_ratio <- (meant - rf) / riskt   # 0.428

# weight of minimum variance portfolio is 0.100
min_w = 0.100
meanp = min_w * THN_mean + (1 - min_w) * PCDirect_mean  # 0.0264
riskp = sqrt(THN_risk^2 * min_w^2 + PCDirect_risk^2 * (1 - min_w)^2 + 
  2 * min_w * (1 - min_w) * THN_risk * PCDirect_risk * stock_cor)  # 0.0627

data <- data.frame(means, risk)

# Visualization for tangent portfolio

plot1 <- 
  ggplot(data = data, aes(x = risk, y = means)) + 
  # portfolio lines for each weights
  geom_point(size = 1.5) + 
  labs(title = 'Tangent Portfolio for Two Stocks',
       subtitle = 'THN and PC Direct, Weight = 0.259') + 
  geom_abline(intercept = rf, slope = sharpe_ratio, lwd = 2, col = 'blue') +
  xlim(0, 0.15) + 
  ylim(0, 0.05) +
  annotate('text', x = THN_risk, y = 0.047, 
           label = 'bold(THN)', parse = TRUE, size = 5) +
  annotate('text', x = PCDirect_risk, y = PCDirect_mean - 0.003, 
           label = "bold('PC Direct')", parse = TRUE, size = 5) +
  geom_point(aes(x = riskt, y = meant), col = 'red', size = 5) +
  geom_point(aes(x = 0, y = rf), col = 'red', size = 4) +
  geom_point(aes(x = riskp, y = meanp), col = 'green', size = 4)
  
plot1


