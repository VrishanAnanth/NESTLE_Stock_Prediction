##Daily
## PART 1 ##
library(readxl)
library(quantmod)
library(forecast)

## Data Collection ##
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2022-03-31", verbose = FALSE, auto.assign = FALSE)
NSE <- na.omit(NSE)

NESTLE <- getSymbols.yahoo("NESTLEIND.NS", from = "2020-04-01", to = "2022-03-31", verbose = FALSE, auto.assign = FALSE)
NESTLE <- na.omit(NESTLE)

NTPC <- getSymbols.yahoo("NTPC.NS", from = "2020-04-01", to = "2022-03-31", verbose = FALSE, auto.assign = FALSE)
NTPC <- na.omit(NTPC)

## Close Prices Data ##
close1 <- cbind(NSE$NSEI.Close, NESTLE$NESTLEIND.NS.Close)
close2 <- cbind(NSE$NSEI.Close, NTPC$NTPC.NS.Close)

## Data Manipulation ##
D_T_Bill <- read_excel("C:/Users/haris/Desktop/FIN F414/Assignment/Draft/D_T-Bills.xlsx")
D_T_Bill <- as.data.frame(D_T_Bill)
D_T_Billxts <- xts(D_T_Bill[,-1], order.by = as.Date(D_T_Bill$Date))
colnames(D_T_Billxts) <- c('values')


## Return Calculation ##
returns1 <- as.xts(tail(data.frame(close1),-1)/head(data.frame(close1),-1) -1)
returns1 <- data.frame(returns1[])
returns1$excessreturn = c(returns1$NSEI.Close - D_T_Billxts$values)
returns1$excessstockreturn1d=c(returns1$NESTLEIND.NS.Close-D_T_Billxts$values)
head(returns1)

returns2 <- as.xts(tail(data.frame(close2),-1)/head(data.frame(close2),-1) -1)
returns2 <- data.frame(returns2[])
returns2$excessreturn = c(returns2$NSEI.Close - D_T_Billxts$values)
returns2$excessstockreturn2d=c(returns2$NTPC.NS.Close-D_T_Billxts$values)
head(returns2)


## Regression Model ##
regression1 <- lm(excessstockreturn1d ~ excessreturn, data.frame(returns1[]))
summary(regression1)


regression2 <- lm(excessstockreturn2d ~ excessreturn, data.frame(returns2[]))
summary(regression2)


## PART 2 ##

library(tseries)

## Return Calculation ##
returns_NESTLE <- as.xts(tail(data.frame(NESTLE$NESTLEIND.NS.Close),-1)/head(data.frame(NESTLE$NESTLEIND.NS.Close),-1)-1, frequency = 365)
returns_NESTLE <- na.omit(returns_NESTLE)
returns_NTPC <- as.xts(tail(data.frame(NTPC$NTPC.NS.Close),-1)/head(data.frame(NTPC$NTPC.NS.Close),-1)-1, frequency = 365)
returns_NTPC <- na.omit(returns_NTPC)

## Data Manipulation ##
colnames(returns_NESTLE) <- "returns_NESTLE"
colnames(returns_NTPC) <- "returns_NTPC"
mean(returns_NESTLE)
var(returns_NESTLE)
mean(returns_NTPC)
var(returns_NTPC)

## Data Visualization ##
plot(NESTLE$NESTLEIND.NS.Close)
plot(returns_NESTLE)

plot(NTPC$NTPC.NS.Close)
plot(returns_NTPC)

## model identification AR & MA ##
adf.test(returns_NESTLE, alternative = "stationary")
plot(acf(returns_NESTLE , lag.max = 10))
plot(pacf(returns_NESTLE , lag.max = 10))
auto.arima(returns_NESTLE)
auto.arima(returns_NESTLE,ic = "bic")
arima_final1 <- arima(returns_NESTLE, order= c(3,0,1))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)

adf.test(returns_NTPC, alternative = "stationary")
plot(acf(returns_NTPC , lag.max = 10))
plot(pacf(returns_NTPC , lag.max = 10))
auto.arima(returns_NTPC)
auto.arima(returns_NTPC,ic = "bic")
arima_final2 <- arima(returns_NTPC, order= c(0,0,0))
arima_final2
predicted <- predict(arima_final2, n.ahead = 10)
predicted
tsdiag(arima_final2)

## PART 3 ##

library(quantmod)
library(rugarch)
library(rmgarch)

## Data Collection ##
NESTLE1 <- getSymbols("NESTLEIND.NS", from = "2020-04-01", to = "2022-03-31")
NESTLE1 <- na.omit(NESTLE1)

NTPC1 <- getSymbols("NTPC.NS", from = "2019-03-01", to = "2021-02-28")
NTPC1 <- na.omit(NTPC1)

## Return Calculation ##
R.NESTLE <- dailyReturn(NESTLEIND.NS)
R.NTPC <- dailyReturn(NTPC.NS)

## Implementing Univariate GARCH ##
ug_spec = ugarchspec()
ug_spec

## Implementing EGARCH ##
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = R.NESTLE) 
ugfit1

ugfit2 = ugarchfit(spec = ug_spec, data = R.NTPC) 
ugfit2

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1

ugforecast2 = ugarchforecast(ugfit2, n.ahead=10) 
ugforecast2

##Weekly
## PART 1 ##
library(readxl)
library(quantmod)
library(forecast)

## Data Collection ##
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "weekly")
NSE <- na.omit(NSE)

NESTLE <- getSymbols.yahoo("NESTLEIND.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "weekly")
NESTLE <- na.omit(NESTLE)

NTPC <- getSymbols.yahoo("NTPC.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "weekly")
NTPC <- na.omit(NTPC)

## Close Prices Data ##
close1 <- cbind(NSE$NSEI.Close, NESTLE$NESTLEIND.NS.Close)
close2 <- cbind(NSE$NSEI.Close, NTPC$NTPC.NS.Close)

## Data Manipulation ##

W_T_Bill <- read_excel("C:/Users/haris/Desktop/FIN F414/Assignment/Draft/W_T-Bills.xlsx")
W_T_Bill <- as.data.frame(W_T_Bill)
W_T_Billxts <- xts(W_T_Bill[,-1], order.by = as.Date(W_T_Bill$Date))
colnames(W_T_Billxts) <- c('values')

## Return Calculation ##
returns1 <- as.xts(tail(data.frame(close1),-1)/head(data.frame(close1),-1) -1)
returns1 <- data.frame(returns1[])
returns1$excessreturn = c(returns1$NSEI.Close - W_T_Billxts$values)
returns1$excessstockreturn1w=c(returns1$NESTLEIND.NS.Close-W_T_Billxts$values)
head(returns1)

returns2 <- as.xts(tail(data.frame(close2),-1)/head(data.frame(close2),-1) -1)
returns2 <- data.frame(returns2[])
returns2$excessreturn = c(returns2$NSEI.Close - W_T_Billxts$values)
returns2$excessstockreturn2w=c(returns2$NTPC.NS.Close-W_T_Billxts$values)
head(returns2)


## Regression Model ##
regression1 <- lm(excessstockreturn1w ~ excessreturn, data.frame(returns1[]))
summary(regression1)


regression2 <- lm(excessstockreturn2w ~ excessreturn, data.frame(returns2[]))
summary(regression2)


## PART 2 ##

library(tseries)

## Return Calculation ##
returns_NESTLE <- as.xts(tail(data.frame(NESTLE$NESTLEIND.NS.Close),-1)/head(data.frame(NESTLE$NESTLEIND.NS.Close),-1)-1, frequency = 52)
returns_NESTLE <- na.omit(returns_NESTLE)
returns_NTPC <- as.xts(tail(data.frame(NTPC$NTPC.NS.Close),-1)/head(data.frame(NTPC$NTPC.NS.Close),-1)-1, frequency = 52)
returns_NTPC <- na.omit(returns_NTPC)

## Data Manipulation ##
colnames(returns_NESTLE) <- "returns_NESTLE"
colnames(returns_NTPC) <- "returns_NTPC"
mean(returns_NESTLE)
var(returns_NESTLE)
mean(returns_NTPC)
var(returns_NTPC)

## Data Visualization ##
plot(NESTLE$NESTLEIND.NS.Close)
plot(returns_NESTLE)

plot(NTPC$NTPC.NS.Close)
plot(returns_NTPC)

## model identification AR & MA ##
adf.test(returns_NESTLE, alternative = "stationary")
plot(acf(returns_NESTLE , lag.max = 10))
plot(pacf(returns_NESTLE , lag.max = 10))
auto.arima(returns_NESTLE)
auto.arima(returns_NESTLE,ic = "bic")
arima_final1 <- arima(returns_NESTLE, order= c(0,0,0))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)

adf.test(returns_NTPC, alternative = "stationary")
plot(acf(returns_NTPC , lag.max = 10))
plot(pacf(returns_NTPC , lag.max = 10))
auto.arima(returns_NTPC)
auto.arima(returns_NTPC,ic = "bic")
arima_final2 <- arima(returns_NTPC, order= c(0,0,0))
arima_final2
predicted <- predict(arima_final2, n.ahead = 10)
predicted
tsdiag(arima_final2)

## PART 3 ##

library(quantmod)
library(rugarch)
library(rmgarch)

## Data Collection ##
NESTLE1 <- getSymbols("NESTLEIND.NS", from = "2020-04-01", to = "2022-03-31", periodicity = "weekly")
NESTLE1 <- na.omit(NESTLE1)

NTPC1 <- getSymbols("NTPC.NS", from = "2019-03-01", to = "2021-02-28", periodicity = "weekly")
NTPC1 <- na.omit(NTPC1)

## Return Calculation ##
R.NESTLE <- dailyReturn(NESTLEIND.NS)
R.NTPC <- dailyReturn(NTPC.NS)

## Implementing Univariate GARCH ##
ug_spec = ugarchspec()
ug_spec

## Implementing EGARCH ##
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = R.NESTLE) 
ugfit1

ugfit2 = ugarchfit(spec = ug_spec, data = R.NTPC, solver = 'hybrid') 
ugfit2

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1

ugforecast2 = ugarchforecast(ugfit2, n.ahead=10) 
ugforecast2

##Monthly
## PART 1 ##
library(readxl)
library(quantmod)
library(forecast)

## Data Collection ##
NSE <- getSymbols.yahoo("^NSEI", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
NSE <- na.omit(NSE)

NESTLE <- getSymbols.yahoo("NESTLEIND.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
NESTLE <- na.omit(NESTLE)

NTPC <- getSymbols.yahoo("NTPC.NS", from = "2020-04-01", to = "2022-03-31", verbose = F, auto.assign = F, periodicity = "monthly")
NTPC <- na.omit(NTPC)

## Close Prices Data ##
close1 <- cbind(NSE$NSEI.Close, NESTLE$NESTLEIND.NS.Close)
close2 <- cbind(NSE$NSEI.Close, NTPC$NTPC.NS.Close)

## Data Manipulation ##
M_T_Bill <- read_excel("C:/Users/haris/Desktop/FIN F414/Assignment/Draft/M_T-Bills.xlsx")
M_T_Bill <- as.data.frame(M_T_Bill)
M_T_Billxts <- xts(M_T_Bill[,-1], order.by = as.Date(M_T_Bill$Date))
colnames(M_T_Billxts) <- c('values')

## Return Calculation ##
returns1 <- as.xts(tail(data.frame(close1),-1)/head(data.frame(close1),-1) -1)
returns1 <- data.frame(returns1[])
returns1$excessreturn = c(returns1$NSEI.Close - M_T_Billxts$values)
returns1$excessstockreturn1m=c(returns1$NESTLEIND.NS.Close-M_T_Billxts$values)
head(returns1)

returns2 <- as.xts(tail(data.frame(close2),-1)/head(data.frame(close2),-1) -1)
returns2 <- data.frame(returns2[])
returns2$excessreturn = c(returns2$NSEI.Close - M_T_Billxts$values)
returns2$excessstockreturn2m=c(returns2$NTPC.NS.Close-M_T_Billxts$values)
head(returns2)


## Regression Model ##
regression1 <- lm(excessstockreturn1m ~ excessreturn, data.frame(returns1[]))
summary(regression1)


regression2 <- lm(excessstockreturn2m ~ excessreturn, data.frame(returns2[]))
summary(regression2)


## PART 2 ##

library(tseries)

## Return Calculation ##
returns_NESTLE <- as.xts(tail(data.frame(NESTLE$NESTLEIND.NS.Close),-1)/head(data.frame(NESTLE$NESTLEIND.NS.Close),-1)-1, frequency = 52)
returns_NESTLE <- na.omit(returns_NESTLE)
returns_NTPC <- as.xts(tail(data.frame(NTPC$NTPC.NS.Close),-1)/head(data.frame(NTPC$NTPC.NS.Close),-1)-1, frequency = 52)
returns_NTPC <- na.omit(returns_NTPC)

## Data Manipulation ##
colnames(returns_NESTLE) <- "returns_NESTLE"
colnames(returns_NTPC) <- "returns_NTPC"
mean(returns_NESTLE)
var(returns_NESTLE)
mean(returns_NTPC)
var(returns_NTPC)

## Data Visualization ##
plot(NESTLE$NESTLEIND.NS.Close)
plot(returns_NESTLE)

plot(NTPC$NTPC.NS.Close)
plot(returns_NTPC)

## model identification AR & MA ##
adf.test(returns_NESTLE, alternative = "stationary")
plot(acf(returns_NESTLE , lag.max = 10))
plot(pacf(returns_NESTLE , lag.max = 10))
auto.arima(returns_NESTLE)
auto.arima(returns_NESTLE,ic = "bic")
arima_final1 <- arima(returns_NESTLE, order= c(0,0,0))
arima_final1
predicted <- predict(arima_final1, n.ahead = 10)
predicted
tsdiag(arima_final1)

adf.test(returns_NTPC, alternative = "stationary")
plot(acf(returns_NTPC , lag.max = 10))
plot(pacf(returns_NTPC , lag.max = 10))
auto.arima(returns_NTPC)
auto.arima(returns_NTPC,ic = "bic")
arima_final2 <- arima(returns_NTPC, order= c(0,0,0))
arima_final2
predicted <- predict(arima_final2, n.ahead = 10)
predicted
tsdiag(arima_final2)

## PART 3 ##

library(quantmod)
library(rugarch)
library(rmgarch)

## Data Collection ##
NESTLE1 <- getSymbols("NESTLEIND.NS", from = "2020-04-01", to = "2022-03-31", periodicity = "monthly")
NESTLE1 <- na.omit(NESTLE1)

NTPC1 <- getSymbols("NTPC.NS", from = "2019-03-01", to = "2021-02-28", periodicity = "monthly")
NTPC1 <- na.omit(NTPC1)

## Return Calculation ##
R.NESTLE <- dailyReturn(NESTLEIND.NS)
R.NTPC <- dailyReturn(NTPC.NS)

## Implementing Univariate GARCH ##
ug_spec = ugarchspec()
ug_spec

## Implementing EGARCH ##
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = R.NESTLE) 
ugfit1

ugfit2 = ugarchfit(spec = ug_spec, data = R.NTPC) 
ugfit2

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1

ugforecast2 = ugarchforecast(ugfit2, n.ahead=10) 
ugforecast2