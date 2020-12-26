library("dplyr")
library(tidyverse)
library(forecast)
library(readxl)
library('plm')
library(dlm)
library(ggplot2)
library(zoo)
library(lattice)
library(dygraphs)
library(MASS) 
library(PerformanceAnalytics)
library('pastecs')
library(sn)
library(tseries)
library(lubridate)
library(aTSA)
library("TTR")
library('TSstudio')
library("numDeriv")
require(ggfortify)  #for plotting timeseries
require('TTR')
library(tsbox)
library(quantmod)

#getSymbols("NG", header=TRUE, row.names="Date")
NG <- getSymbols("NG", auto.assign = FALSE) # OHLCVA data
head(NG)
tail(NG)

#cycle(NG)

#time(NG) <- as.yearmon(time(NG))
#time(NG)

barChart(NG)
#candleChart(NG, up.col='green',dn.col='red')
#chartSeries(to.weekly(NG),up.col='green',dn.col='red')

# Getting OHLC data
NG_ohlc <-OHLC(NG)
str(NG_ohlc)
#colSums(is.na(NG_ohlc)) # checking missing values

stat.desc(NG_ohlc, basic=TRUE, norm=TRUE, p=0.95)

nmonths(NG_ohlc) # number of months
# start and end dates
start(NG_ohlc)
end(NG_ohlc) 

dygraph(OHLC(NG_ohlc), ylab='Price (US$)')

NG1 <- tail(NG_ohlc, n=30)
graph<-dygraph(OHLC(NG1), ylab = 'Price(US$)')
dyCandlestick(graph)

# Extracting the "close" column from data set
NG_close <- Cl(NG)

# converting daily data to monthly mean for faster computation
NG_close_month <- apply.monthly(NG_close, mean)

tail(NG_close_month['2007-03/'])

plot(NG_close_month) # plotting Close price


# setting up "Date" column
row.names(NG_close_month)
# NULL
NG_close_month <- as.data.frame(NG_close_month)
NG_close_month$Date <- row.names(NG_close_month)

cln <- ncol(NG_close_month) 
NG_close_month <- NG_close_month[, c(cln, 1:(cln-1))]
row.names(NG_close_month) <- NULL
head(NG_close_month)

str(NG_close_month)

# converting the dataframe to time series
df1 = ts(NG_close_month$'NG.Close', start= c(2007,1), 
         end = c(2020,3), frequency = 12)

#xyplot(df1, ylab = "Price (US $)", main = "Time series plot for Schlumberger Adj Close price")

par(mfrow=c(2,1))        # set up the graphics 
hist(df1, prob=TRUE, 12)   # histogram    
lines(density(df1))     # density for details 
qqnorm(df1)             # normal Q-Q plot  
qqline(df1)

ggtsdisplay(df1,
            plot.type = "partial",
            main = "ACF & PACF plot for `Natural Gas' Time-Series",
            smooth = TRUE) 

#to check for the stationarity of the time series
adf.test(df1) # Augmented Dickey-Fuller Test 
pp.test(df1) # Phillips-Perron Unit Root Test 
kpss.test(df1) # KPSS Unit Root Test

# Normalize the time series using mean & std
NG_stationary <- rnorm(length(df1), mean=1, sd=1) 

# time series with a trend
NG_trend <- cumsum(rnorm(length(df1), mean=1, sd=4)) + df1/100 

# normalize each trend & starionary
NG_stationary <- NG_stationary / max(NG_stationary) 
NG_trend <- NG_trend / max(NG_trend)

#ggtsdisplay(NG_stationary, plot.type = "partial",
# main = "ACF & PACF plot for `Transformed Natural Gas' Time-Series",smooth = TRUE) 

plot.new()
frame()
par(mfcol=c(2,2))

# the stationary signal and ACF
plot(NG_stationary,
     type='l', col='blue',
     xlab = "time (t)",
     ylab = "Change in Schlumberger price index",
     main = "Stationary signal")
acf(NG_stationary, lag.max = length(NG_stationary),
         xlab = "lag #", ylab = 'ACF',main=' ')

# the trend signal and ACF
plot(NG_trend,
     type='l',col='blue',
     xlab = "time (t)",
     ylab = "Change in Schlumberger price index",
     main = "",  
     sub="Trend signal")
acf(NG_trend, lag.max = length(NG_trend),
         xlab = "lag #", ylab = 'ACF', main=' ')

adf.test(NG_stationary) #to check for the stationarity of the time series
pp.test(NG_stationary)
kpss.test(NG_stationary)

#head(NG_close)
#tail(NG_close)

plot(decompose(df1), yax.flip=TRUE)  # Decompose after conversion to time-series

#attr(NG_close_month, 'frequency') <- 12  # Set the frequency of the xts object to yearly
#periodicity(NG_close_month)             # check periodicity: weekly 
#plot(decompose(as.ts(NG_close_month)), yax.flip=TRUE)  # Decompose after conversion to time-series

# converting xts object to time-series with monthly frequency
#ng_ts = as.ts(NG_close_month, start=c(2007,1), end=c(2020,3))


## Structural time series models
par(mfrow = c(3, 1))
plot(NG_close_month$NG.Close, type ='o')

## local level model
(fit_level <- StructTS(df1, type = "level"))

lines(fitted(fit_level), lty = "dashed", lwd = 2)  # contemporaneous smoothing
lines(tsSmooth(fit_level), lty = "dotted", lwd = 2) # fixed-interval smoothing
plot(residuals(fit_level)); abline(h = 0, lty = 3)

pred <- predict(fit_level, n.ahead = 12)

## with 50% confidence interval
#ts.union(NG_close_month, pred$pred,
        #pred$pred + 0.5*pred$se, pred$pred - 0.5*pred$se)
#legend('topright', lty=1, legend=c('Prediction_50% confidence interval'))
#ts.union(NG_close_month, pred$pred,
        #pred$pred + 0.9*pred$se, pred$pred - 0.9*pred$se)
#legend('topright', lty=1, legend=c('Prediction_90% confidence interval'))


## with 50% confidence interval
#ts.pred(df1, pred$pred,
        #pred$pred + 0.5*pred$se, pred$pred - 0.5*pred$se)
#legend('topright', lty=1, legend=c('Prediction_50% confidence interval'))
#ts.pred(df1, pred$pred,
        #pred$pred + 0.9*pred$se, pred$pred - 0.9*pred$se)
#legend('topright', lty=1, legend=c('Prediction_90% confidence interval'))

str(pred)

pred

plot(forecast::forecast(fit_level, level = c(50, 90), h = 12), xlim = c(2016, 2021))

# recommended setting
auto.arima(df1, trace = T, stepwise = F, approximation = F)

# Arima 
arima <- Arima(df1, order = c(0,1,0))
checkresiduals(arima)

## ARIMA Forecasting

# Forecast of 12 months
pred_arima <- forecast::forecast(arima, h=12)
plot(pred_arima)

pred_arima

# forecasting using dlm model
# local level and seasonal model (Stochastic level and deterministic seasonal)

model <- function(p) {
    return(
      dlmModPoly(2, dV=p[1], dW=p[2:3]) +
      dlmModSeas(12, dV=p[4])
    )
}

# estimating parameters
mle <- dlmMLE(df1, parm = c(0.1, 0.001, 1, 1), build = model)
if (mle$convergence == 0) print('converge') else print('did not converge')

str(model)

unlist(mle)

mle$par

modelfit = model(mle$par) # fitting the dlm model
str(modelfit)

# applying kalman filter
model_filter <- dlmFilter(df1, modelfit)
plot(residuals(model_filter, sd = FALSE), type = "o", ylab = "Standardized prediction error")
abline(h = 0)

str(model_filter,1)

# applying kalman smoother
model_smoothed <- dlmSmooth(df1, modelfit)
str(model_smoothed,1)

with(model_smoothed, drop(dlmSvd2var(U.S[[15]], D.S[15,])))

with(model_smoothed, drop(dlmSvd2var(U.S[[51]], D.S[51,])))

with(model_filter, drop(dlmSvd2var(U.C[[51]], D.C[51,])))

cov <- dlmSvd2var(model_smoothed$U.S, model_smoothed$D.S)
cov

lev.var <- sapply(cov, function(x){x[1,1]})
lev.var

sm <- dropFirst(model_smoothed$s)
mu <- c(sm[,1])
nu <- c(sm[,2])
res <- c(residuals(model_filter,sd=F))

hist(res, prob=T, col="grey", 
     main = "Histogram of standardised one-step prediction errors",
     xlab="Residuals")

par(mfrow=c(3,1))

temp <- window(cbind(df1, mu),start = 2007, end = 2020)
plot(temp, plot.type="single",
col =c("darkgrey","blue"),lty=c(1,2), xlab="",ylab = "", )
legend("topleft",
leg = c("Qtrly price changes_Natural Gas"," stochastic level"),
cex = 0.7, lty = c(1, 2),col = c("darkgrey","blue"),
pch=c(3,NA),bty = "y", horiz = F)

#par(mfrow=c(1,1))
temp <- ts(nu,start = 2007,frequency =4)
plot(temp, col =c("darkgrey"),lty=1, xlab="",
ylab = "Stochastic Seasonal",main="" )

#par(mfrow=c(1,1))
temp <- ts(res, start = 2007,frequency =4)
plot(temp, col =c("darkgrey"),lty=1, xlab="",ylab = "irregular",main="" )

lev.ts <- ts(lev.var[-1],start = 2007, frequency =12)
wid <- qnorm(0.05, lower = FALSE) *sqrt(lev.ts)
temp <- cbind(mu, mu + wid %o% c(-1, 1))
temp <- ts(temp,start = 2007,frequency =12)

par(mfrow=c(1,1))
plot(lev.ts,xlab="",ylab = "Level Estimation Variance")

par(mfrow=c(1,1))
plot(temp, plot.type = "s", type = "l",lty = c(1, 5, 5),
ylab = "Price(US$)", xlab = "", ylim = range(df1),col=c("blue","red","red"),lwd=2)
lines(df1, type = "o", col = "darkgrey")
legend("topright",
leg = c("Natural Gas Close Price"," stochastic level +/- 1.64SE"),
cex = 0.7, lty = c(1, 5),col = c("darkgrey","red"),
,bty = "y", horiz = F)

#model_smoothed = rapply(model_smoothed, f=function(x) ifelse(is.nan(x),0,x), how="replace" )

n <- 1*12
forecast <- dlmForecast(model_filter, nAhead=n)
 
x <- index(df1)
xf <- seq(max(x), max(x)+n/12, 1/12)
aa <- forecast$a[, -1] * (-1)
aa <- cbind(forecast$a[, 1], aa)
a <- drop(forecast$a%*%t(FF(modelfit)))
a <- c(tail(df1, 1), a)
df <- rbind(
  data.frame(x=x, y=as.numeric(df1), series="Original"),
  data.frame(x=x, y=apply(model_filter$m[-1, 1:2], 1, sum), series="Filtered"),
  data.frame(x=x, y=apply(model_smoothed$s[-1, 1:2], 1, sum), series="Smoothed"),
  data.frame(x=xf, y=a, series="Prediction")
)
dlm <- ggplot(subset(df, x>2000), aes(x=x, y=y, colour=series)) + geom_line()
dlm

forecast$f


