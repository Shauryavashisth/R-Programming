a=read.csv("D://Study//BA//March-April Classes//PAR-Time-Series-Assignment//sales.csv")
str(a)
summary(a)
boxplot(a)
library(fpp)
library(ggplot2)
library(GGally)
library(Hmisc)
library(forecast)
library(fpp2)
library(dplyr)
library(tseries)

new = ts(a,1,start = c(1994, 1), end = c(2014, 12), frequency = 12)
View(new)

autoplot(new)
plot(decompose(new))
class(new)
ggAcf(new)
boxplot(new)

#ACF AND PACF
acf(new)
pacf(new)
#decomposing
decomp = stl(new, s.window="periodic")
deseasonal_sales <- seasadj(decomp)
plot(decomp)

#Checking stationarity 
adf.test(new, alternative = "stationary")

#Changing the order of the series by differentiating
count_d1 = diff(deseasonal_sales, differences = 1)
plot(count_d1)

#Checking if the series is stationary
adf.test(count_d1, alternative = "stationary")
#new data acf and pcf
acf(count_d1)
pacf(count_d1)

#applying ARIMA
auto.arima(deseasonal_sales, seasonal=FALSE)

fit = auto.arima(deseasonal_sales, seasonal=FALSE)
tsdisplay(residuals(fit))
fit




#2nd method

fit2 = arima(deseasonal_sales, order=c(6,1,6))
fit2
tsdisplay(residuals(fit2))

#Making Forecasts
fcast = forecast(fit2, h=30)
plot(fcast)

#Model with Seasoanlity

fit_w_seasonality = auto.arima(deseasonal_sales, seasonal=TRUE)
fit_w_seasonality
seas_fcast = forecast(fit_w_seasonality, h=30)
plot(seas_fcast)

