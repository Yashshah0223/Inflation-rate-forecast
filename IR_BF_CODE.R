library(fpp)
library(fpp2)
library(TTR)
library(mFilter)
library(readxl)
Business_Forecasting_proj_dataset <- read_excel("...Path...")
library(readxl)
View(Business_Forecasting_proj_dataset)
bf_data = Business_Forecasting_proj_dataset
#Create a Time series
bf_data_ts = ts(bf_data$`United States`, start = 1960, end = 2020, frequency = 1)
bf_data_ts
#plot your time series
Acf(bf_data_ts)
plot(bf_data_ts)
boxplot(bf_data_ts)
#Taking only the relevant part of the data i.e Onwards 1983 as economic reforms where introduced. 
bf_data_stable = window(bf_data_ts, start = 1983)
bf_data_stable
plot(bf_data_stable)
Acf(bf_data_stable)
summary(bf_data_stable)
class(bf_data_stable)
attributes(bf_data_stable)
boxplot(bf_data_stable)
hist(bf_data_stable)
#Checking the trend of the time series
bf_data_stable_trend = mFilter(bf_data_stable)
plot(bf_data_stable_trend)
#Naive forecast
naive_stable = naive(bf_data_stable, h = 3)
naive_stable
plot(naive_stable)
accuracy(naive_stable)
attributes(naive_stable)
#Checking Residuals
plot(naive_stable$residuals)
Acf(naive_stable$residuals)
plot(naive_stable$fitted, naive_stable$residuals)
plot(naive_stable$x, naive_stable$residuals)
plot(naive_stable$fitted, naive_stable$residuals, xy.labels = FALSE,  xy.lines = FALSE)
plot(naive_stable$x, naive_stable$residuals, xy.labels = FALSE,  xy.lines = FALSE)
#forecasting Naive
f_n = forecast(naive_stable, h = 3)
accuracy(f_n)
#Moving Average Forecast
ma_3 = ma(bf_data_stable, order = 3)
ma_6 = ma(bf_data_stable, order = 6)
ma_12 = ma(bf_data_stable, order = 12)
plot(bf_data_stable)
lines(ma_3, col = "red")
lines(ma_6, col = "blue")
lines(ma_12, col = "green")
plot(ma_3)
plot(ma_6)
plot(ma_12)
#forecast of moving averages
ma_forecast = forecast(ma_3, h = 3)
ma_forecast
accuracy(ma_forecast)
#Exponential smoothing 
ets(bf_data_stable)
ets_1 = ets(bf_data_stable)
attributes(ets_1)
ets_1
ets_1$mse
accuracy(ets_frct)
#Using forecast for ETS
ets_frct = forecast(ets_1,h=3)
ets_frct
accuracy(ets_frct)
plot(ets_frct)
ets_frct$residuals
plot(ets_frct$residuals)
hist(ets_frct$residuals)
accuracy(ets_frct)
#Holts - Winters
hw_stable <- HoltWinters(bf_data_stable, beta= FALSE , gamma = FALSE)
hw_stable
plot(hw_stable)
#We put gamma = FALSE as there in no seasonality in the ts
hw_stable_1 <- HoltWinters(bf_data_stable, gamma = FALSE)
hw_stable_1
plot(hw_stable_1)
#Forecast using HW
hw_forecast = forecast(hw_stable_1, h= 3)
hw_forecast
accuracy(hw_forecast)
plot(hw_forecast)
#Tested Snaive and saw that behaves the same as naive 
snaive_fw = snaive(bf_data_stable, h = 3)
plot(snaive_fw)
snaive_fw
accuracy(snaive)
#Random Walk Forward
rwf_f = rwf(bf_data_stable,drift = TRUE, 3)
plot(rwf_f)
#comparing the techniques using meanf
plot(meanf(bf_data_stable, 3)
lines(snaive_fw$mean, col = "red")
lines(rwf_f$mean, col = "black")
lines(ets_frct$mean, col = "pink")
#ARIMA Model
adf.test(bf_data_stable)
kpss.test(bf_data_stable)
ndiffs(bf_data_stable)
tsdisplay(bf_data_stable)
bf_data_stable1 = diff(bf_data_stable, differences = 1)
ndiffs(bf_data_stable1)
tsdisplay(bf_data_stable1)
#Using auto arima
arima_bf = auto.arima(bf_data_stable1)
arima_bf
arima_bf = auto.arima(bf_data_stable1, stepwise = FALSE, trace = TRUE)
f_arima = forecast(arima_bf, h=3)
f_arima
accuracy(f_arima)
plot(f_arima)
tsdiag(arima_bf)
Acf(arima_bf$residuals)