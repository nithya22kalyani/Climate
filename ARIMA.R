setwd("E:\\DataScience\\Kaggle\\Climate")
getwd()
gt<- read.csv("GlobalTemperatures.csv", header=TRUE, stringsAsFactors=FALSE)
summary(gt$LandAverageTemperature)
gt$LandAverageTemperatureUncertainty = NULL
gt$LandMaxTemperatureUncertainty = NULL
gt$LandMinTemperatureUncertainty = NULL
gt$LandAndOceanAverageTemperatureUncertainty = NULL
gty2000<- subset(gt,(gt$yyyy<-substr((as.Date(gt$dt)),1,4)>=2000))
library('ggplot2')
library('forecast')
library('tseries')
gty2000$dt = as.Date(gty2000$dt)
ggplot(gty2000, aes(dt, LandAverageTemperature)) + geom_line() + scale_x_date('month')  + ylab("tttt") + xlab("dfsdf")
#cleaning time sereis data with outliers. tsclean() identifies and replaces outliers using series smoothing and decomposition. 
#This method is also capable of inputing missing values in the series if there are any.
lat_ts = ts(gty2000[, c('LandAverageTemperature')])
gty2000$cleanlat = tsclean(lat_ts)
ggplot() + geom_line(data = gty2000, aes(x = dt, y = cleanlat)) + ylab('tttt')
#moving average to average the monthly values to check pattern
gty2000<-na.omit(gty2000)
gty2000$lat_ma30 = ma(gty2000$cleanlat, order=30)
ggplot() +
  geom_line(data = gty2000, aes(x = dt, y = cleanlat, colour = "Counts")) +
  geom_line(data = gty2000, aes(x = dt, y = lat_ma30, colour = "Monthly Moving Average"))  +
  ylab('gdfgdf')

#Decomposing
#STL is a flexible function for decomposing and forecasting the series. It calculates the seasonal component of the series
#using smoothing, and adjusts the original series by subtracting seasonality in two simple lines:
count_ma = ts(na.omit(gty2000$LandAverageTemperature), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#Fitting an ARIMA model requires the series to be stationary. 
#A series is said to be stationary when its mean, variance, and autocovariance are time invariant.
adf.test(count_ma, alternative = "stationary")

#Autocorrelations and Choosing Model Order
Acf(count_ma, main='')
Pacf(count_ma, main='')

#differencing
# differencing can remove the trend and coerce the data to stationarity. 
#Differencing looks at the difference between the value of a time series at a certain point in time and its preceding value

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
#the choice of p or q for our model:
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')
#R plots 95% significance boundaries as blue dotted lines
#Fitting an ARIMA model
auto.arima(deseasonal_cnt, seasonal=FALSE)

#Evaluate and Iterate
eval1<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(eval1), lag.max=45, main='(1,1,1) Model Residuals')
eval1
#add seasonal check here and later forecast finally



eval2 = arima(deseasonal_cnt, order=c(8,1,14))
eval2
tsdisplay(residuals(eval2), lag.max=15, main='Seasonal Model Residuals')

#Forecast from Arima
fcast <- forecast(fit2, h=50)
plot(fcast)

#but what if we wanted to get a sense of how the model will perform in the future? 
#One method is to reserve a portion of our data as a "hold-out" set, fit the model,
#and then compare the forecast to the actual observed values:

#hold <- window(ts(deseasonal_cnt), start=700)
#fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))
#fcast_no_holdout <- forecast(fit_no_holdout,h=25)
#plot(fcast_no_holdout, main=" ")
#lines(ts(deseasonal_cnt))

#add back the seasonal component we extracted earlier.
# Re-fitting the model on the same data, we see that there still might be some seasonal pattern in the series, with the seasonal component 
eval_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
eval_seasonality
#30 year forecast
seas_fcast <- forecast(fit_w_seasonality, h=50)
plot(seas_fcast)

tsdisplay(residuals(seas_fcast), lag.max=15, main='Forecast Model Residuals')
seas_fcast


seas_fcast <- forecast(eval_seasonality, h=50)
plot(seas_fcast)

fcastarima <- forecast(eval2, h=50)
plot(fcastarima)
Year<-seq(2016,2065,1)
Yeardf<-as.data.frame(Year)
fcastarimadf<-as.data.frame(fcastarima)
nrow(Yeardf)
nrow(fcastarimadf)
fcastarima1<-cbind(Yeardf,fcastarimadf)
row.names(fcastarima1)<- NULL


