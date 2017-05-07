#installed the libraries first
library('forecast')
library('ggplot2')
library('tseries')

dataset = read.csv('day.csv', header = TRUE, stringsAsFactors = FALSE)
head(dataset)
str(dataset)

#to examine the data, lets convert date into as.date format
dataset$date <- as.Date(dataset$dteday)
str(dataset)

#visualize time series
ggplot(dataset, aes(date, cnt)) + 
  geom_line() +
  scale_x_date('month') +
  xlab("") +
  ylab("Bike checkouts daily")

#several outliers and anamolies observed, so smoothening the data using tsclean
#first convert into a time series object

cnt_ts <- ts(dataset[, c('cnt')]) #part of ts package
dataset$cleaned_cnt <- tsclean(cnt_ts) #part of forecast package
ggplot(dataset, aes(date, cleaned_cnt)) + 
  geom_line() +
  scale_x_date('month') +
  xlab("") +
  ylab("Bike checkouts daily (smoothened)")

#still several outliers so let's take monthly and weekly moving averages

dataset$cnt_ma7 <- ma(dataset$cleaned_cnt, order = 7)
dataset$cnt_ma30 <- ma(dataset$cleaned_cnt, order = 30)
ggplot() +
  geom_line(data = dataset, aes(x= date, y = cleaned_cnt, colour = "Counts")) +
  geom_line(data = dataset, aes(x= date, y = cnt_ma7, colour = "Weekly MA")) +
  geom_line(data = dataset, aes(x= date, y = cnt_ma30, colour = "Monthly MA")) +
  ylab("Bike checkouts")

#let's consider weekly series to be a middle ground and use that for our modeling moving ahead
ggplot() +
  geom_line(data = dataset, aes(x= date, y = cnt_ma7, colour = "Weekly MA")) +
  ylab("Bike checkouts")

#decomposition and seasonality removal 

count_ma7 <- ts(na.omit(dataset$cnt_ma7), frequency = 30) #30 is the periodicity of the data, since we have daily data
decomp <- stl(count_ma7, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp) #gives all 3 building blocks of seasonality, trend and cycle, and residual error

#deseasonal_cnt is our new dependent variable

#seasonality has been fixed, let's move on to stationarity

adf.test(count_ma7, alternative = 'stationary')

#ACF and PACF plots
acf(count_ma7, main='')
pacf(count_ma7, main='')

#difference data once d=1 and perfom ADF test again
count_diff1 <- diff(deseasonal_cnt, differences = 1)
plot(count_diff1) #see turning more stationary
adf.test(count_diff1,alternative = 'stationary')
#looking at the plot we can see trend is almost absent here so we conclude that d=1 is fine

acf(count_diff1, main='ACF for differenced series d=1')
pacf(count_diff1, main='PACF for differenced series d=1')


#Fitting an ARIMA model
fit<- auto.arima(deseasonal_cnt, seasonal = FALSE) #seasonal is false because we already took care of it
fit
tsdisplay(residuals(fit), lag.max = 45, main = 'ARIMA (1,1,1) model residuals')

#one lag on scf at 7. Hence replace q=7 
fit2 <- arima(deseasonal_cnt, order= c(1,1,7)) #iterate 
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
#AIC value is small, also no significant autocorrelations in ACF or PACF. 
#Residuals are normal too, white noise. This is the best model we have


#this is the best model
#now forecasting

count_forecast <- forecast(fit2, h=30) #forecast for h periods
count_forecast
plot(count_forecast)
