# Simple example of a time series object

# Vector is monthly information starting at 1/2003
sales <- c(23, 33, 41, 7, 34, 35, 24, 25, 21, 25, 20,
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)

time_series_sales <- ts(sales, start = c(2003, 1), frequency = 12)

plot(time_series_sales)
start(time_series_sales)
end(time_series_sales)
frequency(time_series_sales)

# First step of time series analysis is to describe
# it numerically and visually

# We're using a Nile time series that recorded
# annual flow of the nile between 1871-1970

# Plot the data first and smooth it to resolve
# error components

install.packages("forecast")
library(forecast)

default_settings <- par(no.readonly = TRUE)
par(mfrow = c(2,2))

yrange <- c(min(Nile), max(Nile))
plot(Nile, main = "Raw time series")
# ma() function is used to smooth the Nile time series
plot(ma(Nile, 3), main = "Simple moving averages (k = 3)", ylim = yrange)
plot(ma(Nile, 7), main = "Simple moving averages (k = 7)", ylim = yrange)
plot(ma(Nile, 15), main = "Simple moving averages (k = 15)", ylim = yrange)
par(default_settings)

# As k increases the plot becomes increasingly smooth

# Plot the acf chart - measure of how the observations
# in the time series relate to each other 
# if the autocorrelation crosses the dashed blue line
# it means specific lag is significantly correlated with
# current time series. A stationary time series will have
# autocorrelation for quickly ro 0. With non-stationarity
# series that drops quickly.

# ACF() plot
acf_result <- Acf(Nile)
acf_result

# PACF or Partial auto-correlation plot
pacf_result <- Pacf(Nile)

# Test that the time series is stationary
library(tseries)

# if p < 0.05 then the time series is stationary
adf.test(Nile)
plot(Nile)

# Assess the presense of a trend in the data
ndiffs(Nile)

# Assessing the presence of a trend in the differenced time series
ndiffs(diff_Nile)

# There is a trend within the data, so the series is differenced
# once (lag = 1)
diff_Nile <- diff(Nile, lag = 1) # given by ndiffs test, value of lag
ndiffs(diff_Nile)

# Show both charts side-by-side for comparision
default_settings <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
plot(Nile)
plot(diff_Nile)
par(default_settings)

# Now we can proceed to the next step
adf.test(diff_Nile)

# Identifying one or more models
# we need to examine autocorrelation and partial autocorrelation plots
# for the differenced Nile time series

Acf(diff_Nile, main = "Autocorrelation plot for differenced nile time series")

#partial autocorrlation plot
pacf(diff_Nile, main = " Partial Autocorrelation plot for differenced nile time series")


# We use the original dataset for the Arima model
# and modify the d-value to suit our area of findings
# and d = 1
arima_model <- Arima(Nile, order = c(0, 1, 1))
arima_model

# Accuracy measures using the MAPE
# Measures the prediction of accuracy
accuracy(arima_model)

qqnorm(arima_model$residuals)
qqline(arima_model$residuals)

# box test function provides a test that autocorrelates
# are all 0 (Null Hypothesis)
Box.test(arima_model$residuals, type = "Ljung-Box")

# Forecast three years ahead for the Nile time series
forecast(arima_model, 3)
plot(forecast(arima_model, 3), xlab = "Year", ylab = "Annual flow")

# Automated Arima forecast
auto_arima_model <- auto.arima(Nile)
accuracy(auto_arima_model)

qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)

# Plot information for predicted values
plot(forecast(auto_arima_model, 3), 
              xlab = "Year", 
              ylab = "Annual flow")

auto_arima_model
arima_model


