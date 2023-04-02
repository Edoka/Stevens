# library(TSA)
# # install.packages("CADFtest")
# library("CADFtest")
# install.packages("forecast")
library(forecast)
# data(co2)
data1 <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/monthly-sunspots.csv", header=TRUE, stringsAsFactors=FALSE)

sunspots = data1$Sunspots

sunspots_ts_all = ts(sunspots, start = c(1749, 1), frequency = 12)
plot(sunspots_ts_all)

# To better view the trend, we took the data for a shorter time frame from  1749 to 1769
sunspots_ts = ts(sunspots, start = c(1749, 1), end = c(1769,  12), frequency = 12)
plot(sunspots_ts)
# The plot show that it does not seem to be stationary.

# plot(data1,ylab='Sunspot',main='Monthly count of the number of observed sunspots Zurich')
# 
# 
# Month=c("J","F","M","A","M","J","J","A","S","O","N","D")
# points(window(sunsports, start=c(1749 ,1)),pch=Month)

acf(as.vector(sunsports), lag.max = 36)
pacf(as.vector(sunsports), lag.max = 36)
plot(sunsports, ylab="Sunspots", xlab="Time")

# plot(diff(sunsports,lag = 12), ylab="First Seasonal difference of Sunspots", xlab="Time")
# acf(as.vector(diff(sunsports)), lag.max = 36)
# pacf(as.vector(diff(sunsports)), lag.max = 36)
# 
# 
# plot(diff(diff(sunsports,lag = 12)), ylab="First Seasonal difference of Sunspots", xlab="Time")
# acf(as.vector(diff(diff(sunsports,lag = 12))), lag.max = 36)
# pacf(as.vector(diff(diff(sunsports,lag = 12))), lag.max = 36)



CADFtest(sunsports, lag=12)
adf.test(sunsports, alternative = 'stationary')

# CADFtest shows that the data is stationary since p-value = 2.202e-12 is very close to zero. This is further confirmed by adf.test.
# However, 

# After trying a series on arima models, We came to the conclusion that ARIMA(2,1,2) is best for the non seasonal part and (2,1,1) for the seasonal part.
#This gave us the lowest AIC 
sarima=arima(sunsports,order = c(2,1,2), seasonal = list(order=c(2,1,1), period=12))
sarima
# dif.sunsport <- diff(sunsports)

# sarima1=arima(diff(sunsports,lag = 12),order = c(0,1,1), seasonal = list(order=c(0,1,1), period=12))
# sarima1

plot(window(residuals(sarima),start=c(1749,1)),main='Monthly count of the number of observed sunspots', ylab='Sunspots')
abline(h=0)
acf(as.vector(window(rstandard(sarima), start=c(1749,1))), lag.max = 36)
pacf(as.vector(window(rstandard(sarima), start=c(1749,1))), lag.max = 36)
hist(window(rstandard(sarima), start=c(1749,1)), xlab = 'Standardized Residuals')

qqnorm(window(residuals(sarima), start=c(1749,1)))
qqline(window(residuals(sarima), start=c(1749,12)))
shapiro.test(residuals(sarima))
tsdiag(sarima, gof=36, omit.initial=f)





