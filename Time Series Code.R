########Group Assingment#########

library(forecast)
library(TSA)
library(tseries)
library(ggplot2)

getwd()
setwd("../Desktop/R FOlder/")

#Reading & Plotting the Data
beer.data <- read.csv("beer.csv", header = T)
class(beer.data)

beer.data <- ts(beer.data, frequency = 4)
head(beer.data)

ts.plot(beer.data, col = "blue", xlab = "Quarter", ylab = "Sales", main = "Beer Sales by Quarter")
abline(reg = lm(beer.data~time(beer.data)), col="Red")
plot(aggregate(beer.data)) 
boxplot(time_beer~cycle(beer.data))


# Seasonality Check
pgram = periodogram(beer.data)
dd = data.frame(freq=pgram$freq,spec=pgram$spec) 
order = dd[order(-dd$spec),]
# display the 2 highest "power" frequencies top2
top2 = head(order,2)
top2
# convert frequency to time periods 
time = 1/top2$freq
time

#Decomposing TimeSeries
plot(decompose(beer.data ))


##########Running Holt-Winters TimeSeries Model#############

beer.model1 <- hw(beer.data, seasonal = "additive")
summary(beer.model1)

plot(beer.model1, ylab = "Beer Sales", xlab = "Time in quarter", type = "o", col = "Blue") #plot the ob served line and the forecast component

lines(fitted(beer.model1),col="Red") #plot fitted line over observed line residuals.fit=fit$residuals #residuals of trg data


beer.model1$model

accuracy(beer.model1)
forecast(beer.model1, 8)

plot(forecast(beer.model1,8))

Box.test(residuals(beer.model1), lag = 4, type = "Ljung")

plot(residuals(beer.model1))

hist(residuals(beer.model1), col = "green")

states <- beer.model1$model$states[,1:3]

colnames(states) <- cbind('Level', 'Trend', 'Seasonal')

plot(states, col = "blue", main = "decomposing the forecast")


#########ARIMA MODEL############

kpss.test(beer.data)

#the data has both Trend and Seasonality

nsdiffs(beer.data) #differencing required to remove seasonality
ndiffs(beer.data) #differencing required to remove trend

d.beer <- diff(beer.data)

plot(d.beer)

kpss.test(d.beer)

acf(d.beer)
pacf(d.beer)

dd.beer <- diff(d.beer, lag = 4)

plot(dd.beer)

kpss.test(dd.beer)

acf(dd.beer)
pacf(dd.beer)

#Identifying Non - Seasonal Components : 
    # PACF cuts off sharply at 2 suggesting AR 2 model
    # (2,1,0)

#Identifying Seasonal Components :
    # PACF Cuts off sharply at 2 suggesting AR 2 Model
    # (2,1,0)

beer.model.arima <- arima(beer.data, order = c(2,1,0), seasonal = list(order = c(0,1,1), period = 4))
beer.model.arima

acf(residuals(beer.model.arima))
hist(residuals(beer.model.arima), col = "blue")
Box.test(residuals(beer.model.arima), lag = 4, type = "Ljung")

beer.forecast <- predict(beer.model.arima, n.ahead = 8)
ts.plot(beer.data, beer.forecast$pred, lty = c(1,3))

#Running auto arima

beer.arima <- auto.arima(beer.data)
beer.arima

acf(residuals(beer.arima))
hist(residuals(beer.arima))
Box.test(residuals(beer.arima), lag = 4, type = "Ljung")

beer.forecast <- predict(beer.arima, n.ahead = 8)
ts.plot(beer.data, beer.forecast$pred, lty = c(1,3))

