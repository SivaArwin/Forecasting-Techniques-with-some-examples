library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(tseries)


#here we gonna use the arima model 


coke = read_excel(file.choose())
coke = coke$Sales
coke = as.ts(coke)#here we use time-series function
View(coke)
str(coke)
class(coke)

cokee = ts(coke, start = c(1986,1), end = c(1995,6), frequency = 5)
start(cokee)
end(cokee)
class(cokee)

#we have to find the no. of missing or invalid values
sum(is.na(cokee))
summary(cokee)
View(cokee)

#decompost
data = decompose(cokee, "additive")
plot(data)

#in the above plot we dont find the actual result hence we are using multiplicative method

data1 = decompose(cokee, "multiplicative")
plot(data1)

plot(data1$seasonal)
plot(data1$trend)
plot(data1$random)
plot(data1$figure)

plot(cokee)
reg=lm(cokee~time(cokee))
abline(reg = lm(cokee~time(cokee)))
summary(cokee)
cycle(cokee)

boxplot(cokee~cycle(cokee, xlab="Date", ylab = "customer rate(100's)",
                    main = "customers from 1995 to 2002"))

#auto arima model

model = auto.arima(cokee)
auto.arima(cokee, ic="aic", trace = T)

plot.ts(model$residuals)

#forecast of the coke for next 2 years

forecast = forecast(model,level=c(95), h =10*12)
plot(forecast)
