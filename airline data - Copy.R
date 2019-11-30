library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(tseries)

# Using Arima Model - 
Airlines<-read_excel(file.choose()) # read the Airlines data
Airlines <- Airlines$Passengers
Airlines <- as.ts(Airlines)
View(Airlines)
class(Airlines)

Airlines1 <- ts(Airlines,start=c(1995,1),end=c(2002,12),frequency=12)
start(Airlines1)
end(Airlines1)
class(Airlines1)
end(Airlines1)
class(Airlines1)

sum(is.na(Airlines1))#used for finding the na or invalid values
summary(airlines1)
View(Airlines1)

#time series plot

data = decompose(Airlines1,"multiplicative")
plot(data)
plot(data$seasonal)
plot(data$trend)
plot(data$random)
plot(data$figure)

plot(Airlines1)
abline(reg=lm(Airlines1~time(Airlines1)))
cycle(Airlines1)

# Boxplot by Cycle
boxplot(Airlines1~cycle(Airlines1,xlab = "Date", ylab = "Passenger Number(100's)",
                        main = "Monthly Boxplot of passengers from 1995 to 2002"))

model = auto.arima(Airlines1)
auto.arima(Airlines1, ic = "aic", trace = TRUE)
plot(model$residuals)

# Forecast for next 2 year
Pass_Forecast <- forecast(model,Level=c(95),h=10*12)
plot(Pass_Forecast)
