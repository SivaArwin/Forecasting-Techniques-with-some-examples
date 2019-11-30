library(forecast)
library(fpp)
library(tseries)
library(smooth)
library(readxl)
x = read.csv(file.choose())
View(x)

amts = ts(x$Sales, frequency = 4, start = c(86))
View(amts)

train=amts[1:38]
test=amts[39:42]

train = ts(train,frequency = 4)
test = ts(test, frequency = 4)

plot(amts)

Holtz=HoltWinters(train, alpha = 0.2, beta = F, gamma = F)
Holtz_pred = data.frame(predict(Holtz, n.ahead = 4))
Holtz_pred
plot(forecast(Holtz,h=4))


Holtz_mape = MAPE(Holtz_pred$fit, test)*100
Holt_a = HoltWinters(train,alpha = 0.2, beta = 0.1, gamma = F)
Holta_pred = data.frame(predict(Holt_a,n.ahead = 4))
plot(forecast(Holt_a,h=4))

Holtab_mape = MAPE(Holta_pred$fit,test)*100
hw_abg = HoltWinters(train, alpha = 0.2, beta = 0.1, gamma = 0.1)
hw_abg

hwabg_pred = data.frame(predict(hw_abg,n.ahead = 4))
plot(forecast(hw_abg, h =4))
hwabg_mape = MAPE(hwabg_pred$fit, test)*100

hw_na = HoltWinters(train, beta = F, gamma = F)
hwna_pred = data.frame(predict(hw_na,n.ahead = 4))
plot(forecast(hw_na, h=4))
hwna_mape = MAPE(hwna_pred$fit,test)*100

hw_nab = HoltWinters(train,gamma=F)
hw_nab

hwnab_pred = data.frame(predict(hw_nab, n.ahead = 4))
hwnab_pred 
plot(forecast(hw_nab,h=4))

hwnabg_mape = MAPE(hwnab_pred$fit,test)*100
hw_nabg = HoltWinters(train)
hw_nabg

hwnabg_pred = data.frame(predict(hw_nabg,n.ahead = 4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape = MAPE(hwnabg_pred$fit, test)*100
