library(dplyr)
library(tidyr)
library(forecast)
library("stringr") 
library(e1071)
library(lattice)
library(caret)
######### Reading the data ########

mydata <- read.csv("wind1.csv")
summary(mydata)
###### Preprocessing ######
boxplot(mydata$Wind.Speed..m.s.)
boxplot(mydata$LV.ActivePower..kW.)
######### REMOVING THE OUTLIERS #######"
summary(mydata)

df_new <- mydata %>%
  dplyr::select (LV.ActivePower..kW.,Wind.Speed..m.s.) %>% #getting mentioned variables
  filter (LV.ActivePower..kW. > 0  & Wind.Speed..m.s. < 18.4  ) %>%   #filtering data with respect to gender and age
  drop_na() #dropping NA values.
df_new <- df_new %>% drop_na()
summary(df_new)
boxplot(df_new$Wind.Speed..m.s.)
boxplot(df_new$LV.ActivePower..kW.)

############## setting the variables #########
observ = 2500
library(caret)

wind_s <- df_new$Wind.Speed..m.s.[ 1 : observ]
power_g <- df_new$LV.ActivePower..kW.[1:observ]
summary(wind_s)

cor(wind_s,power_g)
plot(wind_s,power_g)
###################

########## TIME SERIES ################"

library(tseries)
yt <- ts(df_new)
plot(yt)
wind_speed <- ts(wind_s[1:observ])
plot(wind_speed)

train_set <- window(wind_speed, start = 2200, end = 2500)
plot(train_set)
str(train_set)

###################  STATIONARITY TEST ###########

adf.test(train_set, alternative="stationary", k=0)

################## AUTO-ARRIMA ###############
Bmodel<-auto.arima(train_set,max.p=5, max.q=0,d=0,trace=T,test="adf",stationary=F)
  Bmodel
fit1 <- forecast(Bmodel)

plot(train_set, type = 'l',main="ARIMA(3,0,0)",ylab="", xlim = c(2200,2500))
lines(fitted(fit1), col = 2, lty = 2) 
summary(fit1)
########### ANN ############
library(nnet)
library(NeuralNetTools)

fittt <- nnetar(train_set)
fit2 <- forecast(fittt)
plot(fit2)
summary(fit2)
plot(train_set, type = 'l',main="NNAR(3,2)",ylab="", xlim = c(2200, 2500))
lines(fitted(fit2), col = 2, lty = 2)

##################### 

############## ERRORS TESTING ############"
require(Metrics)
AR_pre <- fitted(fit1)
NR_pre <- fitted(fit2)

a <- AR_pre[282:301]
n <- NR_pre[282:301]
s <- test_set[282:301]

##### the estimation of differentiation criteria for ARIMA #####
mad(s,a)
mape(s,a)
mase(s, a,step_size = 1)

##### the estimation of differentiation criteria for ANN #####
mad(s,n)
mape(s,n)
mase(s, n,step_size = 1)




