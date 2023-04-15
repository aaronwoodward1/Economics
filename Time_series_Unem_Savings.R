###############################################
# U.S. Unemployment Duration and Savings Rate #
###############################################

# Loading libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(xts)
library(tseries)
library(fredr)
library(ggplot2)

#658b4b77ed8f978ca0738e58f03d860f

# Accessing FRED data via API Key
fredr_set_key("658b4b77ed8f978ca0738e58f03d860f")

#Pulling Unemployment Duration Data
unem_dur <- fredr(
  series_id = "LNU03008275",
  observation_start = as.Date("2019-10-01"),
  observation_end = as.Date("2022-10-01"),
  frequency = "m" # quarterly
  #units = "chg" # change over previous value
)

#Plotting Unemployment Duration Time Series
ggplot(data = unem_dur, aes(x = date, y = value, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = "Average Weeks of Unemployment \n (Not seasonally adjusted)", color = "FRED Series")

#Pulling Savings Data
savings_rate <- fredr(
  series_id = "PSAVERT",
  observation_start = as.Date("2019-09-01"),
  observation_end = as.Date("2022-09-01"),
  frequency = "m" # quarterly
  #units = "chg" # change over previous value
)

#Plotting Savings Data
ggplot(data = savings_rate, aes(x = date, y = value, color = series_id, fill = 'blue')) +
  geom_line() + scale_color_manual(values = 'Blue') +
  labs(x = "Observation Date", y = "Personal Savings Rate\n (Percent %)", color = "FRED Series")

#############################
#Time Series Analysis - Unemployment Duration
attach(unem_dur)

#Defining the variables
Y_unem_dur <- value
d.Y_unem_dur <- diff(Y_unem_dur)
t <- date

# Descriptive statistics and plotting the data
summary(Y_unem_dur)
summary(d.Y_unem_dur)

plot.ts(Y_unem_dur)
plot.ts(d.Y_unem_dur)

# Dickey-Fuller test for variable
adf.test(Y_unem_dur, alternative="stationary", k=0)
adf.test(Y_unem_dur, alternative="explosive", k=0)

#summary(lm(dppi ~ lppi, na.action=na.omit))
#summary(lm(dppi ~ lppi + trend, na.action=na.omit))


# Augmented Dickey-Fuller test
adf.test(Y_unem_dur, alternative="stationary")

# DF and ADF tests for differenced variable
adf.test(d.Y_unem_dur, k=0)
adf.test(d.Y_unem_dur)

#taking the second difference of the original variable (first difference of the differenced variable)
d2.Y_unem_dur <- diff(d.Y_unem_dur) 

plot.ts(d2.Y_unem_dur)

# DF and ADF tests for differenced variable
adf.test(d2.Y_unem_dur, k=0)
adf.test(d2.Y_unem_dur)


# ACF and PACF
acf(Y_unem_dur)
pacf(Y_unem_dur)

acf(d2.Y_unem_dur)
pacf(d2.Y_unem_dur)

#Original variable is not stationar

#ARIMA(1,0,1) or ARMA(1,1)
arima(Y_unem_dur, order = c(1,0,1))

#ARIMA(1,2,1) or ARMA(1,1) - probably go with this model
arima(Y_unem_dur, order = c(1,2,1))

# ARIMA(1,0,1) or ARMA(1,1)
arima(d2.Y_unem_dur, order = c(1,0,1))

# ARIMA(1,0,10) or ARMA(1,1,10)
#arima(d.Y_unem_dur, order = c(1,0,10))

# ARIMA(1,0,5) or ARMA(1,1,5) - This is probably the model, I would go with. 
#arima(d.Y_unem_dur, order = c(1,0,5))

# ARIMA(1,2,1) forecasting
unem_dur.arima121 <- arima(Y_unem_dur, order = c(1,2,1))
unem_dur.pred1 <- predict(unem_dur.arima105, n.ahead=12)
plot.ts(Y_unem_dur)
lines(unem_dur.pred1$pred, col="blue")
lines(unem_dur.pred1$pred+2*unem_dur.pred1$se, col="red")
lines(unem_dur.pred1$pred-2*unem_dur.pred1$se, col="red")
unem_dur.pred1


#unem_dur.arima105 <- arima(d.Y_unem_dur, order = c(1,0,5))
#unem_dur.pred1 <- predict(unem_dur.arima105, n.ahead=100)
#plot.ts(d.Y_unem_dur)
#lines(unem_dur.pred1$pred, col="blue")
#lines(unem_dur.pred1$pred+2*unem_dur.pred1$se, col="red")
#lines(unem_dur.pred1$pred-2*unem_dur.pred1$se, col="red")


#Time Series Analysis - Savings rate
attach(savings_rate)

#Defining the variables
Y_savings_rate <- value
d.Y_savings_rate <- diff(Y_savings_rate)
t <- date

# Descriptive statistics and plotting the data
summary(Y_savings_rate)
summary(d.Y_savings_rate)

plot.ts(Y_savings_rate)
plot.ts(d.Y_savings_rate)

# Dickey-Fuller test for variable
adf.test(Y_savings_rate, alternative="stationary", k=0)
adf.test(Y_savings_rate, alternative="explosive", k=0)

#summary(lm(dppi ~ lppi, na.action=na.omit))
#summary(lm(dppi ~ lppi + trend, na.action=na.omit))


# Augmented Dickey-Fuller test
adf.test(Y_savings_rate, alternative="stationary")

# DF and ADF tests for differenced variable
adf.test(d.Y_savings_rate, k=0)
adf.test(d.Y_savings_rate)

# ACF and PACF
acf(Y_savings_rate)
pacf(Y_savings_rate)

acf(d.Y_savings_rate)
pacf(d.Y_savings_rate)

# ARIMA(1,0,3) or ARMA(1,1)
arima(Y_savings_rate, order = c(1,0,1))

# ARIMA(1,0,1) or ARMA(1,1)
#arima(Y_savings_rate, order = c(1,0,1))


# ARIMA(1,0,1) or ARMA(1,1,1)
arima(d.Y_savings_rate, order = c(1,0,1))


# After examining the ACF and PACF diagrams, we probably go with the a ARIMA(1,1,1) on the original variable.
# ARIMA(1,1,1) forecasting
savings_rate.arima101 <- arima(Y_savings_rate, order = c(1,1,1))
savings_rate.pred1 <- predict(savings_rate.arima101, n.ahead=12)
plot.ts(Y_savings_rate)
lines(savings_rate.pred1$pred, col="blue")
lines(savings_rate.pred1$pred+2*savings_rate.pred1$se, col="red")
lines(savings_rate.pred1$pred-2*savings_rate.pred1$se, col="red")

savings_rate.pred1
