# U.S. Inflation Forecasting

## Install packages
install.packages(c("fredr", "forecast"))

## Load required packages
library(fredr)
library(forecast)
library(ggplot2)
library(dplyr)
library(plotly)

fredr_set_key("658b4b77ed8f978ca0738e58f03d860f")

## Retrieving inflation indicators (CPI, PCE)
cpi <- fredr_series_observations("CPIAUCSL", observation_start = as.Date("1965-01-01"), frequency = 'm', units = 'pc1') # Growth rate of CPI
pce <- fredr_series_observations("PCEPILFE", observation_start = as.Date("1965-01-01"), frequency = 'm', units = 'pc1') # Growth rate of PCE
pce_xhome <- fredr_series_observations("IA001176M", observation_start = as.Date("1965-01-01"), frequency = 'm', units = 'pc1') # Growth rate of PCE x Housing

cpi=rename(cpi,cpi=value)
pce=rename(pce,pce=value)
pce_xhome=rename(pce_xhome,pce_xhome=value)

## Dropping useless columns
cols_drop <- c('series_id', 'realtime_start', 'realtime_end')

cpi <- select(cpi,-c(cols_drop))
pce <- select(pce,-c(cols_drop))
pce_xhome <- select(pce_xhome,-c(cols_drop))

# Creating a dataframe
df <- inner_join(cpi,pce, by='date')
df <- inner_join(df,pce_xhome, by='date')

# Housing as pce minus pce_xhome
df <- df %>%
  mutate(., housing = pce - pce_xhome)

## CPI Forecasting
cpi_ts <- ts(df[,"cpi"], start = c(1965, 1), frequency = 12)

### Plot time series
p <- ggplot(df, aes(x = date, y = cpi)) +
  geom_line() +
  labs(title = "CPI Over Time", y = "CPI", x = "Year") +
  theme_bw()

ggplotly(p)

### Plot ACF
acf(cpi_ts, main = "ACF of CPI")

### Plot PACF
pacf(cpi_ts, main = "PACF of CPI")


# Fit ARIMA model
arima_model <- auto.arima(cpi_ts)

# Print model summary
summary(arima_model)

# Generate 24-month forecast CPI
cpi_fcst <- forecast(arima_model, 24)
plot(cpi_fcst)
axis(side=2, at=seq(0, 15, by=.5))

# Visualize via ggplot

## PCE (Core Inflation) Forecasting
pce_ts <- ts(df[,"pce"], start = c(1965, 1), frequency = 12)


### Plot time series
p <- ggplot(df, aes(x = date, y = pce)) +
  geom_line() +
  labs(title = "PCE (Core Inflation) Over Time", y = "Core PCE", x = "Year") +
  theme_bw()

ggplotly(p)

### Plot ACF
acf(pce_ts, main = "ACF of CPI")

### Plot PACF
pacf(pce_ts, main = "PACF of CPI")

# Fit ARIMA model
arima_model <- auto.arima(pce_ts)

# Print model summary
summary(arima_model)

# Generate 24-month forecast CPI
pce_fcst <- forecast(arima_model, 24)
plot(pce_fcst)
axis(side=2, at=seq(0, 12, by=.5))
