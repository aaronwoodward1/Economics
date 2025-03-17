# U.S. Inflation Forecasting

## Install packages
#install.packages(c("fredr", "forecast"))

## Load required packages
library(fredr)
library(forecast)
library(ggplot2)
library(dplyr)
library(plotly)

fredr_set_key("658b4b77ed8f978ca0738e58f03d860f")

PCE_df <- fredr_series_observations("PCEC96", observation_start = as.Date("2017-01-01"), frequency = 'q') 
DPI_df <- fredr_series_observations("DSPIC96", observation_start = as.Date("2017-01-01"), frequency = 'q') 




# Write our eqns:

## Consumption fxn
# C = a + bYd
# a = autonomous consumption (constant)
# b = slope or MPC
# Yd = disposable income (Income - Taxes)

PCE_df$delta <- PCE_df$value - lag(PCE_df$value)
DPI_df$delta <- DPI_df$value - lag(DPI_df$value)

PCE_df <- PCE_df |>
  mutate(perchg = delta/value)

DPI_df <- DPI_df |>
  mutate(perchg = delta/value)


# MPC <- mean(PCE_df$delta/DPI_df$delta, na.rm = TRUE) # This is the slope or 'b' in the consumption fxn
MPC <- PCE_df$delta[32]/DPI_df$delta[32]
# a = 9.0 # based on last quarter PCE and DPI, and average MPC
a = if ((PCE_df$value[32] - (MPC*DPI_df$value[32])) > 0){
  PCE_df$value[32] - (MPC*DPI_df$value[32])
} else {
  1
}

# PCE_g <- mean(PCE_df$perchg, na.rm = TRUE)
# DPI_g <- mean(DPI_df$perchg, na.rm = TRUE)
PCE_g <- PCE_df$perchg[32]
DPI_g <- DPI_df$perchg[32]

viz_data <- data.frame(DPI_df$date, PCE_df$perchg, DPI_df$perchg)

# Visualize PCE and DPI growth rates
viz_data |>
  ggplot(aes(x=DPI_df.date,y=(PCE_df.perchg))) +
  geom_line(size = 1.2,alpha = 0.4) +
  theme_minimal()

# Forecast Consumption using growth rates, MPC, and Disposable Income from prev. quarter
C = a + (MPC * ((1+DPI_g)*tail((DPI_df$value[32]/1000))))
