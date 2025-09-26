

# Tariffs on Autos and Auto parts
# Use 232 and 301 steel and aluminum tariffs from 2018 as baseline pass-through
# Use Durable goods prices to assess changes in price.
# Periods Q4 2017 and Q4 2018


library(fredr)
library(forecast)
library(ggplot2)
library(dplyr)
library(plotly)

fredr_set_key("658b4b77ed8f978ca0738e58f03d860f")

MVP_df <- fredr_series_observations("CUUR0000SETA01", observation_start = as.Date("2017-01-01"), frequency = 'a') 
#DPI_df <- fredr_series_observations("DSPIC96", observation_start = as.Date("2017-01-01"), frequency = 'q') 


#Calc elasticity using CPI for new vehicles in 2023 & 2024
Q_sold_23 = 15.5
Q_sold_24 = 16.0
CPI_23 = MVP_df$value[[7]]
CPI_24 = MVP_df$value[[8]]
P_23 = 48700
P_24 = 49700
Qdem_delta = ((Q_sold_24 - Q_sold_23)/Q_sold_23)
CPI_delta = (CPI_24 - CPI_23)
Price_delta = (P_24 - P_23)/P_23
e = Qdem_delta / CPI_delta
e2 = Qdem_delta / Price_delta

#Get share steel imports from CN, CA, MX
CA_Mshr_stl = .192
MX_Mshr_stl = .069
CN_Mshr_stl = .022
EU_Mshr_stl = .191

#Get tariff rates for CN, CA, MX, EU
CA_tariff_stl = 0.25
MX_tariff_stl = 0.25
CN_tariff_stl = 0.35 # 25% under 232 and additional 10% under 301
EU_tariff_stl = 0.25


# Impact of steel on motor vehicle and parts prices
# MVP_Q3_2017 = MVP_df$value[4]
# MVP_Q3_2018 = MVP_df$value[8]


MVP_Pdlta_calc = (MVP_df$value[31] /  MVP_df$value[7]) 
MVP_Pdlta = .022

pass_thru_rate = MVP_Pdlta /((CA_Mshr_stl*CA_tariff_stl)+(MX_Mshr_stl*MX_tariff_stl)
                              +(CN_Mshr_stl*CN_tariff_stl)+(EU_Mshr_stl*EU_tariff_stl))









# Try HH appliances with CN tariffs

PCEappl_2018 = 69775
PCEappl_2020 = 77031

PCEappl_delta = (PCEappl_2020 - PCEappl_2018) / PCEappl_2018

## PCE for HH appliance on average increased by 10.4%

LE_df <- fredr_series_observations("CUSR0000SS30021", observation_start = as.Date("2017-01-01")) 


pass_thru_rate_appl = PCEappl_delta / CN_tariff_stl
pass_thru_tariff_cost = (PCEappl_2020 - PCEappl_2018) * pass_thru_rate_appl  #in millions

HHs_2020 = 126.8

HH_tariff_cost = pass_thru_tariff_cost / HHs_2020