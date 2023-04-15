#
# Loading libraries
library(dplyr)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(plotly)
library(fredr)
library(timetk)
library(treasuryTR)
library(scales)
library(ecm)
library(purrr)
library(ggthemes)
#FREDr Datasets
#USRECD <- U.S. Recessions
#JTSJOL <- Job Openings from BLS JOLTS
#ECIWAG <- ECI for private industry workers (wages and salaries)
#CPILFESL <- Core CPI (less energy and food prices)
#CORESTICKM159SFRBATL <- Core CPI (less energy and food prices)




#Enter FREDr API key 
fredr_set_key("658b4b77ed8f978ca0738e58f03d860f")

# Create a function for recession shading
add_rec_shade<-function(st_date,ed_date,shade_color="darkgray"){

st_date <- as.Date("2002-12-31")
ed_date<-as.Date(Sys.Date())

recession<-fredr(series_id = "USRECD",observation_start =
                   as.Date(st_date),observation_end =as.Date(ed_date))

recession$diff<-recession$value-lagpad(recession$value,k=1)
recession<-recession[!is.na(recession$diff),]
recession.start<-recession[recession$diff==1,]$date
recession.end<-recession[recession$diff==(-1),]$date

if(length(recession.start)>length(recession.end))
{recession.end<-c(recession.end,Sys.Date())}

if(length(recession.end)>length(recession.start))
{recession.start<-c(min(recession$date),recession.start)}

recs<-as.data.frame(cbind(recession.start,recession.end))
recs$recession.start<-
  as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
if(nrow(recs)>0)
{
  rec_shade<-geom_rect(data=recs, inherit.aes=F, aes(xmin=recession.start,
                                                     xmax=recession.end, ymin=-Inf, ymax=+Inf), fill=shade_color,alpha=0.5)
  return(rec_shade)
}
}

#Job Openings 
job_openings <- fredr(
  series_id = "JTSJOL",
  observation_start = as.Date("2003-01-01"),
  observation_end = as.Date(Sys.Date()),
  frequency = "m" # quarterly
  #units = "chg" # change over previous value
)

##Plot job openings
#ggplot(data = job_openings, aes(x = date, y = value, color = series_id, fill = 'green')) + 
#  add_rec_shade(min(job_openings$date),max(job_openings$date)) +
#  geom_line() + scale_color_manual(values = 'green') +
#  labs(x = "Observation Date", y = "Total Nonfarm Job Openings in the U.S.", color = "FRED Series")

#Employment Cost Index
eci <- fredr(
  series_id = "ECIWAG",
  observation_start = as.Date("2003-01-01"),
  observation_end = as.Date(Sys.Date()),
  frequency = "q", # quarterly
  units = "pc1" # percent change over previous value
)

# A few constants
openingsColor <- "#39F9AA"
eciColor <- "#F35386"

#Data cleaning and manipulation
#Renaming the value variable in both datasets
job_openings <- job_openings %>%
  rename(open_jobs=value)

eci <- eci %>%
  rename(eci_chg=value)

#Merging 2 dataframes
df1 <- merge(job_openings,eci, by="date")


# 2.0 DUAL Y-AXIS PLOTTING TRANSFORMER

# * Transformer Function ----
transformer_dual_y_axis <- function(data,
                                    primary_column, secondary_column,
                                    include_y_zero = FALSE) {
  
  # PARAMETER SETUP
  params_tbl <- data %>%
    summarise(
      max_primary   = max(!!enquo(primary_column)),
      min_primary   = min(!!enquo(primary_column)),
      max_secondary = max(!!enquo(secondary_column)),
      min_secondary = min(!!enquo(secondary_column))
    )
  
  if (include_y_zero) {
    params_tbl$min_primary   <- 0
    params_tbl$min_secondary <- 0
  }
  
  params_tbl <- params_tbl %>%
    mutate(
      scale = (max_secondary - min_secondary) / (max_primary - min_primary),
      shift = min_primary - min_secondary
    )
  
  # MAKE SCALER FUNCTIONS
  scale_func <- function(x) {
    x * params_tbl$scale - params_tbl$shift
  }
  
  inv_func <- function(x) {
    (x + params_tbl$shift) / params_tbl$scale
  }
  
  # RETURN
  ret <- list(
    scale_func = scale_func,
    inv_func   = inv_func,
    params_tbl = params_tbl
  )
  
  return(ret)
}

#* MAKE A Y-Axis Transformer ----
#transformer <- list(job_openings,eci) %>%
#  transformer_dual_y_axis(
#    primary_column   = job_openings$value,
#    secondary_column = eci$value,
#    include_y_zero   = TRUE
#  )
transformer <- df1 %>% 
  transformer_dual_y_axis(
    primary_column   = open_jobs,
    secondary_column = eci_chg,
    include_y_zero   = TRUE
  )


#* How the transformer works...----
df1 %>%
  pull(df$eci_chg) %>%
  transformer$inv_func() %>%
  transformer$scale_func()

# PLOTTING WITH DUAL Y-AXIS

# * PRIMARY Y-AXIS ----

g1 <- df1 %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = open_jobs), stat='identity', size=.1, fill=openingsColor, color="black", alpha=.4, show.legend = TRUE) +
  add_rec_shade(min(job_openings$date),max(job_openings$date))
  #scale_x_continuous(name = "Date")
  #geom_line(data = eci, aes(x = date, y = value), color = eciColor)
  
g2 <- g1 + 
  geom_line(
    aes(
      y = transformer$inv_func(eci_chg), 
      #color=eciColor
    ), 
    size=1,
    color=eciColor
  ) +
  
  #scale_x_continuous(name = "Date") +
  
  scale_y_continuous(
    labels   = scales::unit_format(unit="",big.mark =",", scale = 1e-3),
    name     = "Job openings (Millions)",
    sec.axis = sec_axis(
      trans = ~ transformer$scale_func(.),
      breaks = c(0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
      name  = "Change in ECI (YoY%)"
    )
  ) +
  
  theme_minimal() +
  
  labs(title="Job Openings and Employer Cost Index (ECI), Q4 2002 - Q3 2022",
       subtitle="Job openings (green bars), Percent change in ECI (red line)",
       caption = "Source: U.S. Bureau of Labor Statistics, retrieved from FRED, Federal Reserve Bank of St. Louis, February 3, 2023.") +
  
  theme(plot.title=element_text(vjust = 1.0),
        plot.subtitle=element_text(vjust = 0.5),
        plot.caption=element_text(hjust = -0.20, size = 8))
  

  

ggplotly(g2)