library(networkD3)
library(devtools)
library(rjson)
library(blsAPI)

BLS_API_key <- ""

response <- blsAPI('CES0000000001')
json <- fromJSON(response)

data_list  <- json$Results$series[[1]]$data[-1]

jobs <- data.frame(matrix(unlist(data_list), ncol = 4, byrow = TRUE, 
                      dimnames = list(NULL, c("year", "period", "periodName", "value"))), 
                      stringsAsFactors = FALSE)


library(rvest)


url <- 'https://www.bls.gov/web/empsit/ceseesummary.htm'

data <- read_html(url)

url %>%
  read_html() %>%
  html_table() %>%
  .[[1]] -> df
###########################################

library(blsAPI)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set your BLS API key
bls_api_key <- "b7be4946aace4264af8c8cd83dbbb041"  # Replace with your actual API key

# Define the series ID for total nonfarm employment
# CES0000000001 = Total nonfarm employment, seasonally adjusted
series_id <- "CES0000000001"

# Define the time period (e.g., last 5 years)
start_year <- 2024
end_year <- 2025

# Create payload for the API request
payload <- list(
  'seriesid' = c(series_id),
  'startyear' = as.character(start_year),
  'endyear' = as.character(end_year),
  'registrationkey' = bls_api_key,
  'calculations' = TRUE  # Request 1-month change calculations
)

# Make the API request
response <- blsAPI(payload, api_version = 2)

# Parse the JSON response
data <- fromJSON(response)

## Testing code
series_data <- data$Results$series[[2]][[1]] #Code works!!!
df <- as.data.frame(series_data)
# names(df)[[6]][[1]] <- "change_1month"
# 
# df <- dplyr::rename(df,
#   change_1month = df$calculations$net_changes$[1])

# Check if the request was successful
# if (data$status == "REQUEST_SUCCEEDED") {
#   # Extract the time series data
  series_data <- data$Results$series[[1]]$data
#   series_data <- data$Results$series[[2]][[1]]
#   # Convert to a data frame
  df <- as.data.frame(series_data)
#   
  # Process the data
  df <- df %>%
    mutate(
      year = as.numeric(year),
      month = as.numeric(gsub("M", "", period)),
      date = as.Date(paste(year, month, "01", sep = "-")),
      value = as.numeric(value)
    ) %>%
    arrange(date) #%>%
    # dplyr::rename(
    #   change_1month = df$calculations$net_changes$1
    #   # calculations$net_changes$`1`
    # )
  
  
  # # Check if calculations are included in the response
  # if ("calculations" %in% names(df)) {
  #   # Extract 1-month changes from calculations
  #   df <- df %>%
  #     mutate(change_1month = sapply(calculations, function(x) {
  #       if (is.null(x$net_changes$`1`)) NA else as.numeric(x$net_changes$`1`)
  #     }))
  # } else {
  #   # Calculate 1-month changes manually
    df <- df %>%
      mutate(change_1month = value - lag(value))
  # }
  
  # Select and arrange the columns
  df <- df %>%
    select(date, year, month, value, change_1month) %>%
    arrange(desc(date))
  
  # View the first few rows
  print(head(df))
  
  # Create a plot of the 1-month changes
  ggplot(df, aes(x = date, y = change_1month)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Monthly Change in Total Nonfarm Employment",
      subtitle = paste(start_year, "-", end_year),
      x = "Date",
      y = "1-Month Change (thousands of jobs)",
      caption = "Source: Bureau of Labor Statistics"
    ) +
    theme_minimal()
  
  # Save the data to a CSV file
  write.csv(df, "bls_employment_changes.csv", row.names = FALSE)
  
} else {
  # Print error message
  cat("API request failed with status:", data$status, "\n")
  cat("Error message:", data$message, "\n")
}

###########################
library(blsAPI)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set your BLS API key
bls_api_key <- "YOUR_API_KEY_HERE"  # Replace with your actual API key

# Define the series IDs for different industries
# Each series ID represents a different industry from the CES survey
# Format: CES[supersector][industry][data_type]
series_ids <- c(
  "CES0000000001",  # Total nonfarm employment
  "CES0500000001",  # Total private employment
  "CES0600000001",  # Goods-producing
  "CES0700000001",  # Service-providing
  "CES0800000001",  # Private service-providing
  "CES1000000001",  # Mining and logging
  "CES2000000001",  # Construction
  "CES3000000001",  # Manufacturing
  "CES4000000001",  # Trade, transportation, and utilities
  "CES4142000001",  # Wholesale trade
  "CES4200000001",  # Retail trade
  "CES4300000001",  # Transportation and warehousing
  "CES5000000001",  # Information
  "CES5500000001",  # Financial activities
  "CES6000000001",  # Professional and business services
  "CES6500000001",  # Education and health services
  "CES7000000001",  # Leisure and hospitality
  "CES8000000001",  # Other services
  "CES9000000001"   # Government
)

# Create a dictionary that maps series IDs to industry names
industry_names <- c(
  "CES0000000001" = "Total Nonfarm",
  "CES0500000001" = "Total Private",
  "CES0600000001" = "Goods-producing",
  "CES0700000001" = "Service-providing",
  "CES0800000001" = "Private Service-providing",
  "CES1000000001" = "Mining and Logging",
  "CES2000000001" = "Construction",
  "CES3000000001" = "Manufacturing",
  "CES4000000001" = "Trade, Transportation, and Utilities",
  "CES4142000001" = "Wholesale Trade",
  "CES4200000001" = "Retail Trade",
  "CES4300000001" = "Transportation and Warehousing",
  "CES5000000001" = "Information",
  "CES5500000001" = "Financial Activities",
  "CES6000000001" = "Professional and Business Services",
  "CES6500000001" = "Education and Health Services",
  "CES7000000001" = "Leisure and Hospitality",
  "CES8000000001" = "Other Services",
  "CES9000000001" = "Government"
)

# Define the time period
start_year <- 2024
end_year <- 2025

# Create payload for the API request
payload <- list(
  'seriesid' = series_ids,
  'startyear' = as.character(start_year),
  'endyear' = as.character(end_year),
  'registrationkey' = bls_api_key,
  'calculations' = TRUE  # Request 1-month change calculations
)

response <- blsAPI(payload, api_version = 2)

# Parse the JSON response
data <- fromJSON(response)

# Check if the request was successful
if (data$status == "REQUEST_SUCCEEDED") {
  # Initialize an empty list to store data frames for each industry
  industry_dfs <- list()
  
  # Process data for each series/industry
  for (i in 1:length(data$Results$series)) {
    series <- data$Results$series[[i]]
    # series_id <- series$seriesID
    series_id <-  data$Results$series$seriesID
    series_name <- industry_names[series_id]
    
    # Extract the data for this series
    series_data <- data$Results$series$data
    # series_data <- series[[3]]
    # 
    # # Convert to a data frame
    # df <- as.data.frame(series_data)
  }
    # Process the data
    df <- df %>%
      mutate(
        year = as.numeric(year),
        month = as.numeric(gsub("M", "", period)),
        date = as.Date(paste(year, month, "01", sep = "-")),
        value = as.numeric(value),
        industry = series_name,
        series_id = series_id
      ) %>%
      arrange(date)
  }
    # Check if calculations are included in the response
    # if ("calculations" %in% names(df) && !all(is.na(df$calculations))) {
    #   # Extract 1-month changes from calculations
    #   df <- df %>%
    #     mutate(change_1month = sapply(calculations, function(x) {
    #       if (is.null(x) || is.null(x$net_changes) || is.null(x$net_changes$`1`)) {
    #         NA
    #       } else {
    #         as.numeric(x$net_changes$`1`)
    #       }
    #     }))
    # }  else {
      # Calculate 1-month changes manually
      df <- df %>%
        mutate(change_1month = value - lag(value))
  
    
    # Select and arrange the columns
    df <- df %>%
      select(date, year, month, industry, series_id, value, change_1month) %>%
      arrange(date)
    
    # Add to the list
    industry_dfs[[i]] <- df
  }
  
  # Combine all industry data frames
  all_industries_df <- bind_rows(industry_dfs)
  
  # View the first few rows
  print(head(all_industries_df))
  
  # Create a plot of the 1-month changes for selected industries
  # Choose a subset of major industries for clearer visualization
  major_industries <- c("Total Nonfarm", "Manufacturing", "Construction", 
                        "Professional and Business Services", "Leisure and Hospitality")
  
  plot_data <- all_industries_df %>%
    filter(industry %in% major_industries)
  
  # Create the plot
  ggplot(plot_data, aes(x = date, y = change_1month, color = industry)) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = "Monthly Change in Employment by Industry",
      subtitle = paste(start_year, "-", end_year),
      x = "Date",
      y = "1-Month Change (thousands of jobs)",
      color = "Industry",
      caption = "Source: Bureau of Labor Statistics"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Create a more detailed plot for the most recent month
  most_recent_date <- max(all_industries_df$date, na.rm = TRUE)
  
  recent_data <- all_industries_df %>%
    filter(date == most_recent_date) %>%
    # Remove aggregates for this visualization
    filter(!industry %in% c("Total Nonfarm", "Total Private", 
                            "Goods-producing", "Service-providing", 
                            "Private Service-providing")) %>%
    arrange(desc(change_1month))
  
  # Create a horizontal bar chart
  ggplot(recent_data, aes(x = reorder(industry, change_1month), y = change_1month)) +
    geom_bar(stat = "identity", 
             aes(fill = change_1month > 0),
             show.legend = FALSE) +
    scale_fill_manual(values = c("TRUE" = "green3", "FALSE" = "red3")) +
    geom_text(aes(label = round(change_1month, 1)), 
              hjust = ifelse(recent_data$change_1month > 0, -0.1, 1.1),
              color = ifelse(recent_data$change_1month > 0, "darkgreen", "darkred")) +
    coord_flip() +
    labs(
      title = paste("Employment Change by Industry -", format(most_recent_date, "%B %Y")),
      x = "",
      y = "1-Month Change (thousands of jobs)",
      caption = "Source: Bureau of Labor Statistics"
    ) +
    theme_minimal()
  
  # Save the data to a CSV file
  write.csv(all_industries_df, "bls_employment_changes_by_industry.csv", row.names = FALSE)
  
} else {
  # Print error message
  cat("API request failed with status:", data$status, "\n")
  cat("Error message:", paste(data$message, collapse = "\n"), "\n")
}

######

library(blsAPI)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set your BLS API key
bls_api_key <- "YOUR_API_KEY_HERE"  # Replace with your actual API key

# Define the series IDs for different industries
# Each series ID represents a different industry from the CES survey
# Format: CES[supersector][industry][data_type]
series_ids <- c(
  "CES0000000001",  # Total nonfarm employment
  "CES0500000001",  # Total private employment
  "CES0600000001",  # Goods-producing
  "CES0700000001",  # Service-providing
  "CES0800000001",  # Private service-providing
  "CES1000000001",  # Mining and logging
  "CES2000000001",  # Construction
  "CES3000000001",  # Manufacturing
  "CES4000000001",  # Trade, transportation, and utilities
  "CES4142000001",  # Wholesale trade
  "CES4200000001",  # Retail trade
  "CES4300000001",  # Transportation and warehousing
  "CES5000000001",  # Information
  "CES5500000001",  # Financial activities
  "CES6000000001",  # Professional and business services
  "CES6500000001",  # Education and health services
  "CES7000000001",  # Leisure and hospitality
  "CES8000000001",  # Other services
  "CES9000000001"   # Government
)

# Create a dictionary that maps series IDs to industry names
industry_names <- c(
  "CES0000000001" = "Total Nonfarm",
  "CES0500000001" = "Total Private",
  "CES0600000001" = "Goods-producing",
  "CES0700000001" = "Service-providing",
  "CES0800000001" = "Private Service-providing",
  "CES1000000001" = "Mining and Logging",
  "CES2000000001" = "Construction",
  "CES3000000001" = "Manufacturing",
  "CES4000000001" = "Trade, Transportation, and Utilities",
  "CES4142000001" = "Wholesale Trade",
  "CES4200000001" = "Retail Trade",
  "CES4300000001" = "Transportation and Warehousing",
  "CES5000000001" = "Information",
  "CES5500000001" = "Financial Activities",
  "CES6000000001" = "Professional and Business Services",
  "CES6500000001" = "Education and Health Services",
  "CES7000000001" = "Leisure and Hospitality",
  "CES8000000001" = "Other Services",
  "CES9000000001" = "Government"
)


# Define the time period
start_year <- 2024
end_year <- 2025

# Create payload for the API request
payload <- list(
  'seriesid' = series_ids,
  'startyear' = as.character(start_year),
  'endyear' = as.character(end_year),
  'registrationkey' = bls_api_key,
  'calculations' = TRUE  # Request 1-month change calculations
)

response <- blsAPI(payload, api_version = 2)

# Parse the JSON response
data <- fromJSON(response)


# for(i in 1:length(data$Results$series$data)){ #CODE WORKS!!!
# nam <- paste0(stresy_data$dataset_name[[i]],"_df")
# nam <- series_ids[[i]]
# assign(nam, data$Results$series$data[[i]] %>%
#          mutate(.,
#                 series_id = series_ids[[i]],
#                 industry = industry_names[[i]]))
# 
# s <- data$Results$series$data[[i]]
# series_ids[[i]] <- as.data.frame(series_ids[[i]])
# }

# Code works!!
series_data <- data$Results$series
series_data <- as.data.frame(series_data)

df <-series_data |>
  unnest(data)

df <- df |>
  unnest(calculations)

df <- df |>
  unnest(net_changes, pct_changes)

df <- dplyr::rename(df, 
                    one_month_net_chg = `1`,
                    three_month_net_chg = `3`,
                    six_month_net_chg = `6`,
                    twelve_month_net_chg = `12`,
                    one_month_pct_chg = `11`,
                    three_month_pct_chg = `31`,
                    six_month_pct_chg = `61`,
                    twelve_month_pct_chg = `121`)

# Industry column based on seriesID
df <- df |>
  mutate(industry = case_when(seriesID == "CES0000000001" ~ "Total Nonfarm",
                              seriesID == "CES0500000001" ~ "Total Private",
                              seriesID == "CES0600000001" ~ "Goods-producing",
                              seriesID == "CES0700000001" ~ "Service-providing",
                              seriesID == "CES0800000001" ~ "Private Service-providing",
                              seriesID == "CES1000000001" ~ "Mining and Logging",
                              seriesID == "CES2000000001" ~ "Construction",
                              seriesID == "CES3000000001" ~ "Manufacturing",
                              seriesID == "CES4000000001" ~ "Trade, Transportation, and Utilities",
                              seriesID == "CES4142000001" ~ "Wholesale Trade",
                              seriesID == "CES4200000001" ~ "Retail Trade",
                              seriesID == "CES4300000001" ~ "Transportation and Warehousing",
                              seriesID == "CES5000000001" ~ "Information",
                              seriesID == "CES5500000001" ~ "Financial Activities",
                              seriesID == "CES6000000001" ~ "Professional and Business Services",
                              seriesID == "CES6500000001" ~ "Education and Health Services",
                              seriesID == "CES7000000001" ~ "Leisure and Hospitality",
                              seriesID == "CES8000000001" ~ "Other Services",
                              seriesID == "CES9000000001" ~ "Government"))

# Assigning Industry Groups
df <- df |>
  mutate(industry_group = case_when(seriesID == "CES0000000001" ~ "Total Nonfarm",
                                    seriesID == "CES0500000001" ~ "Total Private",
                                    seriesID == "CES0600000001" ~ "Goods-producing",
                                    seriesID == "CES0700000001" ~ "Service-providing",
                                    seriesID == "CES0800000001" ~ "Private Service-providing",
                                    seriesID == "CES1000000001" ~ "Mining and Logging",
                                    seriesID == "CES2000000001" ~ "Construction",
                                    seriesID == "CES3000000001" ~ "Manufacturing",
                                    seriesID == "CES4000000001" ~ "Trade, Warehousing, Transportation, and Utilities",
                                    seriesID == "CES4142000001" ~ "Wholesale and Retail Trade",
                                    seriesID == "CES4200000001" ~ "Wholesale and Retail Trade",
                                    seriesID == "CES4300000001" ~ "Transportation and Warehousing", # Omit from analysis
                                    seriesID == "CES5000000001" ~ "Information",
                                    seriesID == "CES5500000001" ~ "Financial Activities",
                                    seriesID == "CES6000000001" ~ "Professional and Business Services",
                                    seriesID == "CES6500000001" ~ "Education and Health Services",
                                    seriesID == "CES7000000001" ~ "Leisure and Hospitality",
                                    seriesID == "CES8000000001" ~ "Other Services",
                                    seriesID == "CES9000000001" ~ "Government"))


df <- df |>
  mutate(industry_group = str_wrap(industry_group, width = 30))

# Shaping dataframe

# Creating date column and transforming value column
df <- df %>%
  mutate(
    year = as.numeric(year),
    month = as.numeric(gsub("M", "", period)),
    date = as.Date(paste(year, month, "01", sep = "-")),
    value = as.numeric(value),
    one_month_net_chg = as.numeric(one_month_net_chg),
    one_month_pct_chg = as.numeric(one_month_pct_chg),
    three_month_net_chg = as.numeric(three_month_net_chg),
    three_month_pct_chg = as.numeric(three_month_pct_chg),
    six_month_net_chg = as.numeric(six_month_net_chg),
    six_month_pct_chg = as.numeric(six_month_pct_chg),
    twelve_month_net_chg = as.numeric(twelve_month_net_chg),
    twelve_month_pct_chg = as.numeric(twelve_month_pct_chg),
    mthyr = paste(periodName,"",year)
  ) %>%
  arrange(date)

df$mthyr <- as.Date(df$mthyr, format = "%B %Y")

nf_jobs_df <- subset(df, industry_group %in% 'Total Nonfarm')
nf_jobs_df <- dplyr::rename(nf_jobs_df,
                     nf_one_mnth_netchg = one_month_net_chg)
nf_jobs_df <- nf_jobs_df |> 
  select(date,nf_one_mnth_netchg)

# Altering dataframe based on time horizon
## 1-month
df1 = df |>
  select(date, periodName, year, mthyr, industry_group, value, one_month_net_chg, one_month_pct_chg) |>
  group_by(industry_group, periodName, year, date, mthyr) |>
  dplyr::summarise(one_month_net_chg = sum(one_month_net_chg),
                   value = sum(value),
                   # one_month_pct_chg = weighted.mean(value, one_month_net_chg),
            .groups = 'drop')

df1 <- subset(df1, !(industry_group %in% c('Total Nonfarm','Total Private','Goods-producing',
                                 'Service-providing','Private Service-providing',
                                 'Transportation and Warehousing')))

df1 <- subset(df1, df1$date > '2024-05-01')

# Adding Nonfarm jobs
df1 <- merge(df1, nf_jobs_df, by = "date")

totals <- df1 |>
  group_by(date) |>
  summarize(total = mean(nf_one_mnth_netchg))

# df1 <- df1 |>
#   mutate(
#     nonfarm_one_month_netchg = rep(nf_jobs_df$one_month_net_chg, each = df1$date)
#   )
# 
# df1$nf_one_month_netchg <-rep(nf_jobs_df$one_month_net_chg, each=df$date)

## 3-month
df3 <- df |>
  select(., date, value, three_month_net_chg, three_month_pct_chg)

## 6-month
df6 <- df |>
  select(., date, value, six_month_net_chg, six_month_pct_chg)

## 12-month
df12 <- df |>
  select(., date, value, twelve_month_net_chg, twelve_month_pct_chg)

# Bump chart viz
library(tidyverse)
library(paletteer)
# library(stringr)
library(scales)
library(ggthemes)
# ggplot(df1, 
#        aes(x = date,
#            node = industry_group,
#            fill = industry_group,
#            value = one_month_net_chg)) +
#   geom_sankey_bump(space = 0, 
#                    type = "alluvial", 
#                    color = "transparent", 
#                    smooth = 6) +
#   scale_fill_paletteer_d("MetBrewer::Signac", alpha = 0.8) +
#   scale_x_continuous(breaks = scales::pretty_breaks(), expand = c(0,0)) + 
#   scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
#   theme_sankey_bump(base_size = 16) +
#   labs(x = mthyr,
#        y = "1-Month Change (thousands of jobs)",
#        fill = "Industry",
#        title = "Monthly Change in Total Nonfarm Employment",
#        subtitle = "January 2024-May 2025",
#        caption = "Source: Bureau of Labor Statistics") +
#   theme(legend.position = "top",
#         legend.title = element_text(hjust = 0.5),
#         legend.title.position = "top",
#         plot.caption = element_text(hjust = 0, size = 10, face = "italic"))

pal <- c("#4269d0", # blue
         "#efb118", # orange
         "#ff725c", # red
         "#6cc5b0", # cyan
         "#3ca951", # green
         "#f765b8", # pink
         "#a463f2", # purple
         "#40E0D0", # turquoise
         "#97bbf5", # light blue
         "#9c6b4e", # brown
         "#9498a0", # gray
         "#000080", # navy
        )


options(repr.plot.width =10, repr.plot.height =8)

p1 <- ggplot(df1,aes(x=date, y=one_month_net_chg, fill=industry_group)) +
        geom_bar(position="stack", stat="identity", color="black", width = 20.0) + 
        # geom_col(position = "dodge") +
        # geom_text(aes(label = nf_one_mnth_netchg), vjust=-0.2) +
        scale_fill_manual(values=pal) +
        scale_color_manual(values=pal) +
        labs(x = "Month",
          y = "1-Month Change (thousands of jobs)",
          fill = "Industry",
          title = "Monthly Change in Total Nonfarm Employment",
          subtitle = "May 2024-April 2025",
          caption = "Source: Bureau of Labor Statistics") +
        theme(legend.position = "top",
          legend.title = element_text(hjust = 0.0),
          legend.title.position = "left",
          legend.key.size = unit(0.3, "cm"),
          legend.key.width = unit(0.3,"cm"),
          plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +
        theme_minimal() 
  
p1
  
p1_annotate <-  p1 + geom_label(aes(date, total, label = total, fill = NULL), 
                               inherit.aes = FALSE, 
                               vjust = -4.0,
                               data = totals)

p1_annotate

p2 <- p1 + facet_wrap(~ industry_group, ncol = 5)
p2 


########################
####BLS SCRAPE R ######

library(blscrapeR)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(ggthemes)


# Grab several data sets from the BLS at onece.
# NOTE on series IDs: 
# EMPLOYMENT LEVEL - Civilian labor force - LNS12000000
# UNEMPLOYMENT LEVEL - Civilian labor force - LNS13000000
# UNEMPLOYMENT RATE - Civilian labor force - LNS14000000

#Test
df <- bls_api(c("LNS12000000", "LNS13000000", "LNS14000000"),
              startyear = 2008, endyear = 2017, Sys.getenv("b7be4946aace4264af8c8cd83dbbb041")) %>%
  # Add time-series dates
  dateCast()


# Dataframe
df <- bls_api(c("CES0000000001",  # Total nonfarm employment
                "CES0500000001",  # Total private employment
                "CES0600000001",  # Goods-producing
                "CES0700000001",  # Service-providing
                "CES0800000001",  # Private service-providing
                "CES1000000001",  # Mining and logging
                "CES2000000001",  # Construction
                "CES3000000001",  # Manufacturing
                "CES4000000001",  # Trade, transportation, and utilities
                "CES4142000001",  # Wholesale trade
                "CES4200000001",  # Retail trade
                "CES4300000001",  # Transportation and warehousing
                "CES5000000001",  # Information
                "CES5500000001",  # Financial activities
                "CES6000000001",  # Professional and business services
                "CES6500000001",  # Education and health services
                "CES7000000001",  # Leisure and hospitality
                "CES8000000001",  # Other services
                "CES9000000001" ),  # Government,
              startyear = 2024, endyear = 2025, Sys.getenv("b7be4946aace4264af8c8cd83dbbb041")) %>%
  # Add time-series dates
  dateCast()

df1 <- df |>
  mutate(industry_group = case_when(seriesID == "CES0000000001" ~ "Total Nonfarm",
                                    seriesID == "CES0500000001" ~ "Total Private",
                                    seriesID == "CES0600000001" ~ "Goods-producing",
                                    seriesID == "CES0700000001" ~ "Service-providing",
                                    seriesID == "CES0800000001" ~ "Private Service-providing",
                                    seriesID == "CES1000000001" ~ "Mining and Logging",
                                    seriesID == "CES2000000001" ~ "Construction",
                                    seriesID == "CES3000000001" ~ "Manufacturing",
                                    seriesID == "CES4000000001" ~ "Trade, Warehousing, Transportation, and Utilities",
                                    seriesID == "CES4142000001" ~ "Wholesale and Retail Trade",
                                    seriesID == "CES4200000001" ~ "Wholesale and Retail Trade",
                                    seriesID == "CES4300000001" ~ "Transportation and Warehousing", # Omit from analysis
                                    seriesID == "CES5000000001" ~ "Information",
                                    seriesID == "CES5500000001" ~ "Financial Activities",
                                    seriesID == "CES6000000001" ~ "Professional and Business Services",
                                    seriesID == "CES6500000001" ~ "Education and Health Services",
                                    seriesID == "CES7000000001" ~ "Leisure and Hospitality",
                                    seriesID == "CES8000000001" ~ "Other Services",
                                    seriesID == "CES9000000001" ~ "Government"))

df1 <- subset(df1, df1$date > '2024-05-01')

df1 <- subset(df1, !(industry_group %in% c('Total Nonfarm','Total Private','Goods-producing',
                                           'Service-providing','Private Service-providing',
                                           'Transportation and Warehousing')))

df1 <- df1 |>
  arrange(industry_group, date) |>
  group_by(industry_group) |>
  mutate(change_1month = value-lag(value, n=1))

df1 <- df1 |> 
  select(industry_group, date, periodName, value, change_1month)        
         
         
df1 <- df1 |>
  group_by(industry_group, date, periodName) |>
   dplyr::summarise(value = sum(value),
                    change_1month = sum(change_1month),
  #                  # one_month_pct_chg = weighted.mean(value, one_month_net_chg),
                  .groups = 'drop') 
# order_by(date)

# df1 <- df1 |> 
#   mutate(change_1month = value-lag(value, n=1L))
#   
# df1 <- subset(df1, df1$date > '2024-06-01')



pal <- c("#4269d0", # blue
         "#efb118", # orange
         "#ff725c", # red
         "#6cc5b0", # cyan
         "#3ca951", # green
         "#f765b8", # pink
         "#a463f2", # purple
         "#40E0D0", # turquoise
         "#97bbf5", # light blue
         "#9c6b4e", # brown
         "#9498a0", # gray
         "#000080" # navy
)


options(repr.plot.width =10, repr.plot.height =8)

p1 <- ggplot(df1,aes(x=date, y=change_1month, fill=industry_group)) +
  geom_bar(position="stack", stat="identity", color="black", width = 20.0) + 
  # geom_col(position = "dodge") +
  # geom_text(aes(label = nf_one_mnth_netchg), vjust=-0.2) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  scale_x_continuous(breaks = NULL) +
  labs(x = "Month",
       y = "1-Month Change (thousands of jobs)",
       fill = "Industry",
       title = "Monthly Change in Total Nonfarm Employment",
       subtitle = "May 2024-April 2025",
       caption = "Source: Bureau of Labor Statistics") +
  theme(legend.position = "top",
        legend.title = element_text(hjust = 0.0),
        legend.title.position = "left",
        legend.key.size = unit(0.3, "cm"),
        legend.key.width = unit(0.3,"cm"),
        plot.caption = element_text(hjust = 0, size = 8, face = "italic")) +
  theme_minimal() 

p1
