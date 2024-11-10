# OLS Asymptotics

# Outline:
#  OLS standard errors

# Data Files:
#  wage1.csv

# setup
rm(list = ls()) 
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# OLS standard errors -----------------------------------------------------

wage1 <- read.csv(paste0(directory, "wage1.csv"))

# Regression with full sample
model <- lm(wage ~ educ + tenure + exper, wage1)
summary(model)

(se1 <- vcov(model) %>% diag %>% sqrt %>% .["exper"])
(n1 <- nobs(model))

# Regression with half the sample
model_half <- lm(wage ~ educ + tenure + exper, 
                 slice(wage1, 1:(n1/2-1)))
summary(model_half)

(se2 <- vcov(model_half) %>% diag %>% sqrt %>% .["exper"])
(n2 <- nobs(model_half))

se1/se2
sqrt(n2/n1)

# These ratios are almost the same. 
# As the sample size n increases, standard errors change at the rate sqrt(1/n).
# With a larger sample size, standard errors are smaller, 
# leading to more significant coefficients. 
