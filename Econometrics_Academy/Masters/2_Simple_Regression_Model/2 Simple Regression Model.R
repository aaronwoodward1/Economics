# Simple Regression Model

# Outline:
#   Simple regression
#   Prediction after regression
#   Goodness-of-fit measure (R-squared)
#   Log forms: log-log and log-linear forms

# Data files:
#   CEOSAL1.csv
#   wage1.csv

# setup
rm(list = ls()) 
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Simple regression ----------------------------------------------------

# CEO salary example 
CEOSAL1 <- read.csv(paste0(directory, "CEOSAL1.csv"))

CEOSAL1 %<>% select(salary, roe)
str(CEOSAL1)
stargazer(CEOSAL1, type = "text")
head(CEOSAL1, 10)

# Exploring data
cor(CEOSAL1)
CEOSAL1 %<>% mutate(avg_salary = mean(salary))

# Simple regression: salary = beta0 + beta1*roe + u
model_CEOSAL1 <- lm(formula = salary ~ roe, data = CEOSAL1)
summary(model_CEOSAL1)
model_CEOSAL1$coefficients['roe']

# Plot the observations with a fitted line
plot(x = CEOSAL1$roe, y = CEOSAL1$salary)
abline(a = model_CEOSAL1$coefficients['(Intercept)'], 
       b = model_CEOSAL1$coefficients['roe'],
       col = 'red')
# or use ggplot package
ggplot(data = CEOSAL1, mapping = aes(x = roe, y = salary)) +
  theme_bw() + # set the theme of a plot
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Wage example
wage1 <- read.csv(paste0(directory, "wage1.csv"))
wage1 %<>% select(wage, educ)
str(wage1)
stargazer(wage1, type = "text")
head(wage1, 10)
model_wage1 <- lm(wage ~ educ, wage1)
summary(model_wage1)


# Prediction after regression ------------------------------------------

# CEO salary example
# Use the data 'CEOSAL1' and the model 'model_CEOSAL1'

# Predicted value for the dependent variable (salaryhat)
CEOSAL1 %<>% mutate(salaryhat = fitted(model_CEOSAL1))
stargazer(CEOSAL1, type = "text")
ggplot(data = CEOSAL1, mapping = aes(x = roe)) +
  geom_point(mapping = aes(y = salary, color = 'Salary - actual value')) +
  geom_point(mapping = aes(y = salaryhat, color = 'Salaryhat - predicted value')) + 
  xlab('Return on equity')

# Residuals
CEOSAL1 %<>% mutate(uhat = residuals(model_CEOSAL1))
stargazer(CEOSAL1, type = "text")
ggplot(CEOSAL1, aes(x = roe)) +
  geom_point(aes(y = salary, col = 'Salary - actual value')) +
  geom_point(aes(y = uhat, col = 'Residual uhat')) +
  xlab('Return on equity')

head(CEOSAL1, 10)

# Graph Actual and Predicted Values and Residuals
ggplot(CEOSAL1, aes(x = roe)) +
  geom_point(aes(y = salary, color = 'Salary - actual value')) +
  geom_point(aes(y = salaryhat, color = 'Salaryhat - predicted value')) +
  geom_point(aes(y = uhat, color = 'Residual uhat')) +
  geom_smooth(aes(y = salary, color = 'Fitted line'), 
              method = "lm", se = FALSE) +
  xlab('Return on equity')

# Wage example 
# Use the data 'wage1' and the model 'model_wage1'
wage1 %<>% mutate(wagehat = fitted(model_wage1),
                  uhat = resid(model_wage1))
ggplot(wage1, aes(x = educ)) +
  geom_point(aes(y = wage, color = 'Wage - actual value')) +
  geom_point(aes(y = wagehat, color = 'Wagehat - predicted value')) +
  geom_point(aes(y = uhat, color = 'Residual uhat')) +
  geom_smooth(aes(y = wage, color = 'Fitted line'), 
              method = "lm", se = FALSE)


# Goodness-of-fit measure (R-squared) ----------------------------------

# CEO salary example 
# Use the data 'CEOSAL1' and the model 'model_CEOSAL1'
# Use 'str' function to show what is included in the model or its summary
str(model_CEOSAL1)
str(summary(model_CEOSAL1))

# 'r.squared' in the summary is for R-squared
summary(model_CEOSAL1)$r.squared

# Use 'nobs' function to see the number of observations
nobs(model_CEOSAL1)

# Wage example 
# Use the data 'wage1' and the model 'model_wage1'
str(model_wage1)
str(summary(model_wage1))

summary(model_wage1)$r.squared
nobs(model_wage1)


# Log forms: log-log and log-linear form -------------------------------

# CEO salary example
CEOSAL2 <- read.csv(paste0(directory, "CEOSAL1.csv"))
CEOSAL2 %>% select(salary, lsalary, sales, lsales) %>% head(10)

# Linear form
model_CEOSAL2 <- lm(salary ~ sales, CEOSAL2)
summary(model_CEOSAL2)
ggplot(CEOSAL2, aes(x = sales, y = salary)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Create the log of a variable using the log function
# CEOSAL2 <- CEOSAL2 %>% mutate(lsales = log(sales),
#                               lsalary = log(salary))

# Log-log form
model_CEOSAL3 <- lm(lsalary ~ lsales, CEOSAL2)
summary(model_CEOSAL3)
# Graph
ggplot(CEOSAL2, aes(x = lsales, y = lsalary)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Linear-log form
model_CEOSAL4 <- lm(salary ~ lsales, CEOSAL2)
summary(model_CEOSAL4)
# Graph
ggplot(CEOSAL2, aes(x = lsales, y = salary)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Log-linear form
model_CEOSAL5 <- lm(lsalary ~ sales, CEOSAL2)
summary(model_CEOSAL5)
# Graph
ggplot(CEOSAL2, aes(x = sales, y = lsalary)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Wage Example 
wage2 <- read.csv(paste0(directory, "wage1.csv"))
select(wage2, wage, lwage, educ) %>% head(10)

# Linear form
model_wage2 <- lm(wage ~ educ, wage2)
summary(model_wage2)
# Graph
ggplot(wage2, aes(educ, wage)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Log-linear form
model_wage3 <- lm(lwage ~ educ, wage2)
summary(model_wage3)
# Graph
ggplot(wage2, aes(educ, lwage)) +
  geom_point() +
  geom_smooth(method = lm, se = F)
