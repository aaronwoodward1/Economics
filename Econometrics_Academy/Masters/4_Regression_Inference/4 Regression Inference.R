# Regression Inference

# Outline:
#   Shapiro-Wilk test for normality
#   t-test for coefficient significance
#   F-test for single coefficient significance
#   F-test for joint coefficient significance
#   F-test for overall significance
#   Lagrange Multiplier chi-square test for coefficient significance

# Data files:
#   wage1.csv

# setup
rm(list = ls()) 
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# Shapiro-Wilk test for normality --------------------------------------

# Wage example
wage1 <- read.csv(paste0(directory, "wage1.csv"))

wage1 %>% 
  select(wage, educ, exper, tenure, female) %>% 
  head(10)
wage1 %>% 
  select(wage, educ, exper, tenure, female) %>%
  str
wage1 %>% 
  select(wage, educ, exper, tenure, female) %>%
  stargazer(type = "text")

# Draw a histogram of variable
ggplot(data = wage1) +
  theme_bw() +
  geom_histogram(mapping = aes(x = wage), col = 'grey')

ggplot(data = wage1) +
  theme_bw() +
  geom_histogram(mapping = aes(x = lwage), col = 'grey')

# The Shapiro Wilk test for normality 
# H0: normality, Ha: non-normality
shapiro.test(wage1$wage)
shapiro.test(wage1$lwage)
# If p-value<0.05, the variable is not normally distributed.


# t-test for coefficient significance ----------------------------------

# Run Regression
model <- lm(wage ~ educ + exper + tenure + female, wage1)
summary(model)

# Hypothesis testing method 1: compare t-statistic with t-critical value(s)

# Display coefficient
(coefficient <- coef(model)["exper"])

# Display standard error
(se <- vcov(model) %>% # variance-covariance matrix
  diag %>% # extract diagonals
  sqrt %>% # calculate square-roots
  .["exper"]) # S.E. of the regressor "exper"

# Calculate t-statistic = coefficient/standard error
(tstat <- coefficient/se)

# Degrees of freedom (n-k-1)
(df_r <- model$df.residual)

# t-critical value at 5% significance level 
qt(p = 0.975, df = df_r, lower.tail = TRUE)

# If t-statistic is in the rejection region then reject null, 
# coefficient is significant


# Hypothesis testing method 2: compare p-value with significance level (5%)

# H0: beta[exper] = 0;   H1: beta[exper] not equal to 0 
# P-value for a two-tailed test of coefficient significance
2 * pt(q = abs(tstat), df = df_r, lower.tail = FALSE)
# If p-value < significance level then reject null, coefficient is significant

# H0: beta[exper]<=0;   H1: beta[exper]>0  
# P-value for an upper one-tailed test of positive coefficient
pt(q = tstat, df = df_r, lower.tail = FALSE)

# H0: beta[exper]>=0;   H1: beta[exper]<0  
# P-value for a lower one-tailed test of negative coefficient 
pt(q = tstat, df = df_r, lower.tail = TRUE)


# Hypothesis testing method 3: calculate confidence intervals

# Critical value at 5% significance level 
qt(p = 0.975, df = df_r)
             
# Lower bound at 95% confidence level
coefficient - 1.96 * se

# Upper bound at 95% confidence level
coefficient + 1.96 * se

# If confidence interval does not contain 0 then reject null, 
# coefficient is significant

# Critical value at 10% significance level
qt(p = 0.95, df = df_r)

# Lower bound at 90% confidence level 
coefficient - 1.65 * se

# Upper bound at 90% confidence level
coefficient + 1.65 * se

# The three methods of comparing t-statistic with critical values, 
# p-value with significance level, and confidence intervals lead to 
# the same conclusion.


# F-test for single coefficient significance ---------------------------

# F-test for single coefficient significance is an alternative to t-test.

# H0: beta[exper]=0 
# Restricted model: 
#   wage = alpha0 + alpha1*educ + alpha3*tenure + alpha4*female + e
model_r1 <- lm(wage ~ educ + tenure + female, wage1)
summary(model_r1)

# SSR for the restricted model ssr_r
(ssr_r1 <- sum(resid(model_r1)^2))

# Unrestricted model: 
# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + beta4*female + u
# Same as the model 'model' above
model_ur <- model
summary(model_ur)

# SSR for the unrestricted model = ssr_ur, q = number of restrictions and df_denom=n-k-1
(ssr_ur <- sum(resid(model_ur)^2))
(df_ur  <- model_ur$df.residual) # df_ur = n - k - 1
q <- 1

# Calculate F-stat using ssr_r and ssr_ur 
# F-stat = ((ssr_r-ssr_ur)/q) / (ssr_ur/(n-k-1))
(F_stat <- ((ssr_r1 - ssr_ur)/q) / (ssr_ur/df_ur))

# Calculate F-critical value
qf(p = 0.95, df1 = 1, df2 = df_ur)
# If F-stat > F-critical value then reject null, coefficient is significant

# p-value for F-test
(F_pvalue <- pf(q = F_stat, df1 = 1, df2 = df_ur, lower.tail = F))
# If F p-value<0.05 then reject null, coefficient is significant

# F-test for coefficient significance using R commands
linearHypothesis(model, "exper = 0")

# The F-statistic is different from the t-statistic for the coeff on exper 
# but the p-value is same for the F-test and t-test.

# Above is t-test or F-test for coefficient on exper = 0
# t-test for the variable experience = 0 is not what we want to test
t.test(wage1$exper, mu = 0)


# F-test for joint coefficient significance ----------------------------
  
# F-test for joint coefficient significance is used to test for several 
# coefficients to be jointly significantly different from zero.

# H0: beta[exper]=0 beta[tenure]=0
# Restricted model: wage = alpha0 + alpha1*educ + alpha4*female + e
model_r2 <- lm(wage ~ educ + female, wage1)
summary(model_r2)

# SSR for the restricted model ssr_r
ssr_r2 <- sum(resid(model_r2)^2)

# Unrestricted model: 
# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + beta4*female + u
# Same as the model 'model_ur'
summary(model_ur)

# SSR for the unrestricted model = ssr_ur, q = number of restrictions and
# df_denom = n-k-1
ssr_ur # residual sum of squares of the unrestricted model
df_ur # residual degrees of freedom of the unrestricted model
q <- 2

# Calculate F_stat using ssr_r and ssr_ur 
# F-stat=((ssr_r-ssr_ur)/q) / (ssr_ur/(n-k-1))
(F_stat <- ((ssr_r2 - ssr_ur)/q) / (ssr_ur/df_ur))

# F-critical value
qf(p = 0.05, df1 = q, df2 = df_ur, lower.tail = F)
# If F-stat > F-critical value then reject null, coefficients are jointly significant

# p-value for F-test
(F_pvalue <- pf(q = F_stat, df1 = q, df2 = df_ur, lower.tail = F))
# If F p-value<0.05 then reject null, coefficients are jointly significant

# F-test using R commands
linearHypothesis(model_ur, c("exper = 0", "tenure = 0"))


# F-test for overall significance of regression ------------------------

# F-test for overall significance is an F-test for all coefficients to be jointly
# significantly different from zero.

# H0: beta[educ]=0 beta[exper]=0 beta[tenure]=0 beta[female]=0
# Restricted model: wage = alpha0 + e
# Note that alpha0 = avg(wage)
(model_r3 <- lm(wage ~ 1, wage1))
stargazer(wage1["wage"], type = "text")
# SSR for the restricted model ssr_r
( ssr_r3 <- sum(resid(model_r3)^2) )

# Unrestricted model: 
# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + beta4*female + u
summary(model_ur)
summary(model_ur)$fstatistic

# SSR for the unrestricted model = ssr_ur, q = number of restrictions and
# df_denom=n-k-1
ssr_ur
df_ur
q <- 4

# Calculate F_stat using ssr_r and ssr_ur 
# F-stat=((ssr_r-ssr_ur)/q) / (ssr_ur/(n-k-1))
(F_stat <- ((ssr_r3 - ssr_ur)/q) / (ssr_ur/df_ur))

# Calculate F-statistic using R-squared (alternative way)
# F-stat = (R^2/k)/(1-R^2)/(n-k-1)
R2 <- summary(model_ur)$r.squared
k  <- model_ur$rank - 1
( F_stat1 <- (R2/k) / ((1-R2)/df_ur) )

# F-critical value 
qf(p = 0.95, df1 = q, df2 = df_ur)
# If F-stat > F-critical value then reject null, coefficients are jointly significant

# p-value for F-test
(F_pvalue <- pf(q = F_stat, df1 = q, df2 = df_ur, lower.tail = F))
# If F p-value<0.05 then reject null, coefficients are jointly significant

# F-test using R commands
linearHypothesis(model_ur, c("educ = 0", "exper = 0", 
                             "tenure = 0", "female = 0"))
# F-stat and F p-value are the same as reported in the regression output


# Lagrange Multiplier test for coefficient significance ----------------

# Lagrange Multiplier test is a chi-square test for several coefficient to 
# be jointly significantly different from zero.

# Unrestricted model: 
# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + beta4*female + u

# H0: beta[exper]=0 beta[tenure]=0
# Restricted model: wage = alpha0 + alpha1*educ + alpha4*female + e
# Regress dependent variable on the restricted set of independent variables
# Same as the model 'model_r2'
summary(model_r2)
q <- 2 # 2 restrictions
  
# Get residuals ehat
wage1$ehat <- residuals(model_r2) 

# Regress residuals ehat on all independent variables
model_ehat <- lm(ehat ~ educ + exper + tenure + female, wage1)

# LM statistic = n*(R_e)^2 = n*R-squared of above regression
n <- nobs(model_ehat)
R_e2 <- summary(model_ehat)$r.squared
(LM_stat <- n*R_e2)

# Critical value for chi-square distribution
qchisq(p = 0.95, df = q)
# If LM-stat > chi2 critical value then reject null, coefficients are 
# jointly significant

# P-value for chi-square distribution
pchisq(LM_stat, df = q, lower.tail = FALSE)
# If p-value < 0.05 then reject null, coefficients are jointly significant
