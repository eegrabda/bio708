#' ---
#' title: Model Comparison
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---
#+ message = F, warning = F
library(tidyverse)
library(here)


# Model Fit ---------------------------------------------------------------
#R-squared

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)

# create a design matrix
X <- model.matrix(~x1)

# expected values of y is a function of x
# %*% means matrix multiplication
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra
mu <- drop(X %*% b) #drop vectorizes matrix)

# add noise
y <- rnorm(n = n, mean = mu, sd = 0.5)

# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point(color="steelblue")

#lm - true model
m1 <- lm(y~x1,df0)
summary(m1)

#add bad predictor

df0 <- df0 %>% 
  mutate(x2 = rnorm(n=n))

m2 <- lm(y~x1+ x2,df0)
summary(m2)


summary(m1);summary(m2)


# Likelihood Ratio Test ---------------------------------------------------

logLik(m1)
logLik(m2)


anova(m1,m2, test = "Chisq")

#AIC
# AIC: correct model
AIC(m1)
AIC(m2)
