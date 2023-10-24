#' ---
#' title: Week Nine.5 Code
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---
#+ message = F, warning = F
library(tidyverse)
library(here)


df_fl <- read_csv(here("data_raw/data_fish_length.csv"))


# t.test comparison -------------------------------------------------------


m1 <- lm(length ~ lake,
         data = df_fl)

#calculate mean for lake a/b

mua <- mean(df_fl$length[df_fl$lake == "a"])
mub <- mean(df_fl$length[df_fl$lake == "b"])

mub-mua

summary(m1)

#apply t.test

t.test( df_fl$length[df_fl$lake == "b"], df_fl$length[df_fl$lake == "a"], var.equal = T)


# ANOVA Comparison --------------------------------------------------------


df_anova <- read_csv(here("data_raw/data_fish_length_anova.csv"))

# group means
v_mu <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

print(c(v_mu[1], # mu_a: should be identical to intercept
        v_mu[2] - v_mu[1], # mu_b - mu_a: should be identical to the slope for lakeb
        v_mu[3] - v_mu[1])) # mu_c - mu_a: should be identical to the slope for lakec

# lm() output
m2 <- lm(length ~ lake,
        data = df_anova)

summary(m2)

#aov

anova <- aov(formula = length ~ lake, data = df_anova)
summary(anova)
summary(m2)


# Different Preidcotr Types -----------------------------------------------

# convert the data format to tibble
iris <- as_tibble(iris)

# develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

summary(m_iris)
