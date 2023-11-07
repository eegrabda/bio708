#' ---
#' title: Week Twelve Code
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---
#+ message = F, warning = F
library(tidyverse)
library(here)


# Proportional Data -------------------------------------------------------

df_mussel <- read_csv(here("data_raw/data_mussel.csv"))


# calculate the proportion of fertilized eggs
df_mussel <- df_mussel %>% 
  mutate(prop_fert = n_fertilized / n_examined)

# plot
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point(color = "steelblue") +
  labs(y = "Proportion of Eggs Fertilized",
       x = "Mussel Density")

# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_p = seq(-10, 10, length = 100),
                  p = exp(logit_p) / (1 + exp(logit_p)))

df_test %>% 
  ggplot(aes(x = logit_p,
             y = p)) +
  geom_point(color = "steelblue") +
  geom_line() +
  labs(y = "p",
       x = "logit(p)")


m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")


df_pred <- tibble(density = seq(min(df_mussel$density),
                                max(df_mussel$density),
                                length = 100)) %>% 
  mutate(logit_y_hat = predict(m_binom, newdata = .), y_hat = exp(logit_y_hat)/(1+exp(logit_y_hat)))


df_mussel %>% 
  ggplot(aes(x= density,
         y= prop_fert))+
  geom_point(color="steelblue")+
  geom_line(data = df_pred, aes(y = y_hat))
  


# Laboratory --------------------------------------------------------------


df_fish_glm <- read_csv(here("data_raw/data_vpart.csv"))

glm_fish <- glm(n_sp ~ distance + cat_area + hull_area, data = df_fish_glm, family = "poisson")
summary(glm_fish)

#small ex
# 
# x1 <- rnorm(100,0,5)
# x2 <- rnorm(100,0,10)
# 
# sd(x1);sd(x2)
# 
# z1 <- x1/sd(x1)
# z2 <- x2/sd(x2)
# 
# z1;z2
# sd(z1);sd(z2)
# 
# y1 <- rnorm(100,10,5)
# y2 <- rnorm(100,2,5)
# mean(y1);mean(y2)

#mean(scale(y1));sd(scale(y1));mean(scale(y2));sd(scale(y2))

#effect size 2

glm_fish_scale <- glm(n_sp ~ scale(distance) + scale(cat_area) + scale(hull_area), data = df_fish_glm, family = "poisson")
summary(glm_fish_scale)
