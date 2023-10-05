#' ---
#' title: Week Five Code
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---
#+ message = F, warning = F
library(tidyverse)
library(patchwork)
setwd("C:/Git/bio708")
# Garden Plants -----------------------------------------------------------

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") # unit

# nrow() returns the number of rows
# while piping, "." refers to the dataframe inherited 
# i.e., nrow(.) counts the number of rows in df_h1
df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)

h2 <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h2,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)


df_h0 <- read_csv("data_raw/data_plant_height.csv")


mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

print(mu)

#samples

df_i <- df_h0 %>% 
  sample_n(size = 10)

print(df_i)

# for reproducibility
set.seed(3)

mu_i <- var_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
}
mu_i


#install.packages("patchwork") # install only once
library(patchwork)

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

# layout vertically
# possible only if "patchwork" is loaded
g_mu / g_var


#n-1
# for reproducibility
set.seed(3)

# redo simulations ----
mu_i <- var_i <- var_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
  var_ub_i[i] <- var(df_i$height)
}

# draw histograms ----
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_mu / g_var / g_var_ub
g_mu | g_var / g_var_ub



# Laboratory --------------------------------------------------------------
mu_j <- NULL
mu_k <- NULL
var_j <- NULL
var_k <- NULL

for (i in 1:100) {
  
  df_j <- df_h0 %>% 
    sample_n(size = 50) # random samples of 10 individuals
  df_k <- df_h0 %>% 
    sample_n(size = 100)
  # save mean for sample set i
  mu_j[i] <- mean(df_j$height)
  mu_k[i] <- mean(df_k$height)
  # save variance for sample set i
  var_j[i] <- var(df_j$height) 
  var_k[i] <- var(df_k$height)
  
}

df_s <- tibble(mu_j,mu_k,var_j,var_k)

muj <-  df_s %>% 
  ggplot(aes(x = mu_j)) +
  geom_histogram(color="Black") +
  geom_vline(xintercept = mu,color="Green")

muk <- df_s %>% 
  ggplot(aes(x = mu_k)) +
  geom_histogram(color = "Black") +
  geom_vline(xintercept = mu,color="Green")

varj <- df_s %>% 
  ggplot(aes(x = var_j)) +
  geom_histogram(color = "Black") +
  geom_vline(xintercept = mu,color="Green")

vark <- df_s %>% 
  ggplot(aes(x = var_k)) +
  geom_histogram(color = "Black") +
  geom_vline(xintercept = sigma2,color="Green")


muj/muk|varj/vark

#Part 2

df_h10 <- df_h0 %>% 
  filter(height >= 10)

mu_j1 <- NULL
mu_k1 <- NULL
var_j1 <- NULL
var_k1 <- NULL

for (i in 1:100) {
  
  df_j1 <- df_h10 %>% 
    sample_n(size = 50) # random samples of 50 individuals
  df_k1 <- df_h10 %>% 
    sample_n(size = 100)
  # save mean for sample set i
  mu_j1[i] <- mean(df_j1$height)
  mu_k1[i] <- mean(df_k1$height)
  # save variance for sample set i
  var_j1[i] <- var(df_j1$height) 
  var_k1[i] <- var(df_k1$height)
  
}

df_s1 <- tibble(mu_j1,mu_k1,var_j1,var_k1)

muj1 <-  df_s1 %>% 
  ggplot(aes(x = mu_j1)) +
  geom_histogram(color="Black") +
  geom_vline(xintercept = mu,color="Green")

muk1 <- df_s1 %>% 
  ggplot(aes(x = mu_k1)) +
  geom_histogram(color = "Black") +
  geom_vline(xintercept = mu,color="Green")

varj1 <- df_s1 %>% 
  ggplot(aes(x = var_j1)) +
  geom_histogram(color = "Black") +
  geom_vline(xintercept = mu,color="Green")

vark1 <- df_s1 %>% 
  ggplot(aes(x = var_k1)) +
  geom_histogram(color = "Black") +
  geom_vline(xintercept = sigma2,color="Green")


muj1/muk1|varj1/vark1
