#' ---
#' title: Week Six Code
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---
#+ message = F, warning = F
library(tidyverse)
library(patchwork)
library(here)

# load csv data on R
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1) + # specify binwidth
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

# calculate probability density
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density") # re-label

# probability of x < 10
p10 <- pnorm(q = 10, mean = mu, sd = sigma)
print(p10)

# probability of x < 20
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
print(p20)

# probability of 10 < x < 20
p20_10 <- p20 - p10
print(p20_10)


x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - pnorm(bin[i], mean = mu, sd = sigma)
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
df_prob <- tibble(p, bin = bin[-length(bin)]) %>% 
  mutate(freq = p * nrow(df_h0))

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")



# discrete ----------------------------------------------------------------

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define binwidth
                 center = 0) # relative position of each bin

# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)

# calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

# figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label

df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) # prob x sample size

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq))


# Laboratory --------------------------------------------------------------
##Normal Distribution

xnorm1 <- data.frame(xnorm = rnorm(50,10,10))
xseq <- seq(min(xnorm1$xnorm), max(xnorm1$xnorm), length = 50)

mu <- mean(xnorm1$xnorm)
sigma <- sd(xnorm1$xnorm)
pd <- dnorm(xseq, mean = mu, sd = sigma)

tibble(y = pd, x = xseq) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density") # re-label

xmin <- floor(min(xnorm1$xnorm))
xmax <- ceiling(max(xnorm1$xnorm))

binx <- seq(xmin, xmax, by = 1)

px <- NULL # empty object for probability
for (i in 1:(length(binx) - 1)) {
  px[i] <- pnorm(binx[i+1], mean = mu, sd = sigma) - pnorm(binx[i], mean = mu, sd = sigma)
}

df_probx <- tibble(px, binx = binx[-length(binx)]) %>% 
  mutate(freq = px * nrow(xnorm1))


xnorm1 %>%
  ggplot(aes(x=xnorm))+
  geom_histogram(binwidth = 1,colour = "black")+
  geom_point(data = df_probx, aes(x=binx,y=freq))+
  geom_line(data = df_probx,aes(x=binx, y=freq))

## Poisson Distribution

xpois <- rpois(1000,50)

minpois <- min(xpois)
maxpois <- max(xpois)
poisat <- seq(minpois,maxpois,by=1)

lamdahat2 <- mean(xpois)

pm2 <- dpois(poisat,lambda = lamdahat2)

pois_tib <- tibble(x = poisat, y = dpois(poisat,lambda = lamdahat2)) %>% 
  mutate(freq = length(xpois) * y )

tibble(xpois=xpois) %>% 
  ggplot()+
  geom_histogram(aes(x=xpois),colour = "black")+
  geom_point(data=pois_tib,aes(x=x,y=freq))+
  geom_line(data=pois_tib, aes(x=x,y=freq))
