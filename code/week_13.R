#' ---
#' title: Week Thirteen Code
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---
#+ message = F, warning = F
library(tidyverse)
library(here)


# More Likely -------------------------------------------------------------

# dpois()
# the first argument is "k"
# the second argument is "lambda"
dpois(3, lambda = 3.5)

# change lambda from 0 to 10 by 0.1
lambda <- seq(0, 10, by = 0.01)

# probability
pr1 <- dpois(3, lambda = lambda)

# create a data frame
df_pois <- tibble(y = 3,
                  lambda = lambda,
                  pr = pr1)

df_pois %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_point(color = "steelblue") +
  geom_line() +
  labs(x = "lambda",
       y = "Pr(k = 3)")


#Multiple Observations

# try lambda = 3 for y = 3, 2, 5
pr2 <- dpois(c(3, 2, 5), lambda = 3)
print(pr2)

# probability of observing 3, 2, 5 simultaneously
# with lambda = 3
prod(pr2)

# lambda = 0 - 10 by 0.01
y <- c(3, 2, 5)
lambda <- seq(0, 10, by = 0.1)

# sapply repeats the task in FUN
# each element in "X" will be sequencially substituted in "z"
pr3 <- sapply(X = lambda,
             FUN = function(z) prod(dpois(y, lambda = z)))
pr3

# make a data frame and arrange by pr (likelihood)
df_pois2 <- tibble(lambda = lambda,
                  pr = pr3)

df_pois2 %>% 
  arrange(desc(pr)) %>% 
  print()

# visualize
df_pois2 %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_line() +
  geom_point(color="steelblue") +
  labs(y = "prob")

mean(c(3,2,5))





# Laboratory --------------------------------------------------------------
#MLE for Binomial Dist.

y <- c(2,2,0,0,3,1,3,3,4,3)
p <- seq(0,1,0.01)


bitest <- sapply(X = p,
       FUN = function(z) prod(dbinom(y,10,p=z)))

bitibble <- tibble(p,bitest)

bitibble %>% 
  ggplot(aes(x=p,y=bitest))+
  geom_line()

bitibble %>% 
  arrange(desc(bitest))

#Most likely value is for p = 0.21

#compare NXp to mean y

mean(y)

10*0.21





