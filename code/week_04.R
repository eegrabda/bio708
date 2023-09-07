#' ---
#' title: Week Four Code
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---

# Library -----------------------------------------------------------------
#+ message = FALSE, warning = FALSE
library(tidyverse)


# Central Tendency --------------------------------------------------------

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

# for vector x
n_x <- length(x) # the number of elements in x = the number of data points
sum_x <- sum(x) # summation for x
mu_x <- sum_x / n_x # arithmetic mean
print(mu_x) # print calculated value

# for vector y; we can calculate directly too
mu_y <- sum(y) / length(y)
print(mu_y) # print calculated value

print(mean(x))
print(mean(y))

#Geometric Mean
#for vector x
prod_x <- prod(x) # product of vector x; x1 * x2 * x3...
n_x <- length(x)
mug_x <- prod_x^(1 / n_x) # ^ means power
print(mug_x)

# for vector y
mug_y <- prod(y)^(1 / length(y))
print(mug_y)

#Median
# for vector x
x <- sort(x) # sort x from small to large
index <- (length(x) + 1) / 2 # (N + 1)/2 th index as length(x) is an odd number
med_x <- x[index]
print(med_x)



# Variance ----------------------------------------------------------------

# V for x
sqd_x <- (x - mean(x))^2 # squared deviance
sum_sqd_x <- sum(sqd_x)
var_x <- sum_sqd_x / length(x)
print(var_x)

# V for y
var_y <- sum((y - mean(y))^2) / length(y)
print(var_y)

# SD for x
sd_x <- sqrt(var_x) # sqrt(): square root
print(sd_x)

# for y
sd_y <- sqrt(var_y)
print(sd_y)

# Coef of Variation for x
cv_x <- sd_x / mean(x)
print(cv_x)

# for y
cv_y <- sd_y / mean(y)
print(cv_y)

#IQR
# for x
x_l <- quantile(x, 0.25) # quantile(): return quantile values, 25 percentile
x_h <- quantile(x, 0.75) # quantile(): return quantile values, 75 percentile
iqr_x <- abs(x_l - x_h) # abs(): absolute value
print(iqr_x)

#MAD
# for x
ad_x <- abs(x - mean(x))
mad_x <- median(ad_x)
print(mad_x)

# for y
mad_y <- median(abs(y - mean(y)))
print(mad_y)

#MAD/Median
# for x
mm_x <- mad_x / median(x)
print(mm_x)

# for y
mm_y <- mad_y / median(y)
print(mm_y)



# Laboratory --------------------------------------------------------------

z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))

mean_z <- mean(z)

med_z <- median(z)

gmean <- prod(z)^(1/length(z))
mean_z;med_z;gmean

tib_z <- tibble(z)
tib_z %>% 
  ggplot(aes(x=z))+
  geom_histogram(color="black")+
  geom_vline(xintercept = c(mean_z,med_z,gmean), color = c("blue","steelblue","dodgerblue2"))


z_rev <- -z + max(z) + 0.1

mean_zrev <- mean(z_rev)
med_zrev <- median(z_rev)
gmean_zrev <- prod(z_rev)^(1/length(z_rev)) 
mean_zrev;med_zrev;gmean_zrev


tib_zrev <- tibble(z_rev)
tib_zrev %>% 
  ggplot(aes(x=z_rev))+
  geom_histogram(color="black")+
  geom_vline(xintercept = c(mean_zrev,med_zrev,gmean_zrev), color = c("blue","steelblue","dodgerblue2"))



w <- rnorm(100, mean = 10, sd = 1)
m <- w*1000

sdw <- sqrt(var(w))
sdw
sdm <- sqrt(var(m))
sdm

MAD_W <- median(w-mean(w))
MAD_W
MAD_M <- median(m-mean(m))
MAD_M


CVW <- sdw/mean(w)
CVW
CVM <- sdm/mean(m)
CVM

MAD_W/median(w)
MAD_M/median(m)
