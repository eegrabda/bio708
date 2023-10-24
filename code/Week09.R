#' ---
#' title: Week Nine Code
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---
#+ message = F, warning = F
library(tidyverse)
library(here)

df_algae <- read_csv(here("data_raw/data_algae.csv"))
print(df_algae)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()

# lm() takes a formula as the first argument
# don't forget to supply your data
m <- lm(biomass ~ conductivity,
        data = df_algae)

m;summary(m)

# coef() extracts estimated coefficients
# e.g., coef(m)[1] is (Intercept)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) # draw the line

y_hat <- alpha + beta *df_algae$conductivity
df_algae$y_hat <- y_hat

epsilon <- df_algae$biomass-y_hat
epsilon

sd(epsilon)


theta <- coef(m)
se <- sqrt(diag(vcov(m)))

t_value <- theta/se

# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_beta)


sd(resid(m))

# pull vector data
v_x <- df_algae %>% pull(conductivity)
v_y <- df_algae %>% pull(biomass)

# theta[1] = alpha
# theta[2] = beta
error <- v_y - (theta[1] + theta[2] * v_x)

# cbind() combines vectors column-wise
# head() retrieves the first 6 rows
head(cbind(epsilon, error))

# add error column
df_algae <- df_algae %>% 
  mutate(epsilon = epsilon)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) + 
  geom_segment(aes(x = conductivity, # start-coord x
                   xend = conductivity, # end-coord x
                   y = biomass, # start-coord y
                   yend = biomass - epsilon), # end-coord y
               linetype = "dashed")

# residual variance
ss <- sum(resid(m)^2)

# null variance
ss_0 <- sum((v_y - mean(v_y))^2)

# coefficient of determination
r2 <- 1 - ss / ss_0

print(r2)



# LAB ---------------------------------------------------------------------

setosa <- as.data.frame(filter(iris, Species == "setosa"))

versicolor <- as.data.frame(filter(iris, Species == "versicolor"))

virginica <- as.data.frame(filter(iris, Species == "virginica"))



lmsetosa <- lm(Sepal.Width ~ Petal.Width, data = setosa)
summary(lmsetosa)

lmversicolor <- lm(Sepal.Width ~ Petal.Width, data = versicolor)
summary(lmversicolor)

lmvirginica <- lm(Sepal.Width ~ Petal.Width, data = virginica)
summary(lmvirginica)


lmsetplus <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data = setosa)
summary(lmsetplus)

lmverplus <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data = versicolor)
summary(lmverplus)

lmvplues <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data = virginica)
summary(lmvplues)


lapply(X= list(setosa,versicolor,virginica), function(X){
  summary(lm(Sepal.Width~Petal.Width, data=X))})

