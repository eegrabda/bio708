#' ---
#' title: Week Eleven Code
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---
#+ message = F, warning = F
library(tidyverse)
library(here)


# predictions --------------------------------------------------------------

# convert the data format to tibble
iris <- as_tibble(iris)

# develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

summary(m_iris)

# create a data frame for prediction
# variable names must be identical to the original dataframe for analysis
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))

# make prediction based on supplied values of explanatory variables
y_hat <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(new_data = y_hat)

print(df_pred)

iris %>% 
  ggplot(aes(x=Petal.Width, y=Petal.Length, color= Species))+
  geom_point()+
  geom_line(data=df_pred, aes(y=new_data))


# Laboratory ---------------------------------------------------------------
#Normality Assumption

residuals <- m_iris$residuals

shapiro.test(residuals)

plot(m_iris)

#Intercept Extraction

#Intercept for Setosa
m_iris$coefficients[1]

#Intercept for Versicolor
m_iris$coefficients[1]+m_iris$coefficients[3]

#Intercept for Virginica
m_iris$coefficients[1]+m_iris$coefficients[4]

#Alternative Model
irism <- lm(Petal.Length~Petal.Width, data = iris)

iris %>% 
  ggplot(aes(x=Petal.Width, y=Petal.Length, color= Species))+
  geom_point()+
  geom_abline(intercept = irism$coefficients[1], slope = irism$coefficients[2])
