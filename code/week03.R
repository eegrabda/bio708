#' ---
#' title: Week Three
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---

# Library -----------------------------------------------------------------
library(tidyverse)


## Visualization -----------------------------------------------------------


# basic plot
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point()

# change color by "Species" column
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point() +
  scale_color_brewer()

# sample data
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

# line plot
df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_point() +
  geom_line()

# basic plot; bins = 30 by default
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()
# change bin width
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5, color = "black")
# change bin number
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50)

# box plot
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()

# change fill by "Species"
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot()

# change fill by "Species", but change border color
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(color = "steelblue")

#facetwrap

iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point() +
  facet_wrap(~Species)

#facetgrid

iris %>%
  mutate(site = sample(letters[1:2],nrow(.),replace = T)) %>%
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point() +
  facet_grid(rows = vars(Species),
             cols = vars(site))
