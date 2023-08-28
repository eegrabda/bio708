#' ---
#' title: Week Two - Problem Set 2
#' output: html_document
#' date: 2023-08-28
#' author: Elise Grabda
#' ---
library(tidyverse)

#install.packages("ggplot2movies")
library(ggplot2movies)

data('movies')


# Exercise1  --------------------------------------------------------------

#1a

movies %>%
  mutate(rating_centered = movies$rating - mean(movies$rating))

#1b

moviesb <- movies[movies$year >= "2000",]
moviesb

#1c

moviesc <- movies %>% select(title, year, budget, length, rating, votes)
moviesc

#1d

colnames(movies)[3] <- "length_in_min"
colnames(movies)[3]


# Exercise2 ---------------------------------------------------------------

movies %>%
  group_by(year) %>%
  summarise(mean_budget = mean(budget, na.rm = T)) %>%
  print(n = 50)

# Exercise3 ---------------------------------------------------------------

dat = tibble(id = 1:10,
             x = rnorm(10),
             y = rnorm(10))
pivot_longer(dat,
             cols = -id,
             names_to = 'exwhy',
             values_to = 'values'
             )

# Exercise4 ---------------------------------------------------------------

movies %>%
  filter(year > 1990) %>%
  select(title, year, budget, length_in_min, rating, votes, mpaa, Action, Drama) %>%
  group_by(mpaa, Action) %>%
  summarise(AvgRating = mean(rating, na.rm = T))
