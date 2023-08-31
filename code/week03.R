#' ---
#' title: Week Three Code
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


## Extra Exercises ----------------------------------------------------------

#Create a 10x10 matrix, elements as random numbers

mat_norm <- matrix(rnorm(100, 50,5), 10,10)

#Calculate row means

# means <- c(mean(mat_norm[1,]),
# mean(mat_norm[2,]),
# mean(mat_norm[3,]),
# mean(mat_norm[4,]),
# mean(mat_norm[5,]),
# mean(mat_norm[6,]),
# mean(mat_norm[7,]),
# mean(mat_norm[8,]),
# mean(mat_norm[9,]),
# mean(mat_norm[10,]))
# print(means) #easy, but ugly

#better
rowMeans(mat_norm)

#create a tibble with 3 numeric and 1 charachter columns

tib_rand <- tibble(x1= rnorm(100),x2 = rnorm(100,15),xa = sample(letters,100,replace = T))
tib_rand

# Calculate sums and N for letter groups

tib_rand %>% 
  group_by(xa) %>% 
  summarise(Sums = sum(c(x1,x2)),
            N = n())
#shit question fixed
iris_w <- iris %>% 
  mutate(id = rep(1:50, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

print(iris_w)

# Remove rows with NA

tib_na <- tib_rand %>% 
  mutate(x1 = ifelse(x1 < 0, NA, x1),
  x2 = ifelse(x2 < 0, NA, x2))
  

tib_na %>% 
  drop_na(x1)
