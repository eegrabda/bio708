#' ---
#' title: Week Eight Code
#' output: html_document
#' date: "`r format(Sys.time(), '%d-%m-%Y')`"
#' author: Elise Grabda
#' ---
#+ message = F, warning = F
library(tidyverse)
library(here)



# Multiple Groups ---------------------------------------------------------

df_anova <- read_csv(here("data_raw/data_fish_length_anova.csv"))
distinct(df_anova, lake)

# geom_violin() - function for violin plots
# geom_jitter() - jittered points

df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency



# ANOVA -------------------------------------------------------------------

# first argument is formula
# second argument is data frame for reference
# do not forget specify data = XXX! aov() refer to columns in the data frame
m <- aov(formula = length ~ lake,
         data = df_anova)

print(m)
summary(m)



# Behind Scenes -----------------------------------------------------------
# estimate overall mean
mu <- mean(df_anova$length)

# estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group

print(df_g)

df_g <- df_g %>% 
  mutate(ss = dev_g * n)

print(df_g)

s_b <- sum(df_g$ss)
print(s_b)

df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish

# filter() & slice(): show first 3 rows each group
print(df_i %>% filter(lake == "a") %>% slice(1:3))

print(df_i %>% filter(lake == "b") %>% slice(1:3))

print(df_i %>% filter(lake == "c") %>% slice(1:3))


df_i_g <- df_i %>% 
  group_by(lake) %>% 
  summarize(ss = sum(dev_i))

print(df_i_g)
s_w <- sum(df_i_g$ss)
print(s_w)


# n_distinct() count the number of unique elements
n_g <- n_distinct(df_anova$lake)
s2_b <- s_b / (n_g - 1)
print(s2_b)

s2_w <- s_w / (nrow(df_anova) - n_g)
print(s2_w)



# Laboratory --------------------------------------------------------------

PG <- PlantGrowth

PGroup <- PG %>% 
  group_by(group) %>% 
  summarise(mu = mean(weight),
            sd = sd(weight))


PG %>% 
  ggplot(aes(x=group, y=weight))+
  geom_violin(fill="steelblue",alpha=0.4, draw_quantiles = 0.5)+
  geom_jitter(width=0.3,
              height=0)+
  geom_segment(data=PGroup,
               aes(x=group,
                   xend=group,
                   y=mu-sd,
                   yend=mu+sd))
PG %>% 
  ggplot(aes(x=group,y=weight))+
  geom_jitter(width=0.1,
              height=0)+
  geom_segment(data=PGroup,
               aes(x=group,
                   xend=group,
                   y=mu-sd,
                   yend=mu+sd))+
  geom_point(data = PGroup, aes(x=group, y=mu), size = 3, color = "Salmon")

ANOVA_PG <- aov(weight ~ group, data = PG)
summary(ANOVA_PG)

#5 groups, 30 measures

F <- 4.846
p_val1 <- 1- pf(q=F, df1= 5-1, df2 = 30-5)
p_val1

#3 groups, 15 measures

p_val2 <- 1- pf(q=F, df1= 3-1, df2 = 15-3)
p_val2


#4 groups, 20 measures

p_val3 <- 1- pf(q=F, df1= 4-1, df2 = 20-4)
p_val3

p_val1;p_val2;p_val3
