library(dplyr)
library(tidyverse)
library(nycflights13)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))


# Chapter 5 ---------------------------------------------------------------

# Plotting delays
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
  delay = mean(arr_delay, na.rm = TRUE),
  n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


# Chapter 7 ---------------------------------------------------------------

# Visualizing outliers
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50)) # Important for plot limits
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y) # y is 1 dimension of diamond
unusual

# Visualizing price vs volume
dimensions <- diamonds %>%
  select(price, x, y, z,clarity) %>%
  filter(x!=0, y!=0, z!=0)
price_volume <- mutate(
  dimensions, volume = x*y*z, price_cuberoot = price^(1/3)
)
ggplot(data = price_volume,mapping = aes(x=volume,y=price_cuberoot,color=clarity,alpha=1/3)) +
  geom_point(na.rm = TRUE) +
  xlim(0, 750)

# Covariation
ggplot(data = diamonds, mapping = aes(x = price, y = after_stat(density))) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))