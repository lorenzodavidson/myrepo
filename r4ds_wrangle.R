library(ggplot2)
library(tidyverse)

## Tidy Data
table1 %>% 
  mutate(rate = cases / population * 10000)

table1 %>% 
  count(year, wt = cases)

ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

#12.2.1 Exercise
cases2 <- filter(table2,type == "cases")
population2 <- filter(table2,type == "population")
rate2 <- select(cases2,count) /select(population2,count)*10000
table2_edit <- mutate(table2, rate = rate2)

ggplot(cases2, aes(year, count)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

