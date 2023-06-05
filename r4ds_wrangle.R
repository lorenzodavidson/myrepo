library(ggplot2)
library(tidyverse)

## TIDY DATA
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

ggplot(cases2, aes(year, count)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

# Pivoting
tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")
left_join(tidy4a, tidy4b)

table2 %>%
  pivot_wider(names_from = type, values_from = count)

# Missing values
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
) # missing 2 values

stocks %>%
  pivot_wider(names_from = year, values_from = return)

# Case Study
who
who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>% 
  separate(key, c("new", "type", "sexage"), sep = "_") %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage,c("sex","age"),sep=1)

who_sum <- who1 %>%
  group_by(country,year,sex) %>%
  summarise(
    count = n())

Afghanistan <- filter(by_country,country=="Afghanistan")

ggplot(Afghanistan, aes(year)) + 
  geom_bar(aes(fill = sex))
  
