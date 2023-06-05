library(dplyr)
library(nycflights13)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

# Plotting delays
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
  delay = mean(arr_delay, na.rm = TRUE),
  n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  + geom_point(alpha = 1/10)
