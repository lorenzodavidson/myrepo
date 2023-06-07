library(ggplot2)
library(tidyverse)
library(nycflights13)

# Relational Data ---------------------------------------------------------

# Joining
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x_y <- x %>%
  full_join(y, by = "key") # Join keeping key = 3 and 4

# Add lat lon of airports to flights2 tables based on destination
flights2_latlon <- left_join(flights2,select(airports,faa,lat,lon), c("dest"="faa"))

# Exercise: Map out average delay of airports

destination_delay <- group_by(flights, dest) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
airports_delay <- left_join(airports,destination_delay,c("faa" = "dest"))

airports_delay %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat, size = delay),) +
  borders("state") +
  geom_point(alpha = 1/2) +
  coord_quickmap()

# Filtering Joins (flights by planes with more than 100 flights)

top_planes <- flights %>%
  count(tailnum, sort = TRUE) %>%
  filter(n > 100) 
top_flights <- semi_join(flights,top_planes)

