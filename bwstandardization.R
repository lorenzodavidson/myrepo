library(ggplot2)
library(maps)
install.packages("ggmap")
library(ggmap)
library(tidyverse)

# UTM Projection
lonlat2UTM_hemisphere <- function(lonlat) {
  ifelse(lonlat[1] > 0, "north", "south")
}

lonlat2UTMzone = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

latlon_crs = 4326

# Open and tidy file
bwfw <- read.csv("~/Desktop/Hollingsinternship/Code/myrepo/bwfw_data.csv")

bwfw_date_time <- separate(bwfw,timestamp_gmt, into = c("date", "time"), sep = " ")
bwfw_standardized <- group_by(bwfw_date_time,DeploymentID,date) %>%
  summarise(mean_lat = mean(latitude,na.rm = TRUE), mean_lon = mean(longitude,na.rm = TRUE))
# bwfw_mercator <- st_transform(bwfw_standardized, 4326)

# Map
map <- get_stamenmap(
  bbox = c(left=-135,bottom=0,right=-75,top=50),
  maptype = "terrain",
  zoom=4)

# Plotting telemetry data
ggmap(map) +
  geom_point(bwfw_standardized,mapping=aes(x=mean_lon,y=mean_lat,color=DeploymentID),show.legend = FALSE)
