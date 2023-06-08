library(raster)
library(rgdal)
library(dismo)


#Load libraries 
suppressPackageStartupMessages(suppressWarnings({
  #library(devtools)
  #devtools::install_github("rspatial/dismo")
  library(sp)
  library(rgdal)
  library(raster)
  library(dismo)
  library(adehabitatLT) # help(package='adehabitat') # help.search('angle',package='adehabitat')
  library(maps)       # for map.where
  library(mapdata)    # for worldHires
  library(sp)
  library(maptools)
  library(mgcv)
  library(ape)
  library(ncf)
  library(ncdf4)
  library(spdep)
  library(ROCR)
  library(gbm)
  library(tidyverse)
  library(viridis)
  library(rJava)
  #library(ggmap)
  #library(RgoogleMaps)
}))

install.packages("adehabitatLT")
install.packages("maps")
install.packages("mapdata")
install.packages("maptools")
install.packages("mgcv")
install.packages("ape")
install.packages("ncf")
install.packages("ncdf4")
install.packages("spdep")
install.packages("ROCR")
install.packages("gbm")
install.packages("tidyverse")
install.packages("viridis")


# Data Preparation --------------------------------------------------------
library(dismo)

file <- paste0(system.file(package="dismo"), "/ex/bradypus.csv")
bradypus <- read.table(file,header=TRUE,sep=",")

bwfw <- read.csv("~/Desktop/Hollingsinternship/Code/myrepo/bwfw_data.csv")
bwfw <- bwfw %>%
  rename("lat" = "latitude") %>%
  rename("lon" = "longitude")

# Land removal (from Hazen tutorial) --------------------------------------

# Function
removeland<-function(dataset,sp.obj){
  pt = SpatialPoints(matrix(c(dataset$lon,dataset$lat), nrow=length(dataset$lon)), proj4string=CRS("+proj=longlat +datum=WGS84"))
  place = over(pt, sp.obj)
  return(dataset[is.na(place),])
}

# Land map
map_background = maps::map('worldHires', fill=T, col='transparent')
back.IDs = sapply(strsplit(map_background$names, ":"), function(x) x[1])
back.sp = map2SpatialPolygons(map_background, IDs=back.IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

bwfw_clean<-removeland(bwfw,back.sp)
coordinates(whales_clean) = ~long+lat
#plotting points -- we're still seeing some errors with points that have incorrect longitudes
points(whales_clean, pch=16, col=rgb(1, 0, 0, alpha=0.5), cex=0.3)
bwfw_clean<-removeland(bwfw,back.sp)

library(maptools)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-120,-80), ylim=c(0,50), axes=TRUE, col="light yellow")
points(bwfw$lon, bwfw$lat, col='red', pch=20, cex=0.5)

library(sp)
coordinates(bwfw) <- ~lon+lat
crs(bwfw) <- crs(wrld_simpl)
class(bwfw)

# Sampling Bias
r <- raster(bwfw) # create a RasterLayer with the extent of acgeo
res(r) <- 1 # set the resolution of the cells to (for example) 1 degree
r <- extend(r, extent(r)+1)
bwfw_sel <- gridSample(bwfw, r, n=1) # sample
p <- rasterToPolygons(r)
plot(p, border='gray')
points(bwfw)
points(bwfw_sel, cex=1, col='red', pch='x') # selected points in red

