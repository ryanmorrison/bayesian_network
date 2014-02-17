#### Set working directory ####
setwd("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network")

library(maptools)
library(rgdal)

site1_xy <- read.table("data/coordinates/s1_cords.txt", header=TRUE)
head(site1_xy)
plot(site1_xy$x, site1_xy$y)

# Need to tell R that x,y are coordinates
coordinates(site1_xy) <- c("x", "y")

# Inspect the created object
class(site1_xy)
str(site1_xy)
head(site1_xy)

# X,Y coordinates are in New Mexico West state plane NAD83 (EPSG code 2904). This needs to be defined in the new object.
site1_xy@proj4string # No projection is currently specified
NMWSP <- CRS("+init=epsg:2904") # NMWSP stands for New Mexico West state plane
proj4string(site1_xy) <- NMWSP # Assign the proj4string slot the correct projection
site1_xy@proj4string # Project has been specified
