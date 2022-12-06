#First, let's read in the point data and the shapefile for the excavation boundary. We can then plot the Excavation boundary with points
# http://www.mattpeeples.net/modules/PointPattern.html
library(maptools)
library(rgdal)
library(rgeos)
library(sf)
library(spatstat)
library(tidyverse)

setwd("~/Research/Ch_1_mtDNA_Structure")

### step 1: make the polygon for the boundary window
#use the entire extent of sampling as range.
enc <- read.csv("~/Research/tidy_datafiles/tidy_enounters.csv", na.strings = c("","NA")) %>% 
  drop_na(Latitude_Bin) %>% #remove no loc samples
  dplyr::filter(FreeSwimming == "Y")

##make the poly for sampling range
# make a list of the possible lat bins in sample range
bins <- unique(enc$Latitude_Bin) # doing it this way b/c do not have sample from each lat bin
#length(bins) #24

#tiny loop to find extent you have data for

#TRIED TO MAKE A TINY EMPTY DF WITH PRENAMED COLUMNS BUT R IS HARD.
# THIS WOULD HAVE LET YOU FIND + BUFFER THE COASTAL SIDE OF DATA W/OUT MESSING AROUND W/SHAPE FILES.
# BUT AGAIN. R IS HARD.

c <- data.frame(matrix(nrow=0 , ncol=2))
names <- c("Lat","Long")
colnames(c) <- names

d <- c

#c <- data.frame(Long = c(-114, -134, -114), Lat = c(30, 55, 56)) # start w/df of pts to close polygon
# for (i in bins){
#   temp1 <- enc %>% filter(Latitude_Bin == i)
#   max <- max(temp1$Long)
#   Lat <- i
#   newpt <- c(max, Lat)
#   c <- rbind(c, newpt)
# }

## add western bound
for (i in 1:length(bins)){
  temp1 <- enc %>% filter(Latitude_Bin == bins[i])
  max <- max(temp1$Long)
  Lat <- bins[i]
  c[i,] <- c(max, Lat)
}

## add eastern bound
for (i in 1:length(bins)){
  temp1 <- enc %>% filter(Latitude_Bin == bins[i])
  min <- min(temp1$Long)
  Lat <- bins[i]
  d[i,] <- c(min, Lat)
}

#####
#c %<>% arrange(Lat) %>% # reorder so points make nice loop
#  mutate( Long=(Long-1)) # move polygon line slightly east as a buffer (this is easier than the functions to add buffer)
c <- rbind( d, c) #close the list of points

# # now make the spatial poly for the owin
# P <- Polygon( c ) # make the reg polygon
# ps <-  Polygons(list(P),1) # not sure what this is about (stackoverflow magic)
# sps <-  SpatialPolygons(list(ps)) # make it a spatial polygon
# proj4string(sps) = CRS("+init=epsg:4269")
# 
# plot(sps) # it works!
# class(sps)
# # change it to a owin object
# ENP_buffered <- as.owin.SpatialPolygons(sps, "owin")

# check to see if poly is clockwise? apparently this matters for unknown spatial reasons...
clockwise <- function(x) {
  
  x.coords <- c(x[[1]], x[[1]][1])
  y.coords <- c(x[[2]], x[[2]][1])
  
  double.area <- sum(sapply(2:length(x.coords), function(i) {
    (x.coords[i] - x.coords[i-1])*(y.coords[i] + y.coords[i-1])
  }))
  
  double.area > 0
} 

clockwise(data.frame(x=c$Long, y=c$Lat)) # yep it is
# now make the owin using reversed df...
W <- owin(poly=data.frame(x=rev(c$Long), y=rev(c$Lat)))
plot(W)
# and it works. what. the. ugh.

#########################
## Step 1 part 2:
# clip the owin to a smoothed coastline

# download the data 
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", 
              destfile = './data/coastlines.zip')
# unzip the file
unzip(zipfile = "./data/coastlines.zip", 
      exdir = './data/ne-coastlines-10m')
#, we can open the data using readOGR from the sp (spatial) package.

# set map extent hurr
domain <- c(
  xmin = -135,
  xmax = -115,
  ymin = 28,
  ymax = 60)

# load the data 
coastlines <- readOGR("./data/ne-coastlines-10m/ne_10m_coastline.shp") # %>% st_crop(domain)

# simplify geometry
# uses the tol argument - the tolerance value - a large number will remove more vertices, make the data small AND yield a "blockier" looking object. a SMALLER number will retain more vertices and maintain a smoother looking feature.
coastlines_simp <- gSimplify(coastlines, 
                             tol = 1, 
                             topologyPreserve = TRUE)
plot(coastlines_simp,
     main = "map with boundaries simplified", xlim = c(-135, -115))
plot(W, add=TRUE, col="blue")
#seems reasonalble

# class(coastlines_simp)
# [1] "SpatialLines"
# attr(,"package")
# [1] "sp"

# now need to clip the buffered studyarea polygon to the coastline.....



#convert back for finding the diff
surveyarea <- st_as_sf(W)

coastmap <-st_union(world)
# proj4string(coastmap) = CRS("+proj=longlat +datum=WGS84 +no_defs 
#     +ellps=WGS84 +towgs84=0,0,0")

# difference between world polygons and the rectangle
difference <- st_difference(surveyarea, st_union(world))
difference <- st_difference(surveyarea, coastlines_simp)
# coerce back to sp
difference <- as(difference, 'Spatial')
# plot the result
plot(sps) # it works!
plot(difference)

















## step 2: make a marked point pattern object
encounters <- read.csv(file = "~/Research/tidy_datafiles/tidy_FG_encounters.csv") %>%  # read in csv file
    drop_na(Long) %>% 
    drop_na(dlphap) %>% 
    dplyr::select(Long, Lat, dlphap, Sex) 

# #Next, we need to convert the point plotted data into a ppp object for the spatstat package that we will be using. This requires us define a boundary (window) for the the points. We'll use the LiencresEx shapefile as our boundary for now. Following that, we can plot the data.
# W <- as.owin(sps)
# # define the window (class owin) for analyses
# W <- as(sps, "owin")

# Define ppp object based on point locations
encounters.pp <- ppp(x = encounters$Long, y = encounters$Lat, window = W, marks = encounters$dlphap)
# now let's plot it
plot(encounters.pp, cex = 0.5)


##############################
# Okay, now let's calculate the Ripley's K function for all points at this site and plot the results. 
# Ripley's K is a method of determining whether a point pattern deviates from complete spatial randomness (CSR) at a given distance. 
# use the Kest function from the spatstat package - using the "Ripley" correction factor that helps us deal with edge effects

#Dealing with inhomogeneous data - whales and sampling are not random
#So far, all of the examples we have shown have assumed an homogeneous Poisson point process, meaning that we are assuming that the locations of points across the site are independent (a Poisson process) and that they DO NOT differ in intensity from place to place (homogeneous). An inhomogeneous Poisson point process refers to a distribution generated by process where the locations of points across the site are independent but they DO differ in intensity. How can we tell if our point distribution is inhomogeneous? In many cases we can simply create a density map of all points and assess the degree to which there are concentrated areas of high density. Let's do that for the Liencres data. The value for sigma below defines the interpolation scale for the density plot. I've provided three values to give you an idea of how this can impact your assessments of density.

par(mfrow = c(1, 3))  # set up to plot 3 plots on one row
# plot density at various levels of sigma
plot(density(encounters.pp, sigma = 0.5), main = "Sigma 0.5")
plot(density(encounters.pp, sigma = 0.75), main = "Sigma 0.75")
plot(density(encounters.pp, sigma = 1), main = "Sigma 1.0")
# yep, monterey is where everyone looks for whales...



# Calculate Kdot and Ldot for inhomegenous data [calcuate the relationship between mark i and all other types]
par(mfrow = c(1, 4))
# Calculate L-hat between one hap and all other haplotyped encounters
aplus.dot <- envelope(encounters.pp, Kdot.inhom, i = "A+", verbose = F)
plot(aplus.dot, . - r ~ r)

aminus.dot <- envelope(encounters.pp, Kdot.inhom, i = "A-", verbose = F)
plot(aminus.dot, . - r ~ r)

e1.dot <- envelope(encounters.pp, Kdot.inhom, i = "E1", verbose = F)
plot(e1.dot, . - r ~ r)

f2.dot <- envelope(encounters.pp, Kdot.inhom, i = "F2", verbose = F)
plot(f2.dot, . - r ~ r)







# # Add required packages 
# require(sp)
# require(rgdal)
# require(spatstat)  
# require(sf)
# library(maptools)
# #use the entire extent of sampling as range.
# 
# # read in data
# # using entire encounter dataset
# enc <- read.csv("~/Research/tidy_datafiles/tidy_enounters.csv", na.strings = c("","NA")) %>% 
#   drop_na(Latitude_Bin) %>% #remove no loc samples
#   dplyr::filter(FreeSwimming == "Y")
# 
# # the 'study area' polygon
# #
# # is problematic. Running with a box for now cause idk...
# Studyarea <- sps
# 
# # Coerce study area to owin object
# w <- as.owin(Studyarea)
# 
# # Coerce points to ppp object and pass the object the study area window 
# Year1 <- as.ppp(enc, W=w)
# 
# # Alternate method with marks
# Year1 <- ppp(coordinates(Year1)[,1],coordinates(Year1)[,2], window=w, marks=Year1@data$MyVar) 
# 
# # Plot points with study area window
# plot(Year1)