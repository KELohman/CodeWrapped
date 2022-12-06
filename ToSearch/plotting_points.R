##samples points plotted by haplotype code
##11.19.19

#setwd()
setwd("~/Research/Ch_1_mtDNA_Structure/R")

#libraries
library(tidyverse)
library(maps)

#load data
##import data
Samples <- read.csv("~/Research/Humpback_Data/Humpback_Sample_Data.csv", na.strings = c("","NA"))
head(Samples)

##data wrangling to subset State Region and haplotype code
samples <- Samples %>% dplyr::select(OSU.Region, Sex, dlphap, LAT, LONG, Year, Sex)
head(samples)
nrow(samples) #950

##try to remove blank lat/longs 
biopsy_locs <- samples %>% drop_na(LAT)
head(biopsy_locs)
nrow(biopsy_locs) #930


####color by hap code####
##plot split CA regions
haps <- c("A-", "A+", "A3", "E1", "E10", "E13", "E15", "E2", "E3", "E4", "E5", "E6", "E7", "F1", "F2", "F3", "F4", "F6", "F8")
  
biopsy_locs2 <- biopsy_locs %>%
  filter(dlphap %in% haps) #remove the failed/het samples
nrow(biopsy_locs2) #898!

hapcolors <- ggplot(biopsy_locs2, aes(x=LONG, y=LAT)) +
  geom_point(aes(color=dlphap, alpha =.7)) + 
  annotation_map(map_data("world")) +
  scale_size_area() +
  coord_fixed()

plot(hapcolors)
#ggsave("biopsy_haps.png", plot=last_plot(), dpi = 400)

####color by region####
##fix geographic bin mis-matches
biopsy_locs <- biopsy_locs %>%
  mutate(
    Region = case_when(
      LAT <= 42 ~ "CA",
      (LAT > 42.000000 & LAT <= 46.261935) ~ "OR",
      (LAT > 46.261936 & LAT <= 49.600000) ~ "SBC/WA",
      LAT > 49.6 ~ "NBC"))
head(biopsy_locs)

##geographic bins split ca
biopsy_locs <- biopsy_locs %>%
  mutate(
    SplitCARegion = case_when(
      LAT <= 36.258 ~ "Southern CA",
      (LAT > 36.258 & LAT <= 39.309  ) ~ "Central CA",
      (LAT > 39.309 & LAT <= 42.00 ) ~ "Northern CA",
      (LAT > 42.000000 & LAT <= 46.261935) ~ "OR",
      (LAT > 46.261936 & LAT <= 49.600000) ~ "SBC/WA",
      LAT > 49.6 ~ "NBC"))
head(biopsy_locs)



##plot 4 regions
static <- ggplot(biopsy_locs, aes(x=LONG, y=LAT)) +
  geom_point(aes(color=Region, alpha =.7)) + 
  annotation_map(map_data("world")) +
  scale_size_area() +
  coord_fixed()

plot(static)

ggsave("biopsy_locations.png", plot=last_plot(), dpi = 400)

##plot split CA regions
static2 <- ggplot(biopsy_locs, aes(x=LONG, y=LAT)) +
  geom_point(aes(color=SplitCARegion, alpha =.7)) + 
  annotation_map(map_data("world")) +
  scale_size_area() +
  coord_fixed()

plot(static2)


####color by haplotype code####




####marmap plot####
library(marmap)

enp_bathy <- getNOAA.bathy(lon1 = -115, lon2 = -135,
                        lat1 = 30, lat2 = 55, resolution = 4)
autoplot(enp_bathy, image = TRUE)
scaleBathy(enp_bathy, deg = 2, x = "bottomleft", inset = 5)

blues <- colorRampPalette(c("red","purple","blue",
                            "cadetblue1","white"))
plot(enp_bathy, image = TRUE, bpal = blues(100))

#install.packages("raster")
#install.packages("rasterVis")
#install.packages("levelplot")
library(raster)
library(rasterVis)
library(levelplot)

r <- marmap::as.raster(enp_bathy)

state <- map('state', plot = FALSE) 
state <- data.frame(lon = state$x, lat = state$y) 

state.lab <- data.frame(lon = state.center$x, lat = state.center$y, 
                        label = state.abb)

# you can remove the color legend by adding colorkey = FALSE in levelplot() 
library(rasterVis)
levelplot(r, 
          at = c(seq(min(atl), 0, length.out = 100), 
                 seq(0, max(atl), length.out = 100)[-1]),
          col.regions = c(blues(100), greys(100)), 
          margin = FALSE) +
  xyplot(lat ~ lon, state, type = 'l', 
         col = 'black') +
  xyplot(lat ~ lon, data = state.lab,
         panel = function(y, x, ...) {
           ltext(x = x, y = y, labels = state.lab$label, cex = 0.75)
         })
