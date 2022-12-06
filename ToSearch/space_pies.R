## scatterpie plot on a map in baker lab id 
library(sf)
library(tidyverse)
library(scatterpie)

# need to wrangle encounter data into bia + timings
# using entire encounter dataset
enc <- read.csv("~/Research/tidy_datafiles/tidy_enounters.csv", na.strings = c("","NA")) %>% 
  drop_na(Latitude_Bin) %>% #remove no loc samples
  dplyr::filter(FreeSwimming == "Y") %>% # sorry strandings
  dplyr::filter(dlphap != "X")  # pull out the past failures

# change date to proper date
enc$Date <- as.Date(enc$Date, "%m/%d/%y")
# create month clmn for easy filterng
enc[, "month"] <- format(enc[,"Date"], "%m")
  
# fst bins
by_bia <- enc %>%
  filter( between(month, 4,11) ) %>%  # filter month range between april and november for starters
  distinct(GeneticID, Latitude_Bin, .keep_all = TRUE) %>%  # subset to only one obs per indiv per bin
  group_by(Latitude_Bin) %>% 
  count(dlphap) %>% 
  ungroup() %>%
  pivot_wider(id_cols = Latitude_Bin, names_from = dlphap, values_from = n)


# add a row sum - ugly way bc of nas - erg.
temp <- by_bia %>% dplyr::select(-Latitude_Bin)
temp$n <- rowSums(temp, na.rm=TRUE) * ifelse(rowSums(is.na(temp)) == ncol(temp), NA, 1) 
temp2 <- temp$n
by_bia <- cbind(by_bia, temp2)

# now to get final df for space pie plotting
bia <- by_bia %>% 
  filter(temp2 >= 10) %>%  # then filter by w/in strata sample size - min 10?
  mutate(Lat = (Latitude_Bin +.5), Long = -135) # add lat long values for plotting pies in space

# change na's to 0 to fix cranky scatterpie?
bia[is.na(bia)] = 0 # yep, that did it.

# make tiny df of strata boundary lines
tinydf <- data.frame(lat_1 = 33:56,
                        lon_1 = seq.int(-115,-125.5, by = -0.438),
                        lon_2 = seq.int(-123,-135, by=-.52))

### pretty natural earth map again
# set map extent hurr
domain <- c(
  xmin = -135,
  xmax = -115,
  ymin = 32,
  ymax = 56)

# bring in the coast
coast_cropped <- st_read("~/Research/Spatial Probability of Assignment/data/ne_10m_coastline/ne_10m_coastline.shp") %>%
  st_crop(domain)

# and the us/ca state lines
states <- st_read("~/Research/Spatial Probability of Assignment/data/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp") %>%
  st_crop(domain)

# make the basemap
basemap <- ggplot() +
  geom_sf(data = coast_cropped, colour = "gray") +
  geom_sf(data = states, colour = "darkgray", fill = "lightgrey", size = 0.1) +
  theme_bw()

# pop on some pie points!

## set baker lab colors in a list to call in scale_fill_man
pie_cols <-  c("A+" = "deeppink4", "A-" = "darkgoldenrod1", "A3" = "mistyrose","E1" = "springgreen4", "E6" = "darkcyan","E7" = "olivedrab4", "E10" = "darkseagreen1", "E13" = "darkseagreen1", "E15" = "darkseagreen1","E2" = "darkseagreen1", "E3" = "green", "E4" = "darkseagreen1", "F2" = "blue", "F1" = "lightskyblue", "F3" = "lightskyblue", "F4" = "lightskyblue", "F6" = "lightskyblue","E5" = "#8fbc8f","F8" = "lightskyblue")

happie_plot <- basemap +
  geom_scatterpie(aes(x=Long, y=Lat, group=Latitude_Bin , r= log(temp2)/10),  
                  data=bia, cols=c("A-", "A+", "A3", "E1", "E10", "E13", "E15", "E2", "E3", "E4", "E5", "E6", "E7", "F1", "F2", "F3", "F4", "F6", "F8"))+
  scale_fill_manual(values = pie_cols) +
  geom_segment(data = tinydf, 
               aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_1), 
               color = "darkgrey", size = .5, alpha = 0.5, lineend = "round")+
  theme(axis.title = element_blank())+
  labs( type = "Haplotype")+
  xlab("Longitude")+
  ylab("Latitude")+
  guides(size="legend")+
  theme_classic()

happie_plot


## see how it looks with bars instead of pie charts....
# # Set up the region in which we put dots, numbers, and horizontal freq bars
# dottop <- 56
# dotbot <- 33 
# rectwidth <- 2.2 # total x dim of bars
# rectsep <- 0.75 # x separation between bars
# bbxlo <- -142.5 # below barrrier lowest x value
# rectheight <- 0.81 * (dottop - dotbot) / (56-33)  # silly to be har
# samp3 <- omy5_survey %>%
#   mutate(dot_y = dottop - (trial_map_order - 1) * (dottop - dotbot) / n(),
#          dot_x = -136) %>%
#   mutate(#xmin = ifelse(Migratory_Access == "Below barrier", bbxlo, bbxlo + rectwidth + rectsep),
#     xmin = bbxlo + rectwidth + rectsep,
#     xmax = xmin + rectwidth,
#     ymin = dot_y - 0.5 * rectheight,
#     ymax = dot_y + 0.5 * rectheight,
#     xres = xmin + rectwidth * InversionFreq) %>%
#   mutate(reseq = ifelse(is.na(Resequenced), FALSE, TRUE))
# 
# 
# barchart_plot <- basemap+
#   ggplot(data=pretty_encounters, aes(fill=dlphap, y=counts, x=Latitude_Bin)) + 
#   geom_bar(position="fill", stat="identity") +
#   scale_fill_manual(values = pie_cols) # baker cols
# 



## run amova/pairwise. have pies for comparison. i think pairwise is btr bc all we care about is the hap frequency comparisons

## need to fix up lines - probs gotta hard code. change to dashed for tested and solid color signif splits?
## need to decide on pie placement + scaling. probs gotta hardcode.d