##frequency tables for pop structure analysiss. no replicates per region, recaptures between regions allowed
##4/29/20


##packages!

library(tidyverse)
library(ggplot2)
#library(scatterpie)
#library(marmap)

##import data
data <- read.csv("~/Research/Humpback_Data/Humpback_Sample_Data.csv")
head(data)
##data wrangling to subset State Region and haplotype code
subset <- select(data, BAKER.LAB.Genetic.ID, OSU.Region, dlphap, Year)
dim.data.frame(subset)
#remove samples w/out mt hap call
filtered <- filter(subset, !grepl('het|HET|FAILED|X|CONTAM|REPCR', dlphap))
dim.data.frame(filtered) #nrow 917
#remove pqs samples for iwc analys
filtered <- filter(filtered, !grepl('PQS', BAKER.LAB.Genetic.ID))
dim.data.frame(filtered) #880

##frequeny table by haps
hapbyregion <- filtered %>% group_by(OSU.Region) %>% count(dlphap)
print(hapbyregion)
NPmthaps <- spread(hapbyregion, dlphap, n)
print(NPmthaps)




##entire CA hap freq table
target <- c("California", "California-Central", "Cailfornia-South", "California-North")
cawhole <- filtered %>% group_by(OSU.Region %in% target) %>% 
  filter(grepl(California))


cawhole <- filtered %>% 


  
  count(dlphap)
print(cawhole)


CA_all <- filtered %>% 
  filter(grepl('California', OSU.Region)) %>%
  select(BAKER.LAB.Genetic.ID, dlphap)
  unique(BAKER.LAB.Genetic.ID)



  count(dlphap)


head(CA_all)
nrow(CA_all) #497. removed repls?



###make frequency tables, grep out failed/hets
##NBC hap freq table
NBC <- filter(subset, OSUregion == "Northern British Columbia", !grepl('het|FAILED|X|HET', dlp))
North_BC <- dplyr::count(NBC, dlp, wt=NULL)
NBC_n <-nrow(NBC)
NBC_n

##SBC/WA hap freq table
target <- c("Southern British Columbia", "Washington")
SBCWA <- filter(subset, OSUregion %in% target, !grepl('het|FAILED|X|HET', dlp))
SBC_WA <- dplyr::count(SBCWA, dlp, wt=NULL)
SBCWA_n <-nrow(SBCWA)
SBCWA_n

##OR hap freq table
OR <- filter(subset, OSUregion == "Oregon", !grepl('het|FAILED|X|HET', dlp))
Oregon <- dplyr::count(OR, dlp, wt=NULL)
print(Oregon)
print(OR)
OR_n <-nrow(OR)
OR_n

##Cali hap freq table
Cali <- filter(subset, OSUregion == "California", !grepl('het|FAILED|X|HET', dlp))
California <- dplyr::count(Cali, dlp, wt=NULL)
head(California)
Cali_n <- nrow(Cali)
Cali_n

####pie function
# arg1 =input df
#arg2 =plotname
#arg3 = title in quotes!!

hap.pies <- function(arg1, arg2, arg3, ...) {
  arg2 <- ggplot(arg1, aes(x="", y = n, fill= dlp))+
    geom_bar(stat="identity", width=1) + 
    coord_polar("y") +
    ggtitle(arg3) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), panel.background = element_blank(), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
    scale_fill_manual(values = c( "A+" = "deeppink4",
                                  "A-" = "darkgoldenrod1",
                                  "A3" = "mistyrose",
                                  "A4" = "lightpink",
                                  "A5" = "lightpink",
                                  "E1" = "springgreen4",
                                  "E6" = "darkcyan", ##or turquoise4
                                  "E7" = "olivedrab4", ##or darkolivegreen
                                  "E8" = "chartreuse4",
                                  "E9" = "darkseagreen1",
                                  "E10" = "darkseagreen1",
                                  "E11" = "darkseagreen1",
                                  "E12" = "darkseagreen1",
                                  "E13" = "darkseagreen1",
                                  "E14" = "darkseagreen1",
                                  "E15" = "darkseagreen1",
                                  "E2" = "darkseagreen1",
                                  "E3" = "green",
                                  "E4" = "darkseagreen1",
                                  "F2" = "blue",
                                  "F1" = "lightskyblue",
                                  "F3" = "lightskyblue",
                                  "F4" = "lightskyblue",
                                  "F5" = "lightskyblue",
                                  "F6" = "lightskyblue",
                                  "E5" = rgb(51,153,102),
                                  "F7" = "lightskyblue",
                                  "F8" = "lightskyblue",
                                  "F9" = "lightkyblue"))
}





#####haplotype pies for each region - using baker lab colors
##NBC PIE CHART
NBCpie <- ggplot(North_BC, aes(x="", y = n, fill= dlp))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y") +
  ggtitle("Northern British Columbia (n=168)") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), panel.background = element_blank(), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
  scale_fill_manual(values = c( "A+" = "deeppink4",
                                "A-" = "darkgoldenrod1",
                                "A3" = "mistyrose",
                                "A4" = "lightpink",
                                "E1" = "springgreen4",
                                "E6" = "darkcyan", ##or turquoise4
                                "E7" = "olivedrab4", ##or darkolivegreen
                                "E8" = "chartreuse4",
                                "E9" = "darkseagreen1",
                                "E10" = "darkseagreen1",
                                "E11" = "darkseagreen1",
                                "E12" = "darkseagreen1",
                                "E13" = "darkseagreen1",
                                "E14" = "darkseagreen1",
                                "E15" = "darkseagreen1",
                                "E2" = "darkseagreen1",
                                "E3" = "green",
                                "E4" = "darkseagreen1",
                                "F2" = "blue",
                                "F1" = "lightskyblue",
                                "F3" = "lightskyblue",
                                "F4" = "lightskyblue",
                                "F5" = "lightskyblue",
                                "F6" = "lightskyblue",
                                "E5" = "seagreen4", #"aquamarine4",  
                                "F7" = "lightskyblue",
                                "F8" = "lightskyblue",
                                "F9" = "lightkyblue"))
show(NBCpie)

##SBCWA pie
SBCWApie <- ggplot(SBC_WA, aes(x="", y = n, fill= dlp))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y") +
  ggtitle("Southern British Columbia & Washington (n=120)") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), panel.background = element_blank(), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
  scale_fill_manual(values = c( "A+" = "deeppink4",
                                "A-" = "darkgoldenrod1",
                                "A3" = "mistyrose",
                                "A4" = "lightpink",
                                "E1" = "springgreen4",
                                "E6" = "darkcyan", ##or turquoise4
                                "E7" = "olivedrab4", ##or darkolivegreen
                                "E8" = "chartreuse4",
                                "E9" = "darkseagreen1",
                                "E10" = "darkseagreen1",
                                "E11" = "darkseagreen1",
                                "E12" = "darkseagreen1",
                                "E13" = "darkseagreen1",
                                "E14" = "darkseagreen1",
                                "E15" = "darkseagreen1",
                                "E2" = "darkseagreen1",
                                "E3" = "green",
                                "E4" = "darkseagreen1",
                                "F2" = "blue",
                                "F1" = "lightskyblue",
                                "F3" = "lightskyblue",
                                "F4" = "lightskyblue",
                                "F5" = "lightskyblue",
                                "F6" = "lightskyblue",
                                "E5" = "seagreen4", #"aquamarine4",  
                                "F7" = "lightskyblue",
                                "F8" = "lightskyblue",
                                "F9" = "lightkyblue"))
show(SBCWApie)

##Oregon pie
ORpie <- ggplot(Oregon, aes(x="", y = n, fill= dlp))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y") +
  ggtitle("Oregon (n=49)") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), panel.background = element_blank(), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
  scale_fill_manual(values = c( "A+" = "deeppink4",
                                "A-" = "darkgoldenrod1",
                                "A3" = "mistyrose",
                                "A4" = "lightpink",
                                "E1" = "springgreen4",
                                "E6" = "darkcyan", ##or turquoise4
                                "E7" = "olivedrab4", ##or darkolivegreen
                                "E8" = "chartreuse4",
                                "E9" = "darkseagreen1",
                                "E10" = "darkseagreen1",
                                "E11" = "darkseagreen1",
                                "E12" = "darkseagreen1",
                                "E13" = "darkseagreen1",
                                "E14" = "darkseagreen1",
                                "E15" = "darkseagreen1",
                                "E2" = "darkseagreen1",
                                "E3" = "green",
                                "E4" = "darkseagreen1",
                                "F2" = "blue",
                                "F1" = "lightskyblue",
                                "F3" = "lightskyblue",
                                "F4" = "lightskyblue",
                                "F5" = "lightskyblue",
                                "F6" = "lightskyblue",
                                "E5" = "seagreen4", #"aquamarine4",  
                                "F7" = "lightskyblue",
                                "F8" = "lightskyblue",
                                "F9" = "lightkyblue"))
show(ORpie)


##CALIFORNIA PIE
CApie <- ggplot(California, aes(x="", y = n, fill= dlp))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y") +
  ggtitle("California (n=456)") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
  scale_fill_manual(values = c( "A+" = "deeppink4",
                                "A-" = "darkgoldenrod1",
                                "A3" = "mistyrose",
                                "A4" = "lightpink",
                                "E1" = "springgreen4",
                                "E6" = "darkcyan", ##or turquoise4
                                "E7" = "olivedrab4", ##or darkolivegreen
                                "E8" = "chartreuse4",
                                "E9" = "darkseagreen1",
                                "E10" = "darkseagreen1",
                                "E11" = "darkseagreen1",
                                "E12" = "darkseagreen1",
                                "E13" = "darkseagreen1",
                                "E14" = "darkseagreen1",
                                "E15" = "darkseagreen1",
                                "E2" = "darkseagreen1",
                                "E3" = "green",
                                "E4" = "darkseagreen1",
                                "F2" = "blue",
                                "F1" = "lightskyblue",
                                "F3" = "lightskyblue",
                                "F4" = "lightskyblue",
                                "F5" = "lightskyblue",
                                "F6" = "lightskyblue",
                                "E5" = "seagreen4", #"aquamarine4",  
                                "F7" = "lightskyblue",
                                "F8" = "lightskyblue",
                                "F9" = "lightkyblue"))
show(CApie)


show(NBCpie)
show(SBCWApie)
show(ORpie)
ggsave("ORpie.png", plot = ORpie, dpi=400)
ggsave("NBCpie.png", plot = NBCpie, dpi=400)
ggsave("SBCWApie.png", plot = SBCWApie, dpi=400)
ggsave("CApie.png", plot = CApie, dpi=400)

####Breeding ground pies####
#import data
breeding <- read.csv("BreedingGround_Genotys.csv")
head(breeding)
##data wrangling to subset State Region and haplotype code
subset2 <- select(breeding, OSUregion, SEX, dlphap)
dim.data.frame(subset2)
head(subset2)
#remove samples w/out mt hap call
filtered2 <- filter(subset2, !grepl('het|HET|FAILED|X|CONTAM|REPCR|0', dlphap))
dim.data.frame(filtered2)

regions <- unique(filtered2$OSUregion) 
print(regions)


##COMIBINED centam pivot
target <- c( "Central America-photo")  #c("Central America", "Central America-photo")
centam <- filter(filtered2, OSUregion %in% target)
CENTAM <- dplyr::count(centam, dlphap, wt=NULL)
nrow(centam)
print(CENTAM)


##CentAM
CentAmpie <- ggplot(CENTAM, aes(x="", y = n, fill= dlphap))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y") +
  ggtitle("Central America (n=64)") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
  scale_fill_manual(values = c( "A+" = "deeppink4",
                                "A-" = "darkgoldenrod1",
                                "A3" = "mistyrose",
                                "A4" = "lightpink",
                                "E1" = "springgreen4",
                                "E6" = "darkcyan", ##or turquoise4
                                "E7" = "olivedrab4", ##or darkolivegreen
                                "E8" = "chartreuse4",
                                "E9" = "darkseagreen1",
                                "E10" = "darkseagreen1",
                                "E11" = "darkseagreen1",
                                "E12" = "darkseagreen1",
                                "E13" = "darkseagreen1",
                                "E14" = "darkseagreen1",
                                "E15" = "darkseagreen1",
                                "E2" = "darkseagreen1",
                                "E3" = "green",
                                "E4" = "darkseagreen1",
                                "F2" = "blue",
                                "F1" = "lightskyblue",
                                "F3" = "lightskyblue",
                                "F4" = "lightskyblue",
                                "F5" = "lightskyblue",
                                "F6" = "lightskyblue",
                                "E5" = "seagreen4", #"aquamarine4",  
                                "F7" = "lightskyblue",
                                "F8" = "lightskyblue",
                                "F9" = "lightkyblue"))
show(CentAmpie)

##COMBINED ALL MEXICO 
target <- c("Mexico-photo")   #("MX-ML", "Mexico-photo")
mlmx <- filter(filtered2, OSUregion %in% target)
MLMX <- dplyr::count(mlmx, dlphap, wt=NULL)
nrow(mlmx)
print(MLMX)

##mainland mexico PIE
mlmx_pie<- ggplot(MLMX, aes(x="", y = n, fill= dlphap))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y") +
  ggtitle("Combined Mainland Mexico (n=115)") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
  scale_fill_manual(values = c( "A+" = "deeppink4",
                                "A-" = "darkgoldenrod1",
                                "A3" = "mistyrose",
                                "A4" = "lightpink",
                                "E1" = "springgreen4",
                                "E6" = "darkcyan", ##or turquoise4
                                "E7" = "olivedrab4", ##or darkolivegreen
                                "E8" = "chartreuse4",
                                "E9" = "darkseagreen1",
                                "E10" = "darkseagreen1",
                                "E11" = "darkseagreen1",
                                "E12" = "darkseagreen1",
                                "E13" = "darkseagreen1",
                                "E14" = "darkseagreen1",
                                "E15" = "darkseagreen1",
                                "E2" = "darkseagreen1",
                                "E3" = "green",
                                "E4" = "darkseagreen1",
                                "F2" = "blue",
                                "F1" = "lightskyblue",
                                "F3" = "lightskyblue",
                                "F4" = "lightskyblue",
                                "F5" = "lightskyblue",
                                "F6" = "lightskyblue",
                                "E5" = "seagreen4", #"aquamarine4",  
                                "F7" = "lightskyblue",
                                "F8" = "lightskyblue",
                                "F9" = "lightkyblue"))
show(mlmx_pie)


##COMBINED ALL MEXICO 
target <- c("MX-ML", "MX-AR")
mx <- filter(filtered2, OSUregion %in% target)
MX <- dplyr::count(mx, dlphap, wt=NULL)
nrow(mx)
print(MX)

##allmexico PIE
ALL_mx_pie<- ggplot(MX, aes(x="", y = n, fill= dlphap))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y") +
  ggtitle("All Mexico (n=216)") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
  scale_fill_manual(values = c( "A+" = "deeppink4",
                                "A-" = "darkgoldenrod1",
                                "A3" = "mistyrose",
                                "A4" = "lightpink",
                                "E1" = "springgreen4",
                                "E6" = "darkcyan", ##or turquoise4
                                "E7" = "olivedrab4", ##or darkolivegreen
                                "E8" = "chartreuse4",
                                "E9" = "darkseagreen1",
                                "E10" = "darkseagreen1",
                                "E11" = "darkseagreen1",
                                "E12" = "darkseagreen1",
                                "E13" = "darkseagreen1",
                                "E14" = "darkseagreen1",
                                "E15" = "darkseagreen1",
                                "E2" = "darkseagreen1",
                                "E3" = "green",
                                "E4" = "darkseagreen1",
                                "F2" = "blue",
                                "F1" = "lightskyblue",
                                "F3" = "lightskyblue",
                                "F4" = "lightskyblue",
                                "F5" = "lightskyblue",
                                "F6" = "lightskyblue",
                                "E5" = "seagreen4", #"aquamarine4",  
                                "F7" = "lightskyblue",
                                "F8" = "lightskyblue",
                                "F9" = "lightkyblue"))
show(ALL_mx_pie)

##hawaii pivot
hawaii <- filter(filtered2, OSUregion == "Hawaii")
HI <- dplyr::count(hawaii, dlphap, wt=NULL)
nrow(hawaii)
print(HI)

##HIPIE
HIpie<- ggplot(HI, aes(x="", y = n, fill= dlphap))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y") +
  ggtitle("Hawaii (n=222)") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
  scale_fill_manual(values = c( "A+" = "deeppink4",
                                "A-" = "darkgoldenrod1",
                                "A3" = "mistyrose",
                                "A4" = "lightpink",
                                "E1" = "springgreen4",
                                "E6" = "darkcyan", ##or turquoise4
                                "E7" = "olivedrab4", ##or darkolivegreen
                                "E8" = "chartreuse4",
                                "E9" = "darkseagreen1",
                                "E10" = "darkseagreen1",
                                "E11" = "darkseagreen1",
                                "E12" = "darkseagreen1",
                                "E13" = "darkseagreen1",
                                "E14" = "darkseagreen1",
                                "E15" = "darkseagreen1",
                                "E2" = "darkseagreen1",
                                "E3" = "green",
                                "E4" = "darkseagreen1",
                                "F2" = "blue",
                                "F1" = "lightskyblue",
                                "F3" = "lightskyblue",
                                "F4" = "lightskyblue",
                                "F5" = "lightskyblue",
                                "F6" = "lightskyblue",
                                "E5" = "seagreen4", #"aquamarine4",  
                                "F7" = "lightskyblue",
                                "F8" = "lightskyblue",
                                "F9" = "lightkyblue"))
show(HIpie)


##WNP pivot
target <- c("Phillipines", "Okinawa", "Ogasawara")
westernNP <- filter(filtered2, OSUregion %in% target)
WNP <- dplyr::count(westernNP, dlphap, wt=NULL)
nrow(WNP) 
print(WNP)


##WNPPIE
WNPpie<- ggplot(WNP, aes(x="", y = n, fill= dlphap))+
  geom_bar(stat="identity", width=1) + 
  coord_polar("y") +
  ggtitle("WNP (n=247)") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA), axis.title = element_blank(), plot.title= element_text(hjust = .5)) +
  scale_fill_manual(values = c( "A+" = "deeppink4",
                                "A-" = "darkgoldenrod1",
                                "A3" = "mistyrose",
                                "A4" = "lightpink",
                                "E1" = "springgreen4",
                                "E6" = "darkcyan", ##or turquoise4
                                "E7" = "olivedrab4", ##or darkolivegreen
                                "E8" = "chartreuse4",
                                "E9" = "darkseagreen1",
                                "E10" = "darkseagreen1",
                                "E11" = "darkseagreen1",
                                "E12" = "darkseagreen1",
                                "E13" = "darkseagreen1",
                                "E14" = "darkseagreen1",
                                "E15" = "darkseagreen1",
                                "E2" = "darkseagreen1",
                                "E3" = "green",
                                "E4" = "darkseagreen1",
                                "F2" = "blue",
                                "F1" = "lightskyblue",
                                "F3" = "lightskyblue",
                                "F4" = "lightskyblue",
                                "F5" = "lightskyblue",
                                "F6" = "lightskyblue",
                                "E5" = "seagreen4", #"aquamarine4",  
                                "F7" = "lightskyblue",
                                "F8" = "lightskyblue",
                                "F9" = "lightkyblue"))
show(WNPpie)




ggsave("CentAmpie.png", plot = CentAmpie, dpi=400)
ggsave("All_mx_pie.png", plot = ALL_mx_pie, dpi=400)
ggsave("HIpie.png", plot = HIpie, dpi=400)
ggsave("mlmxpie.png", plot = mlmx_pie, dpi=400)



####Plotted pies -- why is this sooo hard???####

##table of pie slice colors
NPmtDNA <- c("A+", "A-", "A3", "A4", "E1", "E6", "E7", "E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E2", "E3", "E4", "F2", "F1", "F3", "F4","F5","F6","E5","F7","F8","F9")
colors <- c("deeppink4", "darkgoldenrod1", "mistyrose", "lightpink", "springgreen4", "darkcyan", "olivedrab4", "chartreuse4", "darkseagreen1","darkseagreen1", "darkseagreen1", "darkseagreen1", "darkseagreen1", "darkseagreen1","darkseagreen1", "darkseagreen1", "green", "darkseagreen1", "blue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "seagreen4", "lightskyblue", "lightskyblue", "lightskyblue")
piecolors <- as.data.frame(NPmtDNA, colors)
head(piecolors)


pltdpies <- ggplot() + geom_scatterpie(aes(x=Long, y=Lat, group=OSUregion), data=filtered, cols=filtered$dlp[1:29])+ coord_equal()

# prepare the plot

#SBC/WA space pie
space.pies(xpos=48.300000, ypos=-126.12000, 120, pie.colors=NULL, radius=1, 
           link=FALSE, coord=NULL)


space.pies(48.3, -126.12, NPmthaps, piecolors, pie.radius=1, pie.space=5, 
           link=FALSE, seg.lwd=1, seg.col=1, seg.lty=1, coord=NULL)

spacepiedf = subset(NPmthaps, select = -c(V1))
head(spacepiedf)
mutate_all(~replace(., is.na(.), 0))