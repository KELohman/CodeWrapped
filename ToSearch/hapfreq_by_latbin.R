
## mtDNA freq plots

library(tidyverse)
library(broom)

#https://stackoverflow.com/questions/37395059/running-several-linear-regressions-from-a-single-dataframe-in-r
#https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
### nesting in broom suggested method is being weird, just do call this shit.

encounters <- read.csv("~/Research/tidy_datafiles/tidy_FG_encounters.csv") 

# filter by date range - peak feeding season
encounters$Month <- format(as.Date(encounters$Date, format = "%m/%d/%y"), "%m")

# Filter df and compute the frequency
pretty_encounters <- encounters %>%
  filter(FreeSwimming == "Y") %>% # buh bye strandings
  filter(!(dlphap == "X"|dlphap =="F8"|dlphap =="F4")) %>% # drop failures
  filter( between(Month, 4,11)) %>% # filter to peak season obs (April - Nov)
  drop_na(Latitude_Bin) %>% # gotta have locs
  distinct(GeneticID, Latitude_Bin, .keep_all = TRUE) %>% # keeping only one obs of an indiv per bin -- not so sure about this bit but moving on...
  group_by(Latitude_Bin, dlphap) %>% 
  summarise(counts = n()) %>% 
  mutate(bin_sum = sum(counts)) %>% # add own sample size column for later plots
  mutate(freq = counts / bin_sum) %>% 
  filter(bin_sum > 10) %>%  # add arbitrary n>10 cutoff
  ungroup()


### pretty plots to get the idea of what's what
pie_cols <-  c("A+" = "deeppink4", "A-" = "darkgoldenrod1", "A3" = "mistyrose", "A4" = "lightpink", "A5" = "lightpink","E1" = "springgreen4", "E6" = "darkcyan","E7" = "olivedrab4","E8" = "chartreuse4","E9" = "darkseagreen1","E10" = "darkseagreen1", "E11" = "darkseagreen1","E12" = "darkseagreen1","E13" = "darkseagreen1","E14" = "darkseagreen1","E15" = "darkseagreen1","E2" = "darkseagreen1", "E3" = "green", "E4" = "darkseagreen1", "F2" = "blue", "F1" = "lightskyblue", "F3" = "lightskyblue", "F4" = "lightskyblue", "F5" = "lightskyblue", "F6" = "lightskyblue","E5" = "#8fbc8f","F7" = "lightskyblue","F8" = "lightskyblue","F9" = "lightkyblue")

# now to plot relative freq x lat_bin along coast as scatter to fit basic reg. line
hap_by_lat <- ggplot(pretty_encounters, aes(x=Latitude_Bin, y=freq, color=dlphap, size=counts)) +
  geom_point() +
  facet_wrap(~dlphap, ncol=8)+
  scale_color_manual(values = pie_cols)+
  geom_smooth(method=lm , color="darkgrey", fill="lightgrey", se=TRUE)+
  ylab("Relative haplotype frequency") +
  xlab(expression(paste("Degree of Latitude ( " ^0,' N)' ))) +
  labs(title="Relative haplotype frequency by 1-degree latitude")+
  theme(legend.position="bottom")+
  guides(size = FALSE) + # drop counts bit from legend
  theme_minimal()
hap_by_lat

# summary of sample sizes lef in pretty_encounter df
pretty_encounters %>% group_by(Latitude_Bin) %>% tally()

# models en mass
pretty_encounters %>% group_by(dlphap) %>% do(tidy(lm(freq ~ Latitude_Bin, data=.)))

# for r2 values
pretty_encounters %>% group_by(dlphap) %>% do(glance(lm(freq ~ Latitude_Bin, data=.)))
## need to figure out what actually needs to be reported...

# same but store in a list to put out each haps later...
tmp <- pretty_encounters %>% group_by(dlphap) %>% do(model=(lm(freq ~ Latitude_Bin, data=.)))

# try to get summary of this?
summary(tmp)
# or by index?
summary(tmp[tmp$dlphap == "F2",]$model)
# nooope.



#Ok to get the summary thing to work I think you do 1. Then for residuals 2. Should work

tmp <- pretty_encounters %>% group_by(dlphap) %>% do(model=summary(lm(freq ~ Latitude_Bin, data=.)))
tmp <- pretty_encounters %>% group_by(dlphap) %>% do(model=residuals(lm(freq ~ Latitude_Bin, data=.)))

#Tmp2 is how you pull data for a specific dlphap
tmp2 <- tmp %>% filter(dlphap == 'F2') %>% .$model

plot(pretty_encounters %>% group_by(dlphap) %>% do(model=residuals(lm(freq ~ Latitude_Bin, data=.))))




