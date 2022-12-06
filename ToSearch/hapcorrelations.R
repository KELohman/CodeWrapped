library("ggpubr")

# Filter df and compute the frequency
pretty_encounters <- encounters %>%
  filter(!(dlphap == "X")) %>% # drop failures
  filter( between(Month, 4,11)) %>% # filter to peak season obs (April - Nov)
  drop_na(Latitude_Bin) %>% # gotta have locs
  filter(between(Latitude_Bin, 33,55)) %>% 
  group_by(dlphap) %>% 
    mutate(nindiv_withhap = length(unique(GeneticID))) %>% 
    ungroup() %>% 
  group_by(Latitude_Bin) %>% 
    mutate(nencounts = n()) %>% 
    ungroup() %>% 
  distinct(GeneticID, Latitude_Bin, .keep_all = TRUE) %>% # keeping only one obs of an indiv per bin -- not so sure about this bit but moving on...
  group_by(Latitude_Bin) %>% 
    mutate(nindiv_perbin = length(unique(GeneticID))) %>% 
    ungroup()
  
common_haps <- pretty_encounters %>% 
   filter(nindiv_count <= 25) # leaves 10 haplotypes

# # leaves 1358 obs for rel freq calcluations 
# # summary of number of haps in common_haps df
rel_freq_samplesizesbylat <- pretty_encounters %>% group_by(Latitude_Bin) %>% count(dlphap)
 rel_freq_samplesizes <- common_haps %>% distinct(GeneticID,.keep_all = TRUE) %>%  group_by(dlphap) %>% tally()

 # calculate rel freq of each hap per lat bin
relative_freqs <- pretty_encounters %>% 
  group_by(Latitude_Bin, dlphap) %>% 
  summarise(counts = n()) %>%  # count of hap occurance per bin
  mutate(bin_sum = sum(counts)) %>% # add each lat bins own sample size column
  mutate(freq = counts / bin_sum) %>% # calc the rel freq
  ungroup() %>% 
  filter(dlphap %in% common_haps$dlphap)
 # filter(dlphap %in% common_haps$dlphap) # subset to haps that have were found in 15+ individual to drop rare
# okay - all set for regression!


spearman_out <- cor.test(relative_freqs$Latitude_Bin, relative_freqs$freq,
         method = "spearman",
         exact = FALSE)


sp <- ggscatter(relative_freqs, x = "Latitude_Bin", y = "freq", 
          size = 0.001, #make tiny bc double plotting
          add = "reg.line", conf.int = TRUE, # Add abline and confidence interval
          add.params = list(color = "darkgrey", fill = "lightgray"), # Customize reg. line
          cor.coef = TRUE, cor.method = "spearman")

kd <- ggscatter(relative_freqs, x = "Latitude_Bin", y = "freq", 
                size = 0.001, #make tiny bc double plotting
                add = "reg.line", conf.int = TRUE, # Add abline and confidence interval
                add.params = list(color = "darkgrey", fill = "lightgray"), # Customize reg. line
                cor.coef = TRUE, cor.method = "kendall")


sp_plot <- sp +
  geom_point(data=relative_freqs,  aes(y = Latitude_Bin, x = freq, colour = dlphap, size=counts))+
  facet_wrap(~dlphap, nrow=2) +   # a panel for each haplotype
  scale_color_manual(values = pie_cols)+
  theme_bw() +
  labs(x = "Degree of Latitude", y = "Relative Haplotype Frequency", title = "Spearman's correlation (degree of latitude on haplotype relative frequency)")+
  theme(legend.position = "none",
        panel.spacing = unit(.5, "lines")) + # adding space between panels
  theme_minimal()+
  guides(color = guide_legend("mtDNA haplotype"), size = guide_legend("Individuals (N)"))

kd_plot <- kd +
  geom_point(data=relative_freqs,  aes(x = Latitude_Bin, y = freq, colour = dlphap, size=counts))+
  facet_wrap(~dlphap, nrow=2) +   # a panel for each haplotype
  scale_color_manual(values = pie_cols)+
  theme_bw() +
  labs(x = "Degree of Latitude", y = "Relative Haplotype Frequency", title = "Kendall's tau correlation (degree of latitude on haplotype relative frequency)")+
  theme(legend.position = "none",
        panel.spacing = unit(.5, "lines")) + # adding space between panels
  theme_minimal()+
  guides(color = guide_legend("mtDNA haplotype"), size = guide_legend("Individuals (N)"))

# sp_plot
# kd_plot

###
arlequin_in <- encounters %>% 
  drop_na(dlphap) %>% 
  filter(dlphap != "X") %>% 
  filter(between(Month, 5,10)) %>%
  group_by(Latitude_Bin) %>% 
  distinct(GeneticID, Latitude_Bin, .keep_all = TRUE) %>% 
  select(Latitude_Bin,dlphap) %>% 
  count(dlphap)



geo_size <- arlequin_in %>% 
  arrange(dlphap) %>% 
  pivot_wider(names_from= dlphap, values_from = n)
