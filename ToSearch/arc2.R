
# Libraries
library(tidyverse)
#library(viridis)
#library(tidygraph)
#library(patchwork)
library(igraph)
library(ggraph)
#library(colormap)

#load in data
encounters <- read.csv("~/Research/tidy_datafiles/tidy_encounters.csv")
# recap'd individuals
recap_df <- encounters %>%
  mutate(
    TrueRegion = case_when(
      Lat <= 36 ~ "CA-South",
      (Lat > 36 & Lat <= 39.9  ) ~ "CA-Central",
      (Lat > 39.9 & Lat <= 43.000 ) ~ "CA-North",
      (Lat > 43.000 & Lat <= 46.2) ~ "OR",
      (Lat > 46.2 & Lat <= 49.4) ~ "SBC-WA",
      Lat > 49.4 ~ "NBC")) %>% 
  mutate(
    TempBin = case_when(
      Year>=1985 & Year <=1990 ~ 1,
      Year>=1990 & Year <=1995 ~ 2,
      Year>=1995 & Year <=2000 ~ 3,
      Year>=2000 & Year <=2005 ~ 4,
      Year>=2005 & Year <=2010 ~ 5,
      Year>=2010 & Year <=2015 ~ 6,
      Year>=2015 & Year <=2020 ~ 7)) %>%
  drop_na(TrueRegion,TempBin) %>% # must have locations
  group_by(TrueRegion,TempBin) %>%
  mutate(nindiv = length(unique(GeneticID))) %>%
  ungroup() 

# list of regions w/temporal resampling
regions <- c("CA-South", "CA-Central","CA-North", "SBC-WA")
#need an empty list first to save later
all_recappies <- c()

#for loop to work through all the recap regions
recaps <- for (i in 1:length(regions)){
  
  temp <- recap_df %>% 
    select(GeneticID, TrueRegion, TempBin,nindiv) %>% 
    filter(TrueRegion == regions[1]) %>% 
    distinct(GeneticID, TrueRegion, TempBin,.keep_all = TRUE) %>% # one obs of an indiv per tempbin w/in a region
    group_by(GeneticID) %>% 
    filter(n()>1) %>% # must have a between region recap
    ungroup() %>% 
    rowid_to_column() %>% 
    rename(X = "rowid")
  
  # list 'o edges
  edge <- temp %>%
    group_by(GeneticID) %>% 
    arrange(TempBin, .by_group = TRUE) %>% 
    do(data.frame(t(combn(.$TempBin,2)))) %>% 
    ungroup() %>% 
    select(-GeneticID)
  
  temp2 <- edge %>% 
    group_by(X1,X2) %>%
    tally() %>% 
    mutate("Region"= regions[1])
  
  temp2 <- temp %>% 
    select(TempBin,nindiv) %>%
    distinct(TempBin,.keep_all = TRUE) %>% 
      left_join(temp2, temp, by=c("X1" = "TempBin"))
  
  all_recappies[[i]] <- temp2
}

#big_data = do.call(rbind, all_recappies)

nodes <- seq(1,7, by=1)

# turn into a ggraph obj from the dataframe
g <- graph_from_data_frame(all_recappies[[1]], directed = FALSE, nodes)

#now a pretty plot
ggraph(g, layout = "linear")+
  geom_edge_arc(alpha = .8, aes(width = n,color=n), label_colour = "black") +
  labs(edge_width = "Individual recaptures (n)") +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_point(color = "black",fill="darkgrey",size = nindiv,pch=21)+
  geom_node_text(aes(label = name), nudge_y = -.1 ) +
  theme_graph()
  
  
  # + 
  geom_node_text(aes(label = name),  colour = 'white', size=8,
                 show.legend = FALSE) +
  #geom_node_label(aes(label = Region), repel = TRUE)+
  scale_edge_width(limits=c(0,100),breaks=seq(0, 100, by=10),name="Individuals (n)")+ # control size
  scale_edge_colour_gradient(low = "lightblue", high = "steelblue",limits=c(0,100),breaks=seq(0, 100, by=10),name="Individuals (n)") +
  # scale_edge_color_continuous(
  #   low = "lightgrey",
  #   high = "#003366",
  #   space = "Lab",
  #  # na.value = "grey50",
  #  # guide = "edge_colourbar",
  #   limits=c(0,100),
  #   breaks=seq(0, 100, by=10) )+
theme_void()+
  labs(title = paste0(regions[[1]]))# ,' individual recaptures')), 
      # subtitle = 'Within-region recaptures over available sampling timeframe, including one observation of an individual per node.')



###make adj matrix
#edges <- unique(edge)[, if (.N > 1L) transpose(combn(X1, 2L, simplify=FALSE)), X2,   .N, .(V1, V2)]

#set_edge_attr(g, "weight", value=n)
#to check weights, use get.data.frame(g)

#wide_edge <- pivot_wider(edge, names_from = X1, values_from =  )
# 
# edgelists <- as.matrix(edge)
# 
# nodes <- c(seq(1,7,by=1))
# 
# # make graph from edgelist - use this for sizing nodes to degree of connectivity
# glab = graph.edgelist(edgelists, directed = FALSE)
# 
# 
# # get connection weights
# byweight <- edge %>% group_by(X1,X2) %>% tally
# # matrix time
# short_edge <- byweight %>% select(X1,X2) %>% as.matrix(.)
# # convert the edgelist to a igraph obj
# g <- graph_from_edgelist(short_edge, directed=FALSE)
# # weights please
# E(g)$weight <- byweight$n
# 
# # # get degree of node connections
# # lab_degree = degree(glab)
# # # get clusters
# # gclus = clusters(glab)
# # 
# # # set list for node order
# # #node_order <- c("CA-South", "CA-Central","CA-North", "OR", "SBC-WA", "NBC")
# # 
# # par(mar = c(1,5,1,1))
# # # plot a pretty arc diagram
# # arcplot(edgelists, lwd.arcs = ((E(g)$weight)/10), col.arcs=grey.colors(1,alpha=0.8),
# #         cex.nodes = log(lab_degree)+.5, col.nodes = "gray75", bg.nodes = "gray90", pch.nodes=21,
# #         show.nodes = TRUE, horizontal = FALSE,lwd.nodes=2, ljoin = 1,lend = 3)
# # 
# # 








  
  
###########################
  
  geom_edge_arc(aes(edge_width=byweight$n), edge_alpha=0.5, fold=T)+
  geom_node_point(aes(size=strength(glab), color=as.factor(color)))+
  geom_node_text(aes(label=name), angle=90, hjust=1, nudge_y = -0.2, size=4)+
  theme_void()+theme(legend.position = "none")



# ###acutal arcdiagram scraps
# # Number of connection per lat bin
# # lat_bins <- links %>% count(source) %>% summarize(n=n())
# # colnames(lat_bins) <- c("LAT", "n")
# 
# #add a regional grping for coloring nodes
# #lat_bins <- lat_bins %>% 
# #        mutate(
# #         grp = case_when(
# #                LAT <= 36 ~ "CA-S",
# #                (LAT > 36 & LAT <= 39  ) ~ "CA-C",
# #                (LAT > 39 & LAT <= 42 ) ~ "CA-N",
# #                (LAT > 42 & LAT <= 46) ~ "OR",
# #                (LAT > 46 & LAT <= 50) ~ "SBC_WA",
# #                LAT > 50 ~ "NBC"))
# 
# #make the igraph obj
# #mygraph <- graph_from_data_frame(links, vertices = lat_bins, directed = FALSE)
# mygraph <- graph_from_data_frame(d=edges, vertices = lat_bins, directed = FALSE)
# 
# library(tidygraph)
# net.tidy <- tbl_graph(
#         nodes = lat_bins, edges = edges, directed = FALSE
# )
# 
# 
# # get edges value
# values = get.edge.attribute(mygraph)
# 
# #node colors - need to group lat bins by state lines
# mycolor <- colormap(colormap=colormaps$viridis, nshades=6)
# 
# 
# 
# 
# arcdiagram <- ggraph(mygraph, layout="linear") + 
#         geom_edge_arc(edge_colour="lightgrey", edge_alpha=0.2, edge_width = n, fold=TRUE) +
#         geom_node_point(aes(size=n, color=as.factor(grp), fill=grp), alpha=0.5) +
#         scale_size_continuous(range=c(0.5,8)) +
#         scale_color_manual(values=mycolor) +
#         geom_node_text(aes(label=name), angle=65, hjust=1, nudge_y = -1.1, size=4) +
#         labs(as.factor(grp) = "Region")+ 
#         theme_void() +
#         theme(
#                 legend.position="bottom",
#                 plot.margin=unit(c(0,0,0.4,0), "null"),
#                 panel.spacing=unit(c(0,0,3.4,0), "null")
#         ) +
#         expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2)) 
# 
# show(arcdiagram)
# 
# ####GRAVEYARD####
# 
# # Make the usual network diagram w/fanned connections
# p1 <-  ggraph(mygraph) + 
#         geom_edge_fan(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
#         geom_node_point(aes(size=n, color=as.factor(grp), fill=grp), alpha=0.5) +
#         scale_size_continuous(range=c(0.5,8)) +
#         geom_node_text( aes(label=name), repel = TRUE, size=8, color="black") +
#         theme_void() +
#         theme(
#                 legend.position="none",
#                 plot.margin=unit(rep(2,4), "cm")
#         ) 
# p1
# 
# 
# 
# 
# 
# # Make a cord diagram
p2 <-  ggraph(glab, layout="linear") +
        geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width= values) +
        geom_node_point(aes(size=n, alpha=0.5)) +
        geom_node_text( aes(label=name), repel = FALSE, size=8, color="black", nudge_y=-0.1) +
        theme_void() +
        theme(
                legend.position="none",
                plot.margin=unit(rep(2,4), "cm")
        )
p2







ggraph(glab, layout="linear")+
  geom_edge_arc(aes( edge_alpha=0.5, fold=T))+
  geom_node_point(aes(size=2, color="dodgerblue"))
  
  +
  #geom_node_text(aes(label=node), angle=90, hjust=1, nudge_y = -0.2, size=4)+
  theme_void()+theme(legend.position = "none")
