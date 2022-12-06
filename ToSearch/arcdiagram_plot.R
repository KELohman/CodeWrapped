#reproducible example code for linear network graph below
# from https://www.data-to-viz.com/graph/arc.html

# Libraries
#library(tidyverse)
#library(viridis)
#library(tidygraph)
#library(patchwork)
#library(hrbrthemes)
library(igraph)
library(arcdiagram)
#library(ggraph)
#library(colormap)

#load in data

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
# p2 <-  ggraph(mygraph, layout="linear") + 
#         geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width= values) +
#         geom_node_point(aes(size=n, color=as.factor(grp), fill=grp), alpha=0.5) +
#         geom_node_text( aes(label=name), repel = FALSE, size=8, color="black", nudge_y=-0.1) +
#         theme_void() +
#         theme(
#                 legend.position="none",
#                 plot.margin=unit(rep(2,4), "cm")
#         ) 
# p2
# 
# 
# ####arcdiagram pkg plot####
# # load arcdiagram
# library(arcdiagram)
# 
# # get edgelist
# edgelist = get.edgelist(mygraph)
# # get vertex labels
# vlabels = get.vertex.attribute(mygraph, "label")
# # get vertex groups
# vgroups = get.vertex.attribute(mygraph, "grp")
# # get vertex fill color
# vfill = get.vertex.attribute(mis_graph, "fill")
# # get vertex border color
# vborders = get.vertex.attribute(mis_graph, "border")
# # get vertex degree
# degrees = degree(mis_graph)
# # get edges value
# values = get.edge.attribute(mis_graph, "value")
# 
# arcplot(edgelist, ordering=new_ord, labels=vlabels, cex.labels=0.8,
#         show.nodes=TRUE, col.nodes=vborders, bg.nodes=vfill,
#         cex.nodes = log(degrees)+0.5, pch.nodes=21,
#         lwd.nodes = 2, line=-0.5,
#         col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values)




#https://stackoverflow.com/questions/38221588/combinations-by-group-in-r

# recap'd individuals
recaps1 <- encounters %>%
        drop_na(TrueRegion) %>% # must have locations
        filter(between(Month, 05,10)) %>% 
        distinct(GeneticID, TrueRegion) %>% # one obs of an indiv per region
        group_by(GeneticID) %>% 
        filter(n()>1) %>% # must have a between region recap
        ungroup() %>% 
        rowid_to_column() %>% 
        rename(X = "rowid")

#recaps1$TrueRegion <- factor(recaps1$TrueRegion, levels = c("CA-South", "CA-Central","CA-North", "OR", "SBC-WA", "NBC"))

#regions <- c(unique(recaps1$TrueRegion))
# list 'o edges
edge <- recaps1 %>%
        group_by(GeneticID) %>% 
        arrange(TrueRegion, .by_group = TRUE) %>% 
                do(data.frame(t(combn(.$TrueRegion,2)))) %>% 
                ungroup() %>% 
        select(-GeneticID)

edgelists <- as.matrix(edge)

# list 'o edges
# edge <- recaps1 %>%
#         group_by(GeneticID) %>% 
#         do(data.frame(t(combn(.$TrueRegion,2)))) %>% 
#         ungroup() %>% 
#         select(-GeneticID)

# make graph from edgelist - use this for sizing nodes to degree of connectivity
glab = graph.edgelist(edgelists, directed = FALSE)

# get connection weights
byweight <- edge %>% group_by(X1,X2) %>% tally
# matrix time
short_edge <- byweight %>% select(X1,X2) %>% as.matrix(.)
# convert the edgelist to a igraph obj
g <- graph_from_edgelist(short_edge, directed=FALSE)
# weights please
E(g)$weight <- byweight$n

# get degree of node connections
lab_degree = degree(glab)
# get clusters
gclus = clusters(glab)

# set list for node order
node_order <- c("CA-South", "CA-Central","CA-North", "OR", "SBC-WA", "NBC")

par(mar = c(1,5,1,1))
# plot a pretty arc diagram
arcplot(edgelists, lwd.arcs = ((E(g)$weight)/10), col.arcs=grey.colors(1,alpha=0.8),
        cex.nodes = log(lab_degree)+.5, col.nodes = "gray75", bg.nodes = "gray90", pch.nodes=21,
        show.nodes = TRUE, horizontal = FALSE, ordering = node_order,lwd.nodes=2, ljoin = 1,lend = 3)
