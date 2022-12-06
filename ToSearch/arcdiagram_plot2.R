#reproducible example code for linear network graph below
# from https://www.data-to-viz.com/graph/arc.html

# Libraries
#library(tidygraph)
#library(patchwork)
library(igraph)
library(arcdiagram)
library(RColorBrewer)
#library(ggraph)

# relies on encounter df from ch 1 docs

###
# helpful so post about making the edge list
#https://stackoverflow.com/questions/38221588/combinations-by-group-in-r
setwd("~/Research/Ch_1_mtDNA_Structure/figures")

# recap'd individuals
recaps1 <- encounters %>%
        distinct(GeneticID, TrueRegion) %>% # one obs of an indiv per region
        group_by(GeneticID) %>% 
        filter(n()>1) %>% # must have a between region recap
        ungroup() %>% 
        rowid_to_column() %>% 
        rename(X = "rowid")

# list 'o edges
edge <- recaps1 %>%
        group_by(GeneticID) %>% 
        arrange(TrueRegion, .by_group = TRUE) %>% 
                do(data.frame(t(combn(.$TrueRegion,2)))) %>% 
                ungroup() %>% 
        select(-GeneticID)


### for edges
# covert to matrix
edgelists <- as.matrix(edge)

# make graph from edgelist - use this for sizing nodes to degree of connectivity
glab = graph.edgelist(edgelists, directed = FALSE)

# set every link weight to 1
E(glab)$weight <- 1 

# add the double loop arcs together for weights 
glabs <- simplify(glab, edge.attr.comb=c(weight="sum", type="ignore") )

### for nodes
# get connection weights
byweight <- edge %>% group_by(X1,X2) %>% tally
# matrix time
short_edge <- byweight %>% select(X1,X2) %>% as.matrix(.)

# get degree of node connections
lab_degree = degree(glabs)

# set list for node order
node_order <- c("CA-South", "CA-Central","CA-North", "OR", "SBC-WA", "NBC")

#Classic palette BuPu, with 4 colors
#cols <- rev(brewer.pal(15, "Blues")) 

cols <- rev(RColorBrewer::brewer.pal(15, "Blues")[3:9])

# adjust plot margins for long labels
par(mar = c(1,7,1,1))
# plot a pretty arc diagram

dev.copy(png,'arc.png')

        arcplot(short_edge, 
              lwd.arcs = ((E(glabs)$weight)/8), col.arcs=cols, 
              cex.nodes = (lab_degree), col.nodes = "gray40", bg.nodes = "grey80", pch.nodes=21, show.nodes = TRUE, 
              horizontal = FALSE, ordering = node_order,
              lwd.nodes=1, ljoin = 1,lend = 3)


dev.off()

### graveyard
# ggraph(glabs, layout="linear")+
#         geom_edge_arc(aes(edge_width=weight), edge_alpha=weight, fold=T)+
#         geom_node_point(aes(size=strength(glabs), color="darkgrey"))+
#         geom_node_text(aes(label=name), angle=90, hjust=1, nudge_y = -0.2, size=4)+
#         theme_void()+
#         coord_cartesian(clip = "off") +
#         theme(legend.position = "none")+
#         plot.margin = unit(rep(30, 4), "points")