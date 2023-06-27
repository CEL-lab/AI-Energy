library(muxViz)
library(openxlsx)
library(igraph)
library(readxl)
library(RColorBrewer)
library(ggplot2)
library(ggraph)
library(multinet)

setwd("/Users/harunpirim/Downloads/AI-Energy-main/data")
# Read and process the first excel file

mEdges1 <- read.xlsx("1.edgelist_default.xlsx")
Layers1 <- 2
Nodes1 <- max(max(mEdges1$From_Node), max(mEdges1$To_Node))

# Build the supra-adjacency matrix
SA1 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges1,
  Layers = Layers1,
  Nodes = Nodes1,
  isDirected = TRUE
)
AG1 <- GetAggregateNetworkFromSupraAdjacencyMatrix(SA1, Layers1, Nodes1)
AG1 <- GetAggregateNetworkFromSupraAdjacencyMatrix(SA1, Layers1, Nodes1)
dim(AG1)


mEdges2 <- read.xlsx("2.HORTA Electric Demand OFF.xlsx")
Layers <- 2
Nodes2 <- max(max(mEdges2$From_Node), max(mEdges2$To_Node))

# Build the supra-adjacency matrix
SA2 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges2,
  Layers = Layers,
  Nodes = Nodes2,
  isDirected = TRUE
)
#NT2 <- SupraAdjacencyToNodesTensor(SA2, Layers, Nodes2)
AG2<- GetAggregateNetworkFromSupraAdjacencyMatrix( SA2, Layers, Nodes2 )

mEdges3<- read.xlsx("3.NO166,No167 Gas Valve OFF.xlsx")
Layers <- 2
Nodes3 <- max(max(mEdges3$From_Node), max(mEdges3$To_Node))

# Build the supra-adjacency matrix
SA3 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges3,
  Layers = Layers,
  Nodes = Nodes3,
  isDirected = TRUE
)

AG3<- GetAggregateNetworkFromSupraAdjacencyMatrix( SA3, Layers, Nodes3 )
#NT3 <- SupraAdjacencyToNodesTensor(SA3, Layers, Nodes3)

mEdges4<- read.xlsx("4.HORTA, NO166NO167 OFF.xlsx")
Layers <- 2
Nodes4 <- max(max(mEdges4$From_Node), max(mEdges4$To_Node))

# Build the supra-adjacency matrix
SA4 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges4,
  Layers = Layers,
  Nodes = Nodes4,
  isDirected = TRUE
)

AG4<- GetAggregateNetworkFromSupraAdjacencyMatrix( SA4, Layers, Nodes4 )
#NT4 <- SupraAdjacencyToNodesTensor(SA4, Layers, Nodes4)

mEdges5<- read.xlsx("5.CHAMPION, NO45NO52 OFF.xlsx")
Layers <- 2
Nodes5 <- max(max(mEdges5$From_Node), max(mEdges5$To_Node))

# Build the supra-adjacency matrix
SA5 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges5,
  Layers = Layers,
  Nodes = Nodes5,
  isDirected = TRUE
)

AG5<- GetAggregateNetworkFromSupraAdjacencyMatrix( SA5, Layers, Nodes5 )
#NT5 <- SupraAdjacencyToNodesTensor(SA5, Layers, Nodes5)

Layers <- 5
Nodes <- Nodes1
# Build the node tensor
node_tensor <- lapply(list(AG1, AG2, AG3, AG4, AG5), as_adjacency_matrix, attr = 'weight')
node_tensor
# Build the layer tensor
layer_tensor <- diagR(c(1, 1), 5, 1) + diagR(c(1, 1), 5, -1)
layer_tensor
# Build the supra-adjacency matrix
M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(node_tensor, layer_tensor, 5, Nodes1)
g.list <- list(AG1,AG2,AG3,AG4,AG5)

#Calculate PageRank and Degree versatility
pr <- GetMultiPageRankCentrality(M, Layers,Nodes)
deg <- GetMultiDegree(M, Layers,Nodes, isDirected=T)

mypal <- brewer.pal(Layers, "Set1")

#Generate the coordinates for layouting our networks.
lay <- layout_with_fr(graph_from_adjacency_matrix( GetAggregateMatrix(node_tensor, Layers, Nodes) ))

p <- list()

for(l in 1:length(node_tensor)){
  layout <- create_layout(g.list[[l]], layout = 'drl')
  layout$x <- lay[,1]
  layout$y <- lay[,2]
  
  V(g.list[[l]])$pr <- pr
  V(g.list[[l]])$deg <- deg
  
  p[[l]] <- ggraph(layout) + theme_void() +
    geom_edge_link(colour=mypal[l], show.legend = FALSE) + 
    geom_node_point(aes(size = pr, alpha=pr), color=mypal[l]) + 
    theme(legend.position="bottom", plot.title=element_text(size=16, hjust=0.5, face="bold", colour=mypal[l], vjust=-1)) + 
    ggtitle(paste("Layer", l)) +
    guides(size=guide_legend(title="MuxPR"), alpha='none')
}

png("mux_AN_5layers.png", width=1024, height=1024*0.5, res=120)
multiplot(p[[1]], p[[2]], p[[3]],p[[4]],p[[5]], cols=5)
dev.off()


#multinet object code 
for(i in 1:5){
  assign(paste0("layer",i), get(paste0('AG',i)))
  V(get(paste0("layer",i)))$name <- V(get(paste0('layer',i)))
}

mgraph <- ml_empty()
for (k in 1:5) {
  if(all(as.matrix(!get.adjacency(get(paste0('layer',k)))))==0){
    add_igraph_layer_ml(mgraph,get(paste0('layer',k)), paste0("layer",k))
  }
}
mgraph
summary(mgraph) 

