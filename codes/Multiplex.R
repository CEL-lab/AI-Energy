library(muxViz)
library(openxlsx)
library(igraph)
library(readxl)
library(RColorBrewer)
library(ggplot2)
library(ggraph)
library(multinet)

#setwd("/Users/harunpirim/Downloads/AI-Energy-main/data")
# Read and process the first excel file

mEdges1 <- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/1.edgelist_default.xlsx")
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


mEdges2 <- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/2.HORTA Electric Demand OFF.xlsx")
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

mEdges3<- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/3.NO166,No167 Gas Valve OFF.xlsx")
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

mEdges4<- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/4.HORTA, NO166NO167 OFF.xlsx")
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

mEdges5<- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/5.CHAMPION, NO45NO52 OFF.xlsx")
Layers5 <- 2
Nodes5 <- max(max(mEdges5$From_Node), max(mEdges5$To_Node))
class(Nodes5)
class(mEdges5[, 2])

# Build the supra-adjacency matrix
SA5 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges5,
  Layers = Layers5,
  Nodes = Nodes5,
  isDirected = TRUE
)

AG5<- GetAggregateNetworkFromSupraAdjacencyMatrix( SA5, Layers5, Nodes5 )
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

#Multinet Code: UPDATED

for (i in 1:5) {
  layer <- get(paste0("AG", i))
  V(layer)$name <- V(layer)
  assign(paste0("layer", i), layer)
}

mgraph <- ml_empty()
for (k in 1:5) {
  if(all(as.matrix(!get.adjacency(get(paste0('layer',k)))))==0){
    add_igraph_layer_ml(mgraph,get(paste0('layer',k)), paste0("layer",k))
  }
}
mgraph
summary(mgraph) 

#Multinet Analysis: 

layer_comparison_ml(mgraph, method ="jeffrey.degree")
layer_comparison_ml(mgraph, method = "pearson.degree")
layer_comparison_ml(mgraph, method = "jaccard.edges")

#Community detection algorithms and evaluation functions

# Detect communities using Abacus algorithm
communities_AB <- abacus_ml(mgraph, min.actors = 3, min.layers = 1)

# Calculate modularity using modularity_ml function
modularity_AB <- modularity_ml(mgraph, communities_AB, gamma = 1, omega = 1)

# Print the detected communities & Modularity
print(communities_AB)
print(modularity_AB)

# Transform Abacus communities into a list format
community_list_AB <- get_community_list_ml(communities_AB, mgraph)

# Print the Abacus community list
print(community_list_AB)


# Detect communities using Flat EC algorithm & Modularity
communities_EC <- flat_ec_ml(mgraph)

# Calculate modularity using modularity_ml function
modularity_EC <- modularity_ml(mgraph, communities_EC, gamma = 1, omega = 1)

# Print the detected communities & Modularity using Flat EC algorithm 
print(communities_EC)
print(modularity_EC)

# Calculate NMI between communities using Abacus algorithm & Flat EC algorithm using nmi_ml function
nmi_value1_2 <- nmi_ml(mgraph, communities_AB, communities_EC)
print(nmi_value1_2)

# Calculate Omega Index between using Abacus algorithm & Flat EC algorithm using omega_index_ml function
omega_index1_2 <- omega_index_ml(mgraph, communities_AB, communities_EC)

# Print the Omega Index value
print(omega_index1_2)

# Transform Flat EC communities into a list format
community_list_EC <- get_community_list_ml(communities_EC, mgraph)

# Print the Flat EC community list
print(community_list_EC)

# Detect communities using Flat Newman-Watts algorithm
communities_NW <- flat_nw_ml(mgraph)

# Calculate modularity using modularity_ml function
modularity_NW <- modularity_ml(mgraph, communities_NW, gamma = 1, omega = 1)

# Print the detected communities & Modularity using Flat Newman-Watts algorithm 
print(communities_NW)
print(modularity_NW)

# Transform Flat Newman-Watts communities into a list format
community_list_NW <- get_community_list_ml(communities_NW, mgraph)

# Print the Flat Newman-Watts community list
print(community_list_NW)

# Detect communities using Clique Percolation Method (CPM) algorithm
communities_CPM <- clique_percolation_ml(mgraph, k = 3, m = 1)

# Calculate modularity using modularity_ml function
modularity_CPM <- modularity_ml(mgraph, communities_CPM, gamma = 1, omega = 1)

# Print the detected communities & Modularity using Clique Percolation Method (CPM) algorithm 
print(communities_CPM)
print(modularity_CPM)

# Transform CPM communities into a list format
community_list_CPM <- get_community_list_ml(communities_CPM, mgraph)

# Print the CPM community list
print(community_list_CPM)

# Calculate NMI between communities using Flat Newman-Watts & Clique Percolation Method (CPM) algorithm using nmi_ml function
nmi_value3_4 <- nmi_ml(mgraph, communities_NW, communities_CPM)
print(nmi_value3_4)

# Calculate Omega Index between using Flat Newman-Watts & Clique Percolation Method (CPM) using omega_index_ml function
omega_index3_4 <- omega_index_ml(mgraph, communities_NW, communities_CPM)

# Print the Omega Index value
print(omega_index3_4)

# Detect communities using Generalized Louvain algorithm
communities_GLA <- glouvain_ml(mgraph, gamma = 1, omega = 1)

# Calculate modularity using modularity_ml function
modularity_GLA <- modularity_ml(mgraph, communities_GLA, gamma = 1, omega = 1)

# Print the detected communities & Modularity using Generalized Louvain algorithm 
print(communities_GLA)
print(modularity_GLA)

# Transform Generalized Louvain communities into a list format
community_list_GLA <- get_community_list_ml(communities_GLA, mgraph)

# Print the Generalized Louvain community list
print(community_list_GLA)

# Detect communities using Infomap algorithm
communities_IMA <- infomap_ml(mgraph, overlapping = FALSE, directed = TRUE, self.links = FALSE)

# Calculate modularity using modularity_ml function
modularity_IMA <- modularity_ml(mgraph, communities_IMA, gamma = 1, omega = 1)

# Print the detected communities & Modularity using Infomap algorithm algorithm 
print(communities_IMA)
print(modularity_IMA)

# Transform Infomap communities into a list format
community_list_IMA <- get_community_list_ml(communities_IMA, mgraph)

# Print the Infomap community list
print(community_list_IMA)

# Calculate NMI between communities using Generalized Louvain & Infomap algorithm algorithm using nmi_ml function
nmi_value5_6 <- nmi_ml(mgraph, communities_GLA, communities_IMA)
print(nmi_value5_6)

# Calculate Omega Index between using Generalized Louvain & Infomap algorithm using omega_index_ml function
omega_index5_6 <- omega_index_ml(mgraph, communities_GLA, communities_IMA)

# Print the Omega Index value
print(omega_index5_6)


