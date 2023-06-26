library(muxViz)
library(openxlsx)
library(igraph)
library(readxl)

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
AG2<- GetAggregateNetworkFromSupraAdjacencyMatrix( SA2, Layers, Nodes )

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

AG3<- GetAggregateNetworkFromSupraAdjacencyMatrix( SA3, Layers, Nodes )
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

AG4<- GetAggregateNetworkFromSupraAdjacencyMatrix( SA4, Layers, Nodes )
#NT4 <- SupraAdjacencyToNodesTensor(SA4, Layers, Nodes4)

mEdges5<- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/5.CHAMPION, NO45NO52 OFF.xlsx")
Layers <- 2
Nodes5 <- max(max(mEdges5$From_Node), max(mEdges5$To_Node))

# Build the supra-adjacency matrix
SA5 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges5,
  Layers = Layers,
  Nodes = Nodes5,
  isDirected = TRUE
)

AG5<- GetAggregateNetworkFromSupraAdjacencyMatrix( SA5, Layers, Nodes )
#NT5 <- SupraAdjacencyToNodesTensor(SA5, Layers, Nodes5)


# Build the node tensor
node_tensor <- lapply(list(AG1, AG2, AG3, AG4, AG5), as_adjacency_matrix)
node_tensor
# Build the layer tensor
layer_tensor <- diagR(c(1, 1), 5, 1) + diagR(c(1, 1), 5, -1)
layer_tensor
# Build the supra-adjacency matrix
M <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(node_tensor, layer_tensor, 5, Nodes)
