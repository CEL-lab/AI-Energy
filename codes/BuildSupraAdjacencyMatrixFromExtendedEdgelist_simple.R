library(muxViz)
library(openxlsx)
library(igraph)


mEdges <- read.xlsx("D:/NDSU/PhD Work/Research/IME Research/extended file.xlsx")

# Determine the number of layers
Layers <- 2
Nodes<-max(max(mEdges$From_Node), max(mEdges$To_Node))

Nodes
Layers

# Build the supra-adjacency matrix
SA<- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges=mEdges,
  Layers = Layers,
  Nodes = Nodes,
  isDirected = TRUE
)
plot(SA)

# Create the multilayer network
network <- graph_from_adjacency_matrix(SA, mode = "directed")

# Set the layer attribute for each node
V(network)$layer <- rep(1:Layers, each = Nodes)

# Set the layout algorithm
layout <- layout_with_fr(network)

# Plot the network with customized options
plot(
  network,
  layout = layout,
  vertex.size = 10,
  vertex.label = NA,
  edge.width = 0.8,
  edge.arrow.size = 0.5,
  edge.curved = 0.2,
  edge.color = "gray",
  vertex.color = c("skyblue", "lightgreen"),
  vertex.frame.color = "white",
  vertex.label.color = "black",
  main = "Multilayer Network Plot"
)
