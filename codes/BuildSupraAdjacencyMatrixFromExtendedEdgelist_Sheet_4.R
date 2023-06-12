library(muxViz)
library(openxlsx)
library(igraph)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(dplyr)
library(Matrix)
#install.packages("networkD3")

mEdges <- read.xlsx("D:/NDSU/PhD Work/Research/IME Research/4.HORTA, NO166NO167 OFF.xlsx")

# Read the Excel sheet into a data frame
#df <- read_excel("D:/NDSU/PhD Work/Research/IME Research/weighted_edgelist.xlsx")

# Convert Weight column to numeric
#df$Weight <- as.numeric(df$Weight)

# Swap From_Node and To_Node columns where Weight is negative and update weight to be positive
#df <- df %>%
 # mutate(
  #  temp = ifelse(Weight < 0, From_Node, NA),
   # From_Node = ifelse(Weight < 0, To_Node, From_Node),
    #To_Node = ifelse(Weight < 0, temp, To_Node),
    #Weight = ifelse(Weight < 0, abs(Weight), Weight)
  #) %>%
 # select(-temp)

# Filter out rows with Weight not equal to 0.00
#df <- df %>%
#  filter(Weight != 0.00)

# Print the modified data frame
#print(df)



# Determine the number of layers
Layers <- 2
Nodes <- max(max(mEdges$From_Node), max(mEdges$To_Node))


# Build the supra-adjacency matrix
SA <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges,
  Layers = Layers,
  Nodes = Nodes,
  isDirected = TRUE
)

# Calculate the aggregate network
AM <- GetAggregateNetworkFromSupraAdjacencyMatrix(SA, Layers, Nodes)

# Calculate the Transition Matrix
TM <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA, Layers, Nodes)

# Create a graph object from the supra-transition matrix
g <- graph.adjacency(TM, mode = "directed", weighted = TRUE)

# Set the layout algorithm for node positioning
layout <- layout_with_fr(g)  # You can use other layout algorithms as well

# Plot the network graph
plot(g, layout = layout, edge.width = E(g)$weight, edge.arrow.size = 0.5, vertex.label = NA)

# Calculate the inter-assortativity tensor
IAS <- GetInterAssortativityTensor(SA, Layers, Nodes, isDirected = TRUE, Type = "OO")


CN <- GetMultiAuthCentrality(SA, Layers, Nodes)

TM<-BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA,Layers,Nodes,Type = "pagerank")
MC <- GetMultiClosenessCentrality(SA, Layers, Nodes)
MC
#hist(MC)
hist((MC$closeness))
plot(MC$closeness,1:222)

MD <- GetMultiDegreeSum(SA, Layers, Nodes, isDirected = TRUE)

MEV <- GetMultiEigenvectorCentrality(SA, Layers, Nodes)
MH <- GetMultiHubCentrality(SA, Layers, Nodes)
MKZ <- GetMultiKatzCentrality(SA, Layers, Nodes)
MC <- GetMultiKCoreCentrality(SA, Layers, Nodes)
MPR <- GetMultiPageRankCentrality(SA, Layers, Nodes)
MRW <- GetMultiRWCentrality(SA, Layers, Nodes, Type = "classical", Method = "multilayer")
#print(TM)
#head(TM)
# Call the modified GetCoverageEvolutionMultilayer function
CEM <- GetCoverageEvolutionMultilayer(TM, Layers,Nodes,1:5)

print(CEM)





