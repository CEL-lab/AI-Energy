library(muxViz)
library(openxlsx)
library(igraph)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(MASS)  # Load the MASS package for ginv()

mEdges <- read.xlsx("D:/NDSU/PhD Work/Research/IME Research/extended file.xlsx")

# Determine the number of layers
Layers <- 2
Nodes <- max(max(mEdges$From_Node), max(mEdges$To_Node))

Nodes
Layers

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

# Calculate the inter-assortativity tensor
IAS <- GetInterAssortativityTensor(SA, Layers, Nodes, isDirected = TRUE, Type = "OO")

CN <- GetMultiAuthCentrality(SA, Layers, Nodes)
MC <- GetMultiClosenessCentrality(SA, Layers, Nodes)
MD <- GetMultiDegreeSum(SA, Layers, Nodes, isDirected = TRUE)
MEV <- GetMultiEigenvectorCentrality(SA, Layers, Nodes)
MH <- GetMultiHubCentrality(SA, Layers, Nodes)
MKZ <- GetMultiKatzCentrality(SA, Layers, Nodes)
MC <- GetMultiKCoreCentrality(SA, Layers, Nodes)
MPR <- GetMultiPageRankCentrality(SA, Layers, Nodes)
MRW <- GetMultiRWCentrality(SA, Layers, Nodes, Type = "classical", Method = "multilayer")

print(TM)
head(TM)
# Call the modified GetCoverageEvolutionMultilayer function
CEM <- GetCoverageEvolutionMultilayer(TM, Layers, Nodes, TimeSequence, Approximate = FALSE, Approximate.disconnected = 222)

print(CEM)


#Ploting 

#Assortativity
LL.cor3 <- IAS$InterPearson
LL.cor3.df <- melt(as.matrix(LL.cor3))
LL.cor3.df$type <- "Deg-deg Pearson"

LL.cor4 <- IAS$InterSpearman
LL.cor4.df <- melt(as.matrix(LL.cor4))
LL.cor4.df$type <- "Deg-deg Spearman"

LL.cor.df <- rbind(LL.cor3.df, LL.cor4.df)

p <- ggplot(LL.cor.df, aes(Var1, Var2, fill=value, group=type)) + theme_bw() +
  theme(panel.grid=element_blank()) + xlab("") + ylab("") +
  scale_fill_gradientn(colors=brewer.pal(9, "YlOrRd")) +
  geom_tile() + 
  facet_wrap(.~type, ncol=2)

print(p)






