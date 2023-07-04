library(muxViz)
library(openxlsx)
library(igraph)
library(readxl)

# Function to calculate variables for a given file
calculateVariables <- function(file) {
  # Read the sheet
  mEdges <- read.xlsx(file)
  
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
  
  # Calculate the inter-assortativity tensor
  IAS <- GetInterAssortativityTensor(SA, Layers, Nodes, isDirected = TRUE, Type = "OO")
  
  # Calculate other centrality measures
  CN <- GetMultiAuthCentrality(SA, Layers, Nodes)
  MC <- GetMultiClosenessCentrality(SA, Layers, Nodes)
  MD <- GetMultiDegreeSum(SA, Layers, Nodes, isDirected = TRUE)
  MEV <- GetMultiEigenvectorCentrality(SA, Layers, Nodes)
  MH <- GetMultiHubCentrality(SA, Layers, Nodes)
  MKZ <- GetMultiKatzCentrality(SA, Layers, Nodes)
  MKC <- GetMultiKCoreCentrality(SA, Layers, Nodes)
  MPR <- GetMultiPageRankCentrality(SA, Layers, Nodes)
  MRW <- GetMultiRWCentrality(SA, Layers, Nodes, Type = "classical", Method = "multilayer")
  
  # Store the calculated variables
  results <- list(AM = AM, TM = TM, IAS = IAS, CN = CN, MC = MC, MD = MD, MEV = MEV, MH = MH, MKZ = MKZ, MKC = MKC, MPR = MPR, MRW = MRW)
  
  return(results)
}

# Read the Excel files
file1 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/1.edgelist_default.xlsx"
file2 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/2.HORTA Electric Demand OFF.xlsx"
file3 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/3.NO166,No167 Gas Valve OFF.xlsx"
file4 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/4.HORTA, NO166NO167 OFF.xlsx"
file5 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/5.CHAMPION, NO45NO52 OFF.xlsx"

# Calculate variables for each file
results1 <- calculateVariables(file1)
results2 <- calculateVariables(file2)
results3 <- calculateVariables(file3)
results4 <- calculateVariables(file4)
results5 <- calculateVariables(file5)


### TESTING TO DISPLAY #########

# Function to display the MultiPageRank results for a given file
displayMPR <- function(file, results) {
  cat("MultiPageRank results for file:", file, "\n")
  cat("\nMPR:\n")
  print(results$MD)
  cat("\n---------------------------\n")
}

# Display MultiPageRank results for file5
displayMPR(file3, results3)
