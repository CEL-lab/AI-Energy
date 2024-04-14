library(muxViz)
library(openxlsx)
library(igraph)
library(readxl)
library(dplyr)
library(tidyr)
library(hrbrthemes)
library(scales)

# Function to calculate variables for a given file
calculateVariables <- function(edge_list) {
  # Read the sheet
  #mEdges <- read.xlsx(file)
  Layers <- 2
  Nodes <- max(max(edge_list$From_Node), max(edge_list$To_Node))
  
  # Build the supra-adjacency matrix
  SA <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
    mEdges = edge_list,
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
  AC  <- GetMultiAuthCentrality(SA, Layers, Nodes)
  MC  <- GetMultiClosenessCentrality(SA, Layers, Nodes)
  MD  <- GetMultiDegreeSum(SA, Layers, Nodes, isDirected = TRUE)
  MEV <- GetMultiEigenvectorCentrality(SA, Layers, Nodes)
  MH  <- GetMultiHubCentrality(SA, Layers, Nodes)
  MKZ <- GetMultiKatzCentrality(SA, Layers, Nodes)
  MKC <- GetMultiKCoreCentrality(SA, Layers, Nodes)
  MPR <- GetMultiPageRankCentrality(SA, Layers, Nodes)
  MRW <- GetMultiRWCentrality(SA, Layers, Nodes, Type = "classical", Method = "multilayer")
  
  # Store the calculated variables
  results <- list(SA = SA, AM = AM, TM = TM, IAS = IAS, AC = AC, MC = MC, MD = MD, MEV = MEV, MH = MH, MKZ = MKZ, MKC = MKC, MPR = MPR, MRW = MRW)
  
  return(results)
}

# Read the Excel files
file1 <- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/1.edgelist_default.xlsx")
file2 <- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/2.HORTA Electric Demand OFF.xlsx")
file3 <- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/3.NO166,No167 Gas Valve OFF.xlsx")
file4 <- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/4.HORTA, NO166NO167 OFF.xlsx")
file5 <- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/5.CHAMPION, NO45NO52 OFF.xlsx")

# Extract the edge lists from the data frames
edge_list1 <- file1
edge_list2 <- file2
edge_list3 <- file3
edge_list4 <- file4
edge_list5 <- file5

# Calculate variables for each file
results <- list(
  results1 = calculateVariables(file1),
  results2 = calculateVariables(file2),
  results3 = calculateVariables(file3),
  results4 = calculateVariables(file4),
  results5 = calculateVariables(file5)
)

# Assign variables using a loop
for (i in 1:length(results)) {
  assign(paste0("SA", i), results[[i]]$SA)
  assign(paste0("AM", i), results[[i]]$AM)
  assign(paste0("TM", i), results[[i]]$TM)
  assign(paste0("IAS", i), results[[i]]$IAS)
  assign(paste0("AC", i), results[[i]]$AC)
  assign(paste0("MC", i), results[[i]]$MC)
  assign(paste0("MD", i), results[[i]]$MD)
  assign(paste0("MEV", i), results[[i]]$MEV)
  assign(paste0("MH", i), results[[i]]$MH)
  assign(paste0("MKZ", i), results[[i]]$MKZ)
  assign(paste0("MKC", i), results[[i]]$MKC)
  assign(paste0("MPR", i), results[[i]]$MPR)
  assign(paste0("MRW", i), results[[i]]$MRW)
  #assign(paste0("CEM", i), results[[i]]$CEM)
}
########## Coverage Function #################

mEdges1 <- read.xlsx(file1)
Layers <- 2
Nodes1 <- max(max(mEdges1$From_Node), max(mEdges1$To_Node))

SA_UD1 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges1,
  Layers = Layers,
  Nodes = Nodes1,
  isDirected = FALSE
)

mEdges2 <- read.xlsx(file2)
Layers <- 2
Nodes2 <- max(max(mEdges2$From_Node), max(mEdges2$To_Node))

SA_UD2 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges2,
  Layers = Layers,
  Nodes = Nodes2,
  isDirected = FALSE
)

mEdges3 <- read.xlsx(file3)
Layers <- 2
Nodes3 <- max(max(mEdges3$From_Node), max(mEdges3$To_Node))

SA_UD3 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges3,
  Layers = Layers,
  Nodes = Nodes3,
  isDirected = FALSE
)

mEdges4 <- read.xlsx(file4)
Layers <- 2
Nodes4 <- max(max(mEdges4$From_Node), max(mEdges4$To_Node))

SA_UD4 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges4,
  Layers = Layers,
  Nodes = Nodes4,
  isDirected = FALSE
)

mEdges5 <- read.xlsx(file5)
Layers <- 2
Nodes5 <- max(max(mEdges5$From_Node), max(mEdges5$To_Node))

SA_UD5 <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = mEdges5,
  Layers = Layers,
  Nodes = Nodes5,
  isDirected = FALSE
)

TM_UD1 <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA_UD1, Layers, Nodes1)
TM_UD2 <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA_UD2, Layers, Nodes2)
TM_UD3 <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA_UD3, Layers, Nodes3)
TM_UD4 <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA_UD4, Layers, Nodes4)
TM_UD5 <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA_UD5, Layers, Nodes5)

CEM1 <- GetCoverageEvolutionMultilayer(TM_UD1, Layers,Nodes1,1:5)
CEM2 <- GetCoverageEvolutionMultilayer(TM_UD2, Layers,Nodes2,1:5)
CEM3 <- GetCoverageEvolutionMultilayer(TM_UD3, Layers,Nodes3,1:5)
CEM4 <- GetCoverageEvolutionMultilayer(TM_UD4, Layers,Nodes4,1:5)
CEM5 <- GetCoverageEvolutionMultilayer(TM_UD5, Layers,Nodes5,1:5)

##########

### TESTING TO DISPLAY #########
# Function to display a matrix for a given file and results
displayMatrix <- function(file, results, matrix_name) {
  cat(paste(matrix_name, "for file:", file), "\n")
  cat("\n", matrix_name, ":\n")
  print(results[[matrix_name]])
  cat("\n---------------------------\n")
}

                                  ######################################
                                  ###### Supra Adjacency Plot ##########
                                  ######################################

# Function to extract and stack the nonzero values
#stack_nonzero_values <- function(matrix) {
 # subset_matrix <- matrix[1:216, 1:216]
  #subset_vector <- as.vector(subset_matrix[subset_matrix != 0])
  #data.frame(value = subset_vector)
#}

# Create a list of SA matrices
#SA_matrices <- list(SA1 = as.matrix(SA1), SA2 = as.matrix(SA2), SA3 = as.matrix(SA3), SA4 = as.matrix(SA4), SA5 = as.matrix(SA5))

# Stack the nonzero values for each SA matrix
#stacked_data <- bind_rows(lapply(SA_matrices, stack_nonzero_values), .id = "SA")

# Plot the histogram with different colors for each SA matrix
#ggplot(stacked_data, aes(x = value, fill = SA)) +
 # geom_histogram(binwidth = 1, color = "black") +
  #facet_wrap(~ SA, scales = "free") +
  #labs(title = "", x = "Source Node", y = "Target Node") +
  #theme_minimal() +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
   #     axis.text.y = element_text(angle = 0))


                                    ######################################
                                    ###### Aggregated Networks ##########
                                    ######################################

######@@@@@@AM1
# Define the AM matrices
AM_matrices <- list(AM1 = AM1, AM2 = AM2, AM3 = AM3, AM4 = AM4, AM5 = AM5)

# Get AM1
AM1 <- AM_matrices$AM1

# Convert AM1 to a regular matrix
AM1_matrix <- as.matrix(AM1)

# Create an empty dense matrix to store differing edges
differing_edges <- matrix(0, nrow = nrow(AM1_matrix), ncol = ncol(AM1_matrix))

# Optionally convert the dense matrix to a sparse matrix (if needed)
differing_edges_sparse <- as(differing_edges, "dgCMatrix")

# Initialize the differing_edges matrix with zeros of the same dimensions as AM1_matrix
differing_edges <- matrix(0, nrow = nrow(AM1_matrix), ncol = ncol(AM1_matrix))

for (i in 1:length(AM_matrices)) {
  if (i != 1) {
    AM_matrix <- as.matrix(AM_matrices[[i]])
    
    # Check if both matrices have the same dimensions
    if (identical(dim(AM1_matrix), dim(AM_matrix))) {
      differing_edges <- differing_edges + (AM1_matrix != AM_matrix)
    } else {
      warning(paste("Matrix", i, "has different dimensions and is skipped."))
      # Print the dimensions of the matrix causing the issue
      print(dim(AM_matrix))
    }
  }
}

# Convert the differing_edges matrix to a dense matrix
differing_edges <- as.matrix(differing_edges)

# Calculate the sum of differing edges per column (nodes)
differing_edges_sum_per_node <- colSums(differing_edges)

# Get the indices of nodes with differing edges
differing_nodes <- which(differing_edges_sum_per_node > 0)

# Create a graph object from AM1
g <- graph_from_adjacency_matrix(AM1_matrix, mode = "directed")

# Set node labels based on index
V(g)$label <- ifelse(as.numeric(V(g)) <= 75, "E", "G")

# Set node color for differing nodes, E, and G nodes
node_colors <- rep("blue", vcount(g))
node_colors[V(g) %in% differing_nodes] <- "yellow"
node_colors[V(g)$label == "E"] <- "red"

# Set edge color and width
edge_colors <- ifelse(differing_edges > 0, "red", "black")
edge_widths <- ifelse(differing_edges > 0, 2, 1)

# Plot the graph
plot(g,
     vertex.size = 5,
     vertex.label = V(g)$label,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.frame.color = "transparent",
     vertex.color = node_colors,
     edge.color = edge_colors,
     edge.width = edge_widths,
     edge.arrow.size = 0.1,
     edge.arrow.width = 5,
     main = "AM1",
     layout = layout_randomly(g)
)

# Create the legend
legend("topright",
       legend = c("E (Electricity)", "G (Gas)", "Differing Edges"),
       fill = c("red", "blue", "yellow"),
       cex = 0.6
)

###########@@@@@@ Final Code For All AM's Comparison with AM1 

# Function to compare and plot adjacency matrices
compareAndPlotAdjacencyMatrix <- function(AM1, AM, file_name) {
  # Extract edge weights from AM1 and the other AM
  AM1_matrix <- as.matrix(as_adjacency_matrix(AM1))
  AM1_weights <- E(AM1)$weight
  AM_matrix <- as.matrix(as_adjacency_matrix(AM))
  AM_weights <- E(AM)$weight
  
  # Compare AM1 and the other AM edge weights
  differing_edges <- (AM1_weights != AM_weights)
  
  # Create a graph object from AM1
  g <- graph_from_adjacency_matrix(AM1_matrix, mode = "directed")
  
  # Set node labels based on index
  V(g)$label <- ifelse(as.numeric(V(g)) <= 75, "E", "G")
  
  # Get the indices of gas and electricity nodes
  gas_nodes <- V(g)[V(g)$label == "G"]
  electricity_nodes <- V(g)[V(g)$label == "E"]
  
  # Set node colors for gas and electricity nodes
  node_colors <- rep("blue", vcount(g))
  node_colors[gas_nodes] <- "black"
  node_colors[electricity_nodes] <- "red"
  
  # Set edge colors based on differing edges
  edge_colors <- ifelse(differing_edges, "red", "green")
  edge_widths <- ifelse(differing_edges, 1, 2)
  
  # Plot the graph
  plot(g,
       vertex.size = 5,
       vertex.label = V(g)$label,
       vertex.label.cex = 0.5,
       vertex.label.color = "white",
       vertex.frame.color = "transparent",
       vertex.color = node_colors,
       edge.color = edge_colors,
       edge.width = edge_widths,
       edge.arrow.size = 0.1,
       edge.arrow.width = 2,
       main = paste("Comparison of AM1 and", file_name),
       layout = layout_randomly(g)
  )
  
  # Create the legend
  legend("topright",
         legend = c("Electricity Node", "Gas Node", "Same Edge Weight", "Different Edge Weight"),
         fill = c("red", "black", "green", "red"),
         cex = 0.5  # Adjust the size of the legend labels (smaller value for smaller text)
  )
}

AM_matrices <- list(AM1 = AM1, AM2 = AM2, AM3 = AM3, AM4 = AM4, AM5 = AM5)

# Compare and plot AM2 with AM1
compareAndPlotAdjacencyMatrix(AM1, AM2, "AM2")

# Compare and plot AM3 with AM1
compareAndPlotAdjacencyMatrix(AM1, AM3, "AM3")

# Compare and plot AM4 with AM1
compareAndPlotAdjacencyMatrix(AM1, AM4, "AM4")

# Compare and plot AM5 with AM1
compareAndPlotAdjacencyMatrix(AM1, AM5, "AM5")

###############

###### Final Code for all the possible AM's Pairs 

# Function to compare and plot adjacency matrices
compareAndPlotAdjacencyMatrix <- function(AMi, AMj, file_name) {
  # Extract edge weights from AMi and AMj
  AMi_matrix <- as.matrix(as_adjacency_matrix(AMi))
  AMi_weights <- E(AMi)$weight
  AMj_matrix <- as.matrix(as_adjacency_matrix(AMj))
  AMj_weights <- E(AMj)$weight
  
  # Compare AMi and AMj edge weights
  differing_edges <- (AMi_weights != AMj_weights)
  
  # Create a graph object from AMi
  g <- graph_from_adjacency_matrix(AMi_matrix, mode = "directed")
  
  # Set node labels based on index
  V(g)$label <- ifelse(as.numeric(V(g)) <= 75, "E", "G")
  
  # Get the indices of gas and electricity nodes
  gas_nodes <- V(g)[V(g)$label == "G"]
  electricity_nodes <- V(g)[V(g)$label == "E"]
  
  # Set node colors for gas and electricity nodes
  node_colors <- rep("blue", vcount(g))
  node_colors[gas_nodes] <- "black"
  node_colors[electricity_nodes] <- "red"
  
  # Set edge colors based on differing edges
  edge_colors <- ifelse(differing_edges, "red", "green")
  edge_widths <- ifelse(differing_edges, 1, 2)
  
  # Plot the graph
  plot(g,
       vertex.size = 5,
       vertex.label = V(g)$label,
       vertex.label.cex = 0.5,
       vertex.label.color = "white",
       vertex.frame.color = "transparent",
       vertex.color = node_colors,
       edge.color = edge_colors,
       edge.width = edge_widths,
       edge.arrow.size = 0.1,
       edge.arrow.width = 2,
       main = paste("Comparison of AM", file_name),
       layout = layout_randomly(g)
  )
  
  # Create the legend
  legend("topright",
         legend = c("Electricity Node", "Gas Node", "Same Edge Weight", "Different Edge Weight"),
         fill = c("red", "black", "green", "red"),
         cex = 0.5  # Adjust the size of the legend labels (smaller value for smaller text)
  )
}
#### make general statements like compareAndPlotAdjacencyMatrix(AMi, AMj, "AMi and AMj Comparison")

# Compare and plot AM1 with AM2
compareAndPlotAdjacencyMatrix(AM5, AM3, "AM5 and AM3 Comparison")

# Compare and plot AM2 with AM3
compareAndPlotAdjacencyMatrix(AM2, AM3, "AM2 and AM3 Comparison")

# Compare and plot AM4 with AM5
compareAndPlotAdjacencyMatrix(AM4, AM5, "AM4 and AM5 Comparison")


                  ###########################################
                  ###### Authority Centrality HITS ##########
                  ###########################################

AC_matrices <- list(AC1 = AC1, AC2 = AC2, AC3 = AC3, AC4 = AC4, AC5 = AC5)
AC1
AC2
sort(AC2, decreasing = TRUE)
centrality_values <- c(
  1.000000e+00, 3.725635e-01, 1.860523e-01, 7.072895e-02, 4.075535e-02,
  3.715190e-02, 2.088149e-02, 5.484153e-03, 3.210721e-03, 1.070286e-03,
  9.036262e-04, 6.761730e-04, 5.742947e-04, 1.306123e-04, 4.999582e-05,
  4.624275e-05, 1.255965e-05, 1.179541e-05, 5.633560e-06, 1.588760e-06
)

node_indices <- which(centrality_values %in% AC1 )
# Print the node indices
print(node_indices)

centrality_values <- AC1
indices <- which(centrality_values == 1.588760e-06)
indices

centrality_values <- AC2
indices <- which(centrality_values == 1.000000e+00)
indices

centrality_values <- AC3
indices <- which(centrality_values == 1.000000e+00)
indices

centrality_values <- AC4
indices <- which(centrality_values == 1.000000e+00)
indices

centrality_values <- AC5
indices <- which(centrality_values == 1.000000e+00)
indices

                      ######################################
                      ###### Closeness Centrality ##########
                      ######################################

MC_matrices <- list(MC1 = MC1, MC2 = MC2, MC3 = MC3, MC4 = MC4, MC5 = MC5)

# Convert the MC_matrices list to a list of vectors
MC_vectors <- lapply(MC_matrices, unlist)

# Plot histogram for each centrality measure
par(mfrow = c(1, length(MC_vectors)))  # Set up a grid for subplots

# Iterate over the MC_vectors list and plot histogram for each centrality measure
lapply(names(MC_vectors), function(name) {
  centrality_values <- MC_vectors[[name]]  # Convert to numeric
  hist(centrality_values, main = paste("Case", name), xlab = "Centrality Value")
})
###########
library(ggplot2)
library(dplyr)
library(gridExtra)

# Create a list to store the line plots
line_plots <- list()

# Iterate over the MC_matrices and create line plots
for (i in 1:length(MC_matrices)) {
  case_name <- paste0("MC", i)
  centrality_values <- unlist(MC_matrices[[i]])
  
  # Create a data frame with Nodes and Centrality_Values
  mc_data <- data.frame(Nodes = 1:length(centrality_values), Centrality_Values = centrality_values)
  
  # Sort the data frame by Nodes
  mc_data <- mc_data %>% arrange(Nodes)
  
  # Create the line plot
  line_plot <- ggplot(mc_data, aes(x = Nodes, y = Centrality_Values)) +
    geom_line(color = "black") +
    labs(title = paste("Line Plot for", case_name), x = "Nodes", y = "Centrality Values") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  line_plots[[i]] <- line_plot
}

# Arrange and display the line plots in a grid
grid.arrange(grobs = line_plots, ncol = 3)  # Change the number of columns as per your preference

####### Overleaf Pic Code
library(ggplot2)
library(dplyr)
library(gridExtra)

# Create a list to store the faceit plots
faceit_plots <- list()

# Iterate over the MC matrices and create faceit plots
for (i in 1:length(MC_matrices)) {
  case_name <- paste0("MC", i)
  centrality_values <- unlist(MC_matrices[[i]])
  
  # Create a data frame with Index and Values
  mc_data <- data.frame(Index = 1:length(centrality_values), Values = centrality_values)
  
  # Sort the data frame by Values in descending order
  mc_data <- mc_data %>% arrange(desc(Values))
  
  # Select the top 5 Electricity nodes and top 5 Gas nodes
  top_electricity <- mc_data %>% filter(Index <= 75) %>% head(5)
  top_gas <- mc_data %>% filter(Index > 75) %>% head(5)
  
  # Combine the top nodes
  top_nodes <- rbind(top_electricity, top_gas)
  
  # Create a new column for labels
  mc_data$Label <- ifelse(mc_data$Index %in% top_electricity$Index, paste0("E", mc_data$Index),
                          ifelse(mc_data$Index %in% top_gas$Index, paste0("G", mc_data$Index), ""))
  
  # Create a new column for colors based on condition
  mc_data$Color <- ifelse(mc_data$Index %in% top_electricity$Index, "E",
                          ifelse(mc_data$Index %in% top_gas$Index, "G", "Other"))
  
  # Assign colors to the remaining nodes based on their top five nodes
  for (j in 1:nrow(mc_data)) {
    if (mc_data$Color[j] == "Other") {
      index <- mc_data$Index[j]
      if (index <= 75) {
        mc_data$Color[j] <- mc_data$Color[mc_data$Index %in% top_electricity$Index][1]
      } else {
        mc_data$Color[j] <- mc_data$Color[mc_data$Index %in% top_gas$Index][1]
      }
    }
  }
  
  # Set the position of node labels
  label_position <- ifelse(mc_data$Index %in% top_nodes$Index, "right", "left")
  
  # Create the faceit plot
  faceit_plot <- ggplot(mc_data, aes(x = Index, y = Values, group = 1)) +
    geom_line(color = "black") +
    geom_point(aes(color = Color), size = 2) +
    geom_text(data = mc_data %>% filter(Index %in% top_nodes$Index),
              aes(label = Label, color = Color), position = position_nudge(x = ifelse(label_position == "right", 0.5, -0.5)), 
              vjust = -1, size = 3, show.legend = FALSE) +
    labs(title = paste("Closeness Centrality", case_name), x = "Nodes", y = "Values") +
    scale_color_manual(values = c("E" = "black", "G" = "red")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none")
  
  faceit_plots[[i]] <- faceit_plot
}

# Arrange and display the faceit plots in a grid
grid.arrange(grobs = faceit_plots, ncol = 2)  # Change the number of columns as per your preference

######

                                  ######################################
                                  ###### Multi Degree Sum     ##########
                                  ######################################
MD_matrices <- list(MD1 = MD1, MD2 = MD2, MD3 = MD3, MD4 = MD4, MD5 = MD5)

# Plot histogram for each MD matrix
par(mfrow = c(1, length(MD_matrices)))  # Set up a grid for subplots

for (i in 1:length(MD_matrices)) {
  centrality_values <- MD_matrices[[i]]  # Convert to numeric
  hist(centrality_values, main = paste("Histogram for MD", i), xlab = "Centrality Value")
}
###### Simple Rigid Line Graph##########
# Combine MD matrices into a single data frame
MD_combined <- data.frame(Nodes = 1:length(MD1))
for (i in 1:length(MD_matrices)) {
  MD_combined <- cbind(MD_combined, Centrality_Values = unlist(MD_matrices[[i]]))
  names(MD_combined)[i + 1] <- paste0("MD", i)
}

# Reshape data from wide to long format
MD_long <- tidyr::pivot_longer(MD_combined, -Nodes, names_to = "Case", values_to = "Centrality_Values")

# Plot line graph for each MD matrix
ggplot(MD_long, aes(x = Nodes, y = Centrality_Values, group = Case, color = Case)) +
  geom_line() +
  scale_color_viridis_d() +
  facet_wrap(~ Case, scales = "free") +
  ggtitle("Multi Degree Sum across Cases") +
  theme_minimal() +
  xlab("Nodes") +
  ylab("Centrality Values") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
#####
#library(cowplot)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Create a list to store the faceit plots
faceit_plots <- list()

# Iterate over the MD matrices and create faceit plots
for (i in 1:length(MD_matrices)) {
  case_name <- paste0("MD", i)
  centrality_values <- unlist(MD_matrices[[i]])
  
  # Create a data frame with Nodes and Centrality_Values
  md_data <- data.frame(Nodes = 1:length(centrality_values), Centrality_Values = centrality_values)
  
  # Sort the data frame by Centrality_Values in descending order
  md_data <- md_data %>% arrange(desc(Centrality_Values))
  
  # Select the top 5 rows
  top_nodes <- head(md_data, 5)$Nodes
  
  # Create a new column for labels
  md_data$Label <- ifelse(md_data$Nodes <= 75, paste0("E", md_data$Nodes), paste0("G", md_data$Nodes))
  
  # Create a new column for colors based on condition
  md_data$Color <- ifelse(md_data$Nodes <= 75, "E", "G")
  
  # Create the faceit plot
  faceit_plot <- ggplot(md_data, aes(x = Nodes, y = Centrality_Values, group = 1)) +
    geom_line(color = "black") +
    geom_point(aes(color = Color), size = 2) +
    geom_text(data = md_data %>% filter(Nodes %in% top_nodes),
              aes(label = Label, color = Color), vjust = -1, size = 2,  show.legend = FALSE) +
    labs(title = paste("Plot for", case_name), x = "Nodes", y = "MD Values") +
    scale_color_manual(values = c("E" = "black", "G" = "red"), labels = c("E" = "Electricity", "G" = "Gas")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  faceit_plots[[i]] <- faceit_plot
}

# Arrange and display the faceit plots in a grid
grid.arrange(grobs = faceit_plots, ncol = 3)  # Change the number of columns as per your preference

                                  ######################################
                                  ###### Eigenvector Centrality ########
                                  ######################################

# Create a data frame with all MEV matrices
MEV_df <- data.frame(Nodes = 1:length(MEV1), MEV1, MEV2, MEV3, MEV4, MEV5) %>%
  pivot_longer(cols = starts_with("MEV"), names_to = "Case", values_to = "Centrality_Values")

# Define labels for node and centrality values
labels <- MEV_df %>%
  group_by(Case) %>%
  mutate(Label = paste("Node:", Nodes, "\nCentrality:", format(Centrality_Values, digits = 4)))

# Define custom colors for faceit labels
label_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd") # You can customize the colors here

# Plot each case separately in a facet grid
ggplot(MEV_df, aes(x = Nodes, y = Centrality_Values, group = Case, color = Case)) +
  geom_line() +
  scale_color_manual(values = label_colors) + # Set custom colors for the faceit labels
  facet_wrap(~ Case, scales = "free") +
  ggtitle("Eigenvector Centrality across Cases") +
  xlab("Nodes") +
  ylab("Centrality Values") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside") +
  geom_text(data = labels, aes(label = Label), hjust = 0, vjust = 1, size = 2, nudge_x = 10, nudge_y = 0)

                                  ######################################
                                  ###### PageRankCentrality   ##########
                                  ######################################
#MPR <- GetMultiPageRankCentrality(SA, Layers, Nodes)
#MD_matrices <- list(MD1 = MD1, MD2 = MD2, MD3 = MD3, MD4 = MD4, MD5 = MD5)

MPR_matrices<-list(MPR1 = MPR1, MPR2 = MPR2, MPR3 = MPR3, MPR4 = MPR4, MPR5 = MPR5)
# Create a list to store the faceit plots
faceit_plots <- list()

# Iterate over the MC matrices and create faceit plots
for (i in 1:length(MPR_matrices)) {
  case_name <- paste0("MPR", i)
  centrality_values <- unlist(MPR_matrices[[i]])
  
  # Create a data frame with Index and Values
  mcr_data <- data.frame(Index = 1:length(centrality_values), Values = centrality_values)
  
  # Sort the data frame by Values in descending order
  mcr_data <- mcr_data %>% arrange(desc(Values))
  
  # Select the top 5 Electricity nodes and top 5 Gas nodes
  top_electricity <- mcr_data %>% filter(Index <= 75) %>% head(5)
  top_gas <- mcr_data %>% filter(Index > 75) %>% head(5)
  
  # Combine the top nodes
  top_nodes <- rbind(top_electricity, top_gas)
  
  # Create a new column for labels
  mcr_data$Label <- ifelse(mcr_data$Index %in% top_electricity$Index, paste0("E", mcr_data$Index),
                          ifelse(mcr_data$Index %in% top_gas$Index, paste0("G", mcr_data$Index), ""))
  
  # Create a new column for colors based on condition
  mcr_data$Color <- ifelse(mcr_data$Index %in% top_electricity$Index, "E",
                          ifelse(mcr_data$Index %in% top_gas$Index, "G", "Other"))
  
  # Assign colors to the remaining nodes based on their top five nodes
  for (j in 1:nrow(mcr_data)) {
    if (mcr_data$Color[j] == "Other") {
      index <- mcr_data$Index[j]
      if (index <= 75) {
        mcr_data$Color[j] <- mcr_data$Color[mcr_data$Index %in% top_electricity$Index][1]
      } else {
        mcr_data$Color[j] <- mcr_data$Color[mcr_data$Index %in% top_gas$Index][1]
      }
    }
  }
  
  # Set the position of node labels
  label_position <- ifelse(mcr_data$Index %in% top_nodes$Index, "right", "left")
  
  # Create the faceit plot
  faceit_plot <- ggplot(mcr_data, aes(x = Index, y = Values, group = 1)) +
    geom_line(color = "black") +
    geom_point(aes(color = Color), size = 2) +
    geom_text(data = mcr_data %>% filter(Index %in% top_nodes$Index),
              aes(label = Label, color = Color), position = position_nudge(x = ifelse(label_position == "right", 0.5, -0.5)), 
              vjust = -1, size = 3, show.legend = FALSE) +
    labs(title = paste("Page Rank Centrality", case_name), x = "Nodes", y = "Values") +
    scale_color_manual(values = c("E" = "black", "G" = "red")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none")
  
  faceit_plots[[i]] <- faceit_plot
}

# Arrange and display the faceit plots in a grid
grid.arrange(grobs = faceit_plots, ncol = 2)  # Change the number of columns as per your preference

####$$$$$$$

MPR_matrices <- list(MPR1 = MPR1, MPR2 = MPR2, MPR3 = MPR3, MPR4 = MPR4, MPR5 = MPR5)

# Create a list to store the faceit plots
faceit_plots <- list()

# Iterate over the MPR matrices and create faceit plots
for (i in 1:length(MPR_matrices)) {
  case_name <- paste0("MPR", i)
  centrality_values <- unlist(MPR_matrices[[i]])
  
  # Create a data frame with Index and Values
  mcr_data <- data.frame(Index = 1:length(centrality_values), Values = centrality_values)
  
  # Sort the data frame by Values in descending order
  mcr_data <- mcr_data %>% arrange(desc(Values))
  
  # Select the top 5 Electricity nodes
  top_electricity <- mcr_data %>% filter(Index <= 75) %>% head(5)
  
  # Select the top 5 Gas nodes
  top_gas <- mcr_data %>% filter(Index > 75) %>% head(5)
  
  # Combine the top nodes
  top_nodes <- rbind(top_electricity, top_gas)
  
  # Create a new column for labels
  top_nodes$Label <- ifelse(top_nodes$Index <= 75, paste0("E", top_nodes$Index), paste0("G", top_nodes$Index))
  
  # Create a new column for colors based on condition
  top_nodes$Color <- ifelse(top_nodes$Index <= 75, "red", "blue")
  
  # Create the faceit plot
  faceit_plot <- ggplot(top_nodes, aes(x = Index, y = Values)) +
    geom_line(color = "black") +
    geom_point(aes(color = Color), size = 2) +
    geom_text(aes(label = Label, color = Color), vjust = -1, size = 3, show.legend = FALSE) +
    labs(title = paste("Page Rank Centrality", case_name), x = "Nodes", y = "Values") +
    scale_color_manual(values = c("red", "blue"), guide = FALSE) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none")
  
  faceit_plots[[i]] <- faceit_plot
}

# Arrange and display the faceit plots in a grid
grid.arrange(grobs = faceit_plots, ncol = 2)  # Change the number of columns as per your preference

                                  ######################################
                                  ###### RWCentrality         ##########
      
#MRW <- GetMultiRWCentrality(SA, Layers, Nodes, Type = "classical", Method = "multilayer")

