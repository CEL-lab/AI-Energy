library(muxViz)
library(openxlsx)
library(igraph)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(hrbrthemes)
library(scales)

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
file1 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/1.edgelist_default.xlsx"
file2 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/2.HORTA Electric Demand OFF.xlsx"
file3 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/3.NO166,No167 Gas Valve OFF.xlsx"
file4 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/4.HORTA, NO166NO167 OFF.xlsx"
file5 <- "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/5.CHAMPION, NO45NO52 OFF.xlsx"

# Calculate variables for each file
results <- list(
  results1 <- calculateVariables(file1),
  results2 <- calculateVariables(file2),
  results3 <- calculateVariables(file3),
  results4 <- calculateVariables(file4),
  results5 <- calculateVariables(file5)
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
}

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
                                    ###### Aggregate Network ##########
                                    ######################################

library(RColorBrewer)

# Convert AM1 into an adjacency matrix format
adj_matrix <- as.matrix(AM1)

# Create a graph object from the adjacency matrix
g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")

# Get the node indices
node_indices <- as.numeric(V(g))

# Update vertex labels based on condition
V(g)$label <- ifelse(node_indices <= 75, "E", "G")

# Set the colors for the vertices based on the labels
vertex_colors <- ifelse(node_indices <= 75, "red", "orange")

# plot
par(bg = "grey13", mar = c(0, 0, 0, 0))

# Plot the graph with updated colors and labels
plot(g, 
     vertex.size = 12,
     vertex.color = vertex_colors, 
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.frame.color = "transparent",
     main = "AM1 Network Graph",
     edge.width = 1,
     edge.arrow.size = 0.3,
     edge.arrow.width = 0.5,
     layout = layout_with_fr(g)
)
# Create custom legend
legend("topright",
       legend = c("Electricity", "Gas"),
       col = c("red", "orange"),
       bty = "n",
       pch = 20,
       pt.cex = 3,
       cex = 0.8,  # Adjust font size
       text.col = c("red", "orange"),
       horiz = FALSE,
       inset = c(0.05, 0.05)  # Adjust the distance from the plot
)
#############
# Define the AM matrices
AM_matrices <- list(AM1 = AM1, AM2 = AM2, AM3 = AM3, AM4 = AM4, AM5 = AM5)

# Set up the plotting area with multiple panels
par(mfrow = c(1, 5), mar = c(1, 1, 1, 0), oma = c(0, 3, 0, 0))

# Loop through each AM matrix
for (i in 1:length(AM_matrices)) {
  # Get the current AM matrix
  AM <- as.matrix(AM_matrices[[i]])
  
  # Create a graph object from the AM matrix
  g <- graph_from_adjacency_matrix(AM, mode = "directed")
  
  # Update vertex labels based on condition
  V(g)$label <- ifelse(as.numeric(V(g)) <= 75, "E", "G")
  
  # Set the colors for the vertices based on the labels
  vertex_colors <- ifelse(V(g)$label == "E", "red", "orange")
  
  # Plot the graph with Fruchterman-Reingold layout and vertex colors
  plot(g, 
       vertex.size = 15,
       vertex.color = vertex_colors,
       vertex.label.cex = 0.5,
       vertex.label.color = "white",
       vertex.frame.color = "transparent",
       edge.width = 1,
       edge.arrow.size = 0.03,
       edge.arrow.width = 0.5,
       main = paste("AM", i),
       #layout = layout_with_fr(g)
       layout = layout_randomly(g)
  )
  
  # Add the legend
  legend("topright",
         legend = c("Electricity", "Gas"),
         col = c("red", "orange"),
         bty = "n",
         pch = 20,
         pt.cex = 3,
         cex = 0.8,  # Adjust font size
         text.col = c("red", "orange"),
         horiz = FALSE,
         inset = c(0.05, 0.05)  # Adjust the distance from the plot
  )
}

# Reset the plotting area
par(mfrow = c(1, 1))
####
library(gridExtra)

# Define the AM matrices
AM_matrices <- list(AM1 = AM1, AM2 = AM2, AM3 = AM3, AM4 = AM4, AM5 = AM5)

# Initialize the faceit plots list
faceit_plots <- list()

# Loop through each AM matrix
for (i in 1:length(AM_matrices)) {
  # Get the current AM matrix
  AM <- as.matrix(AM_matrices[[i]])
  
  # Create a graph object from the AM matrix
  g <- graph_from_adjacency_matrix(AM, mode = "directed")
  
  # Update vertex labels based on condition
  V(g)$label <- ifelse(as.numeric(V(g)) <= 75, "E", "G")
  
  # Set the colors for the vertices based on the labels
  vertex_colors <- ifelse(V(g)$label == "E", "red", "orange")
  
  # Plot the graph with Fruchterman-Reingold layout and vertex colors
  plot <- plot(g, 
               vertex.size = 15,
               vertex.color = vertex_colors,
               vertex.label.cex = 0.5,
               vertex.label.color = "white",
               vertex.frame.color = "transparent",
               edge.width = 1,
               edge.arrow.size = 0.03,
               edge.arrow.width = 0.5,
               main = paste("AM", i),
               layout = layout_randomly(g)
  )
  
  # Add the plot to the list
  faceit_plots[[i]] <- plot
}

# Create the shared legend
legend <- legendGrob("topright",
                     legend = c("Electricity", "Gas"),
                     col = c("red", "orange"),
                     pch = 20,
                     pt.cex = 3,
                     cex = 0.8,  # Adjust font size
                     text.col = c("red", "orange"),
                     horiz = FALSE,
                     inset = c(0.05, 0.05)  # Adjust the distance from the plot
)

# Arrange the faceit plots in a grid
faceit_grid <- do.call(grid.arrange, c(faceit_plots, ncol = 2))

# Combine the faceit grid and the legend
grid_combined <- grid.arrange(faceit_grid, legend, ncol = 2, widths = c(4, 1))

# Display the combined grid
grid_combined

######@@@@@@AM1
library(igraph)

# Get AM1
AM1 <- AM_matrices$AM1

# Convert AM1 to a regular matrix
AM1_matrix <- as.matrix(AM1)

# Create an empty adjacency matrix to store differing edges
differing_edges <- matrix(0, nrow = dim(AM1)[1], ncol = dim(AM1)[2])

# Compare AM1 with other AM matrices
for (i in 1:length(AM_matrices)) {
  if (i != 1) {
    # Convert the current AM matrix to a regular matrix
    AM_matrix <- as.matrix(AM_matrices[[i]])
    
    # Find differing edges
    differing_edges <- differing_edges + (AM1_matrix != AM_matrix)
  }
}

# Create a graph object from AM1
g <- graph_from_adjacency_matrix(AM1_matrix, mode = "directed")

# Get the nodes with differing edges
differing_nodes <- V(g)[rowSums(differing_edges) > 0]

# Set node labels based on index
V(g)$label <- ifelse(as.numeric(V(g)) <= 75, "E", "G")

# Set node color for differing nodes
node_colors <- rep("blue", vcount(g))
node_colors[V(g) %in% differing_nodes] <- "yellow"

# Set node color for E and G nodes
node_colors[V(g)$label == "E"] <- "red"

# Set edge color and width
edge_colors <- ifelse(differing_edges > 0, "red", "black")
edge_widths <- ifelse(differing_edges > 0, 2, 1)

# Plot the graph
plot(g,
     vertex.size = 05,
     vertex.label = V(g)$label,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.frame.color = "transparent",
     vertex.color = node_colors,  # Set node color
     edge.color = edge_colors,
     edge.width = edge_widths,
     edge.arrow.size = 0.1,
     edge.arrow.width = 5,
     main = "AM1",
     #layout = layout_with_fr(g)
     layout = layout_randomly(g)
)

# Create the legend
legend("topright",
       legend = c("E (Electricity)", "G (Gas)", "Differing Nodes"),
       fill = c("red", "blue", "yellow")
      # border = NA,
       #bg = "white"
)


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

