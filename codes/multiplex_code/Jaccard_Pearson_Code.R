# Load required libraries
library(multinet)
library(openxlsx)
library(readxl)
library(muxViz)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(scales)
library(igraph)
# Function to read and process an Excel file
read_and_process_excel <- function(file_path) {
  mEdges <- tryCatch({
    read.xlsx(file_path)
  }, error = function(e) {
    message("Error reading file:", file_path, ":", e$message)
    return(NULL)
  })
  
  if (is.null(mEdges)) return(NULL)
  
  numeric_cols <- c("From_Node", "To_Node", "From_Layer", "To_Layer", "Length", "Flow")
  mEdges[numeric_cols] <- lapply(mEdges[numeric_cols], as.numeric)
  
  Layers <- 2
  Nodes <- max(max(mEdges$From_Node, na.rm = TRUE), max(mEdges$To_Node, na.rm = TRUE))
  
  SA <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
    mEdges = mEdges,
    Layers = Layers,
    Nodes = Nodes,
    isDirected = TRUE
  )
  AG <- GetAggregateNetworkFromSupraAdjacencyMatrix(SA, Layers, Nodes)
  V(AG)$name <- as.character(V(AG))
  return(AG)
}

# File paths
file_paths <- c("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_1.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_2.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_3.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_4.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_5.xlsx")


# Initialize AG_list and construct the multilayer graph
AG_list <- list()
mgraph <- ml_empty()
for (file_path in file_paths) {
  AG <- read_and_process_excel(file_path)
  if (!is.null(AG)) {
    layer_name <- basename(file_path)
    add_igraph_layer_ml(mgraph, AG, layer_name)
    AG_list[[layer_name]] <- AG
  }
}

# Calculate Jaccard similarity
jaccard_matrix <- layer_comparison_ml(mgraph, method = "jaccard.edges")


# Convert each igraph object in AG_list to an adjacency matrix
adj_matrix_list <- lapply(AG_list, function(AG) {
  as_adjacency_matrix(AG, sparse = TRUE, attr = "weight")
})
# Construct supra-adjacency matrix using bdiag for Pearson assortativity
supra_adj_matrix_flow <- do.call(bdiag, adj_matrix_list)

# Calculate Pearson assortativity
Layers <- length(adj_matrix_list)
Nodes <- sum(sapply(adj_matrix_list, nrow)) / Layers
IAS_flow <- GetInterAssortativityTensor(supra_adj_matrix_flow, Layers, Nodes, isDirected = TRUE, Type = "OO")
IAS_flow
jaccard_matrix
# Function to process the matrix and return a long format dataframe for heatmap plotting
process_matrix <- function(mat, use_layer_names = FALSE) {
  df <- as.data.frame(mat)
  if (use_layer_names) {
    # Use "Layer 1", "Layer 2", etc. as names
    layer_names <- paste0("Layer ", 1:nrow(df))
    colnames(df) <- layer_names
    rownames(df) <- layer_names
  } else {
    colnames(df) <- gsub("^V", "Layer", colnames(df))
    rownames(df) <- gsub("^V", "Layer", rownames(df))
  }
  df_long <- df %>% rownames_to_column(var = "layer") %>% pivot_longer(cols = -layer, names_to = "compared_layer", values_to = "value")
  return(df_long)
}

# Plotting function for heatmaps with customizable color schemes
plot_enhanced_heatmap <- function(df, low_color, high_color) {
  ggplot(df, aes(x = layer, y = compared_layer, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient(low = low_color, high = high_color) +
    labs(x = "Layer", y = "Compared Layer", fill = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12, face = "bold"),
          axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1, size = 12, face = "bold"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(color = "black"))
}

# Plot and display the heatmaps for Jaccard and Pearson
df_jaccard <- process_matrix(jaccard_matrix, use_layer_names = TRUE)
df_pearson <- process_matrix(as.matrix(IAS_flow$InterPearson), use_layer_names = TRUE)

heatmap_jaccard <- plot_enhanced_heatmap(df_jaccard, "#FFEDA0", "#FC4E2A")
heatmap_pearson <- plot_enhanced_heatmap(df_pearson, "#B3CDE3", "#011F4B")

grid.arrange(heatmap_jaccard, heatmap_pearson, ncol = 2)

# community detection
# Assuming mgraph is your multilayer graph object

# Aggregate the networks from all layers into one graph
combined_graph <- igraph::disjoint_union(lapply(AG_list, as.undirected))

# Apply the Louvain algorithm on the aggregated network
combined_comms <- igraph::cluster_louvain(combined_graph)

# combined_comms now contains the community structure of the entire network
# Print the community structure
print(combined_comms)

# For more detailed output: 
# Print the membership of each node in the community
membership(combined_comms)

# Optionally, you can also print the size of each community
cat("Community sizes:\n")
print(table(membership(combined_comms)))


