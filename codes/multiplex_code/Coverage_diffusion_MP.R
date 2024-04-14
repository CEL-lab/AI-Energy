library(readxl)
library(muxViz)
library(igraph)
library(ggplot2)
library(openxlsx)
library(Matrix)
library(MASS)  
library(ggrepel)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(tictoc)
library(multinet)

# Define layer mapping
layer_mapping <- c('Layer_1' = 1, 'Layer_2' = 2, 'Layer_3' = 3, 'Layer_4' = 4, 'Layer_5' = 5)

#Read and combine new links
read_and_combine_new_links <- function(jaccard_file_path, adamic_adar_file_path, layer_mapping) {
  jaccard_links_df <- read.csv(jaccard_file_path, stringsAsFactors = FALSE)
  adamic_adar_links_df <- read.csv(adamic_adar_file_path, stringsAsFactors = FALSE)
  combined_df <- rbind(jaccard_links_df, adamic_adar_links_df)
  
  combined_links_list <- lapply(1:nrow(combined_df), function(idx) {
    list(
      Node_U = combined_df$Node_U[idx],
      Layer = layer_mapping[combined_df$Layer[idx]],
      Node_V = combined_df$Node_V[idx],
      weight = combined_df$weight[idx]
    )
  })
  return(combined_links_list)
}

#Add new links to network
add_links_to_network <- function(extended_edge_lists, new_links, layer_mapping) {
  for (link in new_links) {
    link_df <- data.frame(
      node.from = link$Node_U, 
      layer.from = layer_mapping[link$Layer], 
      node.to = link$Node_V, 
      layer.to = layer_mapping[link$Layer], 
      weight = link$weight
    )
    extended_edge_lists[[layer_mapping[link$Layer]]] <- rbind(extended_edge_lists[[layer_mapping[link$Layer]]], link_df)
  }
  return(extended_edge_lists)
}

#Analyse network
process_network <- function(extended_edge_lists, title_suffix, num_layers) {
  combined_extended_edge_list <- do.call(rbind, extended_edge_lists)
  #Layers <- length(original_file_paths)
  Layers <- num_layers
  Nodes  <- max(max(combined_extended_edge_list$node.from), max(combined_extended_edge_list$node.to))
  
  edges <- transform(
    combined_extended_edge_list,
    from = node.from + Nodes * (layer.from - 1),
    to = node.to + Nodes * (layer.to - 1)
  )
  
  SupraA <- sparseMatrix(
    i = edges$from,
    j = edges$to,
    x = edges$weight,
    dims = c(Nodes * Layers, Nodes * Layers)
  )
  
  SupraT <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SupraA, Layers, Nodes, Type="classical")
  TAUS <- 10^seq(-1,3,0.05)
  DisconnectedNodes <- igraph::clusters(igraph::graph.adjacency(SupraA))$no
  cov.mul.appr <- GetCoverageEvolutionMultilayer(SupraT, Layers, Nodes, TAUS, Approximate = TRUE, Approximate.disconnected = DisconnectedNodes)
  
  cov_data <- transform(cov.mul.appr, Coverage = Re(rho), Network = title_suffix)
  return(cov_data)
} 

# Function to generate a combined coverage plot of original and a specific stage
generate_coverage_plot <- function(cov_data, plot_title) {
  ggplot(cov_data, aes(x = tau, y = Coverage)) +
    geom_line() +
    scale_x_log10() +
    theme_minimal() +
    labs(
      title = plot_title,
      x = "Diffusion Time (tau)",
      y = "Coverage"
    )
}

# Read and process the original data
original_file_paths <- c(
  "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Updated Data/Case_1.xlsx",
  "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Updated Data/Case_2.xlsx",
  "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Updated Data/Case_3.xlsx",
  "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Updated Data/Case_4.xlsx",
  "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Updated Data/Case_5.xlsx"
)

extended_edge_lists_original <- list()
for (i in 1:length(original_file_paths)) {
  network_data <- read_excel(original_file_paths[i])
  colnames(network_data) <- c("node.from", "layer.from", "node.to", "layer.to", "weight")
  extended_edge_lists_original[[i]] <- network_data
}

# Analyze and visualize the original network
num_layers <- length(original_file_paths)
original_cov_data <- process_network(extended_edge_lists_original, "Original", num_layers)
original_cov_plot <- generate_coverage_plot(original_cov_data, "Multiplex Network Coverage")
print(original_cov_plot)
