# Load necessary libraries
library(muxViz)
library(igraph)
library(Matrix)
library(readxl)
library(ggrepel)
library(gridExtra)
library(ggplot2)
library(tidyverse)

# Function to process each file and return its adjacency matrix
process_file <- function(file_path, attr_name, overall_unique_nodes) {
  df <- read_excel(file_path)
  df$From_Node <- trimws(df$From_Node)
  df$To_Node <- trimws(df$To_Node)
  agg_formula <- as.formula(paste(attr_name, "~ From_Node + To_Node"))
  aggregated_edges <- aggregate(agg_formula, data=df, sum)
  g <- graph_from_data_frame(aggregated_edges, directed=TRUE, vertices=overall_unique_nodes)
  return(as_adjacency_matrix(g, type="both", attr=attr_name))
}

# Function to extract unique nodes from a file
extract_unique_nodes <- function(file_path) {
  df <- read_excel(file_path)
  unique_nodes <- unique(c(trimws(df$From_Node), trimws(df$To_Node)))
  return(unique_nodes)
}
# Define file paths
file_paths <- c("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_1.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_2.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_3.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_4.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_5.xlsx")

# Extract unique nodes from each file
unique_nodes_list <- lapply(file_paths, extract_unique_nodes)

# Combine unique nodes from all files to get overall unique nodes
overall_unique_nodes <- unique(unlist(unique_nodes_list))

# Process each file for Length and Flow
adj_matrices_length <- lapply(file_paths, process_file, attr_name="Length", overall_unique_nodes=overall_unique_nodes)
adj_matrices_flow <- lapply(file_paths, process_file, attr_name="Flow", overall_unique_nodes=overall_unique_nodes)

# Construct the supra-adjacency matrices
supra_adj_matrix_length <- do.call(bdiag, adj_matrices_length)
supra_adj_matrix_flow <- do.call(bdiag, adj_matrices_flow)

# Assuming Layers and Nodes are defined
Layers <- 5 # Example: 5 layers
# Nodes needs to be calculated based on your network's structure
Nodes <- nrow(supra_adj_matrix_flow) / Layers

# Flow-Based Centrality Calculations
#AM_flow <- GetAggregateNetworkFromSupraAdjacencyMatrix(supra_adj_matrix_flow, Layers, Nodes)
#IAS_flow <- GetInterAssortativityTensor(supra_adj_matrix_flow, Layers, Nodes, isDirected = TRUE, Type = "OO")
#AC_flow  <- GetMultiAuthCentrality(supra_adj_matrix_flow, Layers, Nodes)
MD_flow  <- GetMultiDegreeSum(supra_adj_matrix_flow, Layers, Nodes, isDirected = TRUE)
MEV_flow <- GetMultiEigenvectorCentrality(supra_adj_matrix_flow, Layers, Nodes)
#MKZ_flow <- GetMultiKatzCentrality(supra_adj_matrix_flow, Layers, Nodes)
MPR_flow <- GetMultiPageRankCentrality(supra_adj_matrix_flow, Layers, Nodes)
MRW_flow <- GetMultiRWCentrality(supra_adj_matrix_flow, Layers, Nodes, Type = "classical", Method = "multilayer")

# Length-Based Centrality Calculations
MC_length  <- GetMultiClosenessCentrality(supra_adj_matrix_length, Layers, Nodes)
#MH_length  <- GetMultiHubCentrality(supra_adj_matrix_length, Layers, Nodes)
#MKC_length <- GetMultiKCoreCentrality(supra_adj_matrix_length, Layers, Nodes)
#MD_length  <- GetMultiDegreeSum(supra_adj_matrix_length, Layers, Nodes, isDirected = TRUE)

#layer extraction 
calculate_layer <- function(centrality_scores, Layers) {
  nodes_per_layer <- length(centrality_scores) / Layers
  centrality_df <- data.frame(
    node_id = 1:length(centrality_scores),
    centrality_score = centrality_scores,
    layer = ceiling((1:length(centrality_scores)) / nodes_per_layer)
  )
  centrality_df <- centrality_df %>% arrange(desc(centrality_score))
  return(centrality_df)
}

# Example application for the dataframes and layers information
MD_flow_df <- calculate_layer(MD_flow, Layers)
MEV_flow_df <- calculate_layer(MEV_flow, Layers)
MPR_flow_df <- calculate_layer(MPR_flow, Layers)
MRW_flow_df <- calculate_layer(MRW_flow, Layers)
# For MC_length, extract the closeness scores first
closeness_scores <- MC_length$closeness
# Then calculate layer information with the general approach
MC_length_df <- calculate_layer(closeness_scores, Layers)

# Plotting
# Adjusted plotting function to include layer information in node labels
plot_centrality <- function(centrality_df, title, num_top_electricity = 5, num_top_gas = 5) {
  
  # Assuming centrality_df already includes node_id, centrality_score, and layer
  # Extract top nodes based on their type and requested number
  top_electricity <- centrality_df %>% filter(node_id <= 75) %>% head(num_top_electricity)
  top_gas <- centrality_df %>% filter(node_id > 75) %>% head(num_top_gas)
  
  # Combine the top nodes
  top_nodes_df <- rbind(top_electricity, top_gas)
  
  # Update labels to include the layer, maintaining the original E/G prefix
  top_nodes_df$Label <- ifelse(top_nodes_df$node_id <= 75, 
                               paste0("E", top_nodes_df$node_id, "L", top_nodes_df$layer),
                               paste0("G", top_nodes_df$node_id, "L", top_nodes_df$layer))
  
  # Keep the original logic for Shape and color distinction
  top_nodes_df$Shape <- ifelse(top_nodes_df$node_id <= 75, "E", "G")
  
  # Plot using original aesthetics but with updated labels
  plot <- ggplot(top_nodes_df, aes(x = node_id, y = centrality_score)) +
    geom_line(aes(group = 1), color = "black") +
    geom_point(aes(shape = Shape, color = Shape), size = 2) +
    geom_text_repel(aes(label = Label), 
                    box.padding = 0.5, point.padding = 0.5, segment.color = "transparent") +
    labs(title = title, x = "Node ID", y = "Centrality Score", color = "Node Type", shape = "Node Type") +
    scale_color_manual(name = "Node Type",
                       values = c("E" = "red", "G" = "orange"),
                       labels = c("Electricity", "Gas")) +
    scale_shape_manual(name = "Node Type",
                       values = c("E" = 16, "G" = 15),
                       labels = c("Electricity", "Gas")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "right")
  
  return(plot)
}

# Adjusted wrapper function to calculate layer and plot centrality
calculate_and_plot_centrality <- function(centrality_scores_or_list, Layers, title, score_extract = NULL, num_top_electricity = 5, num_top_gas = 5) {
  # Check if we need to extract scores from a list
  if (!is.null(score_extract)) {
    centrality_scores <- centrality_scores_or_list[[score_extract]]
  } else {
    centrality_scores <- centrality_scores_or_list
  }
  
  # Calculate layers
  centrality_df <- calculate_layer(centrality_scores, Layers)
  
  # Plot centrality using the plot_centrality function
  plot <- plot_centrality(centrality_df, title, num_top_electricity, num_top_gas)
  
  # Return both the dataframe and the plot
  list(centrality_df = centrality_df, plot = plot)
}
################## Overleaf Centrality Plotting ##################

# Closeness
MC_length_result <- calculate_and_plot_centrality(MC_length, Layers, "", score_extract = "closeness")
print(MC_length_result$plot)
ggsave("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Journal Paper/Over leaf /MP/Centralities/MP_Closseness_N.pdf", width = 7, height = 7, units = "in")

# Degree
MD_flow_result <- calculate_and_plot_centrality(MD_flow, Layers, "")
print(MD_flow_result$plot)
ggsave("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Journal Paper/Over leaf /MP/Centralities/MP_Degree_N.pdf", width = 7, height = 7, units = "in")

# Eigon Vector
MEV_flow_result <- calculate_and_plot_centrality(MEV_flow, Layers, "")
print(MEV_flow_result$plot)
ggsave("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Journal Paper/Over leaf /MP/Centralities/MP_Eigon_N.pdf", width = 7, height = 7, units = "in")

# Page Rank
MPR_flow_result <- calculate_and_plot_centrality(MPR_flow, Layers, "")
print(MPR_flow_result$plot)
ggsave("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Journal Paper/Over leaf /MP/Centralities/MP_pagerank_N.pdf", width = 7, height = 7, units = "in")

# Random Walk
MRW_flow_result <- calculate_and_plot_centrality(MRW_flow, Layers, "")
print(MRW_flow_result$plot)
ggsave("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Journal Paper/Over leaf /MP/Centralities/MP_RW_N.pdf", width = 7, height = 7, units = "in")


# layer summaries

# Function to calculate average centrality for each layer
average_centrality_by_layer <- function(centrality_df, centrality_name) {
  centrality_df %>%
    group_by(layer) %>%
    summarise(average_centrality = mean(centrality_score, na.rm = TRUE)) %>%
    mutate(centrality_type = centrality_name)
}

# Combine the average centralities into a single dataframe
average_centrality_df <- bind_rows(
  average_centrality_by_layer(MD_flow_result$centrality_df, "Degree"),
  average_centrality_by_layer(MC_length_result$centrality_df, "Closeness"),
  average_centrality_by_layer(MEV_flow_result$centrality_df, "Eigenvector"),
  average_centrality_by_layer(MPR_flow_result$centrality_df, "PageRank"),
  average_centrality_by_layer(MRW_flow_result$centrality_df, "Random Walk")
)

# Normalize centrality scores
average_centrality_df$normalized_centrality <- ave(average_centrality_df$average_centrality, average_centrality_df$centrality_type, FUN = function(x) (x - min(x)) / (max(x) - min(x)))

# Assuming average_centrality_df is already prepared and contains 'centrality_type', 'layer', and 'average_centrality'

ggplot(average_centrality_df, aes(x = centrality_type, y = average_centrality, fill = as.factor(layer))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_fill_brewer(palette = "Set2", name = "Layer") +
  labs(title = "Centrality Measures Across Layers",
       x = "Centrality Measure",
       y = "Average Centrality Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(),  # Corrected here
        axis.text.x = element_text(angle = 45, hjust = 1))

#######
library(dplyr)

# Function to analyze and summarize the most influential layer for each centrality measure
summarize_influential_layers <- function(centrality_data, layers, centrality_names) {
  summaries <- vector("list", length(centrality_data))
  names(summaries) <- centrality_names
  
  for (i in seq_along(centrality_data)) {
    centrality_df <- centrality_data[[i]]
    
    # Calculate layer information, assuming centrality_df includes node_id, centrality_score, and layer
    centrality_df <- centrality_df %>%
      mutate(layer = ceiling(node_id / (n()/layers))) %>%
      arrange(desc(centrality_score))
    
    # Identify the layer with the highest number of top central nodes
    top_layer <- centrality_df %>%
      mutate(top_node = if_else(rank(-centrality_score) <= (n()/layers), TRUE, FALSE)) %>%
      group_by(layer) %>%
      summarise(num_top_nodes = sum(top_node)) %>%
      arrange(desc(num_top_nodes)) %>%
      slice(1) %>%
      pull(layer)
    
    # Create summary text
    summaries[[i]] <- paste("In the context of", centrality_names[i], "centrality, Layer", top_layer,
                            "is very important because it contains the highest number of top central nodes.")
  }
  
  # Return all summaries as a single text
  return(paste(unlist(summaries), collapse = "\n"))
}

# Example usage
# Assuming you have data frames for each centrality measure already calculated
centrality_data <- list(MD_flow_df, MEV_flow_df, MPR_flow_df, MRW_flow_df) # Add your actual data frames
centrality_names <- c("Degree", "Eigenvector", "PageRank", "Random Walk")
Layers <- 5 # Number of layers in your network

# Get summaries for each centrality
centrality_summaries <- summarize_influential_layers(centrality_data, Layers, centrality_names)
print(centrality_summaries)




