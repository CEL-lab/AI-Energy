library(multinet)
library(openxlsx)
library(readxl)
library(muxViz)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(scales)

# Function to read and process an Excel file
read_and_process_excel <- function(file_path) {
  mEdges <- tryCatch({
    read.xlsx(file_path)
  }, error = function(e) {
    message("Error reading file:", file_path, ":", e$message)
    return(NULL)
  })
  
  if (is.null(mEdges)) return(NULL)
  
  # Ensure all relevant columns are numeric
  numeric_cols <- c("From_Node", "To_Node", "From_Layer", "To_Layer", "Length", "Flow")
  mEdges[numeric_cols] <- lapply(mEdges[numeric_cols], as.numeric)
  
  print(paste("Read file:", file_path))
  print(head(mEdges))
  
  Layers <- 2
  Nodes <- max(max(mEdges$From_Node, na.rm = TRUE), max(mEdges$To_Node, na.rm = TRUE))
  
  tryCatch({
    SA <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
      mEdges = mEdges,
      Layers = Layers,
      Nodes = Nodes,
      isDirected = TRUE
    )
    AG <- GetAggregateNetworkFromSupraAdjacencyMatrix(SA, Layers, Nodes)
    return(AG)
  }, error = function(e) {
    message("Error in processing file:", file_path, ":", e$message)
    return(NULL)
  })
}

# File paths
file_paths <- c("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_1.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_2.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_3.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_4.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_5.xlsx")

# Process each file
AG_list <- vector("list", length(file_paths))
names(AG_list) <- paste0("AG", seq_along(file_paths))

for (i in seq_along(file_paths)) {
  AG_list[[i]] <- read_and_process_excel(file_paths[i])
}

# Constructing the multilayer graph
mgraph <- ml_empty()
for (i in seq_along(AG_list)) {
  if (!is.null(AG_list[[i]])) {
    layer_name <- paste0("layer", i)
    
    # Ensure that each igraph object has a 'name' vertex attribute
    if(is.null(V(AG_list[[i]])$name) || length(V(AG_list[[i]])$name) == 0) {
      V(AG_list[[i]])$name <- as.character(1:vcount(AG_list[[i]]))
    }
    
    add_igraph_layer_ml(mgraph, AG_list[[i]], layer_name)
  }
}
class(mgraph)
layer_comparison_ml(mgraph, method = "jaccard.edges")
