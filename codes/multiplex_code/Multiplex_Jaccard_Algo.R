library(multinet)
library(openxlsx)
library(readxl)
library(muxViz)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(scales)

# Define a function to read and process an Excel file
read_and_process_excel <- function(file_path) {
  mEdges <- read.xlsx(file_path)
  Layers <- 2
  Nodes <- max(max(mEdges$From_Node), max(mEdges$To_Node))
  
  # Build the supra-adjacency matrix
  SA <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
    mEdges = mEdges,
    Layers = Layers,
    Nodes = Nodes,
    isDirected = TRUE
  )
  
  AG <- GetAggregateNetworkFromSupraAdjacencyMatrix(SA, Layers, Nodes)
  
  return(AG)
}

# List of file paths
file_paths <- c("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_1.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_2.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_3.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_4.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_5.xlsx")


# Create a list to store the AG results
AG_list <- list()

# Create a named list of AG objects using lapply
AG_list <- lapply(file_paths, read_and_process_excel)

# Use setNames to give each AG object a meaningful name
AG_list <- setNames(AG_list, paste0("AG", 1:length(AG_list)))

# Access AG1, AG2, AG3, AG4, AG5 using the list
AG1 <- AG_list[["AG1"]]
AG2 <- AG_list[["AG2"]]
AG3 <- AG_list[["AG3"]]
AG4 <- AG_list[["AG4"]]
AG5 <- AG_list[["AG5"]]

                                        #############################################
                                        ########## Multinet Code: UPDATED ###########
                                        #############################################
for (i in 1:5) {
  layer <- get(paste0("AG", i))
  V(layer)$name <- V(layer)
  assign(paste0("layer", i), layer)
}

mgraph <- ml_empty()
for (k in 1:5) {
  if(all(as.matrix(!get.adjacency(get(paste0('layer',k)))))==0){
    add_igraph_layer_ml(mgraph,get(paste0('layer',k)), paste0("layer",k))
  }
}
class(mgraph)
                                      ####################################################
                                      ################ Multinet Analysis #################
                                      ####################################################

layer_comparison_ml(mgraph, method ="jeffrey.degree")
layer_comparison_ml(mgraph, method = "pearson.degree")
layer_comparison_ml(mgraph, method = "jaccard.edges")

                                      ###################################################
                                      ################### Heatmap #######################
                                      ###################################################

jaccard_result <- layer_comparison_ml(mgraph, method = "jaccard.edges")

# Reshape the data
df_long <- as.data.frame(as.table(as.matrix(jaccard_result)))

# Rename columns for clarity
colnames(df_long) <- c("layer", "compared_layer", "value")

# Rename the levels
df_long$layer <- factor(df_long$layer, levels = c("layer1", "layer2", "layer3", "layer4", "layer5"), 
                        labels = c("Layer 1", "Layer 2", "Layer 3", "Layer 4", "Layer 5"))
df_long$compared_layer <- factor(df_long$compared_layer, levels = c("layer1", "layer2", "layer3", "layer4", "layer5"), 
                                 labels = c("Layer 1", "Layer 2", "Layer 3", "Layer 4", "Layer 5"))



# Enhanced heatmap plot with a single color gradient
ggplot(df_long, aes(x=layer, y=compared_layer, fill=value)) +
  geom_tile(color="white", linewidth=0.5) +
  scale_fill_gradient(low="white", high="orange") +  # Single color gradient
  theme_minimal() +
  labs(title="", x="Layer", y="Compared Layer", fill="Jaccard Index") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust=1, size=10), # Rotate and resize x axis text
    axis.text.y = element_text(angle = 45, hjust = 1, vjust=1, size=10),  # Resize y axis text
    plot.title = element_text(face="bold", size=16, hjust=0.5),  # Center the plot title and make it bold
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),   # Remove panel background
    axis.line = element_line(color="black")  # Add axis lines
  )
