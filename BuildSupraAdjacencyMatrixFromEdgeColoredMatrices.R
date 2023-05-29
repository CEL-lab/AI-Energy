library("muxViz")
library(igraph)
library(Matrix)
library(readxl)
# Read the data from the Excel sheets
powergrid_data <- read_excel("D:/NDSU/PhD Work/Research/IME Research multinet_course/Belgium Updated.xlsx", sheet = "PowerGrid")
gasnetwork_data <- read_excel("D:/NDSU/PhD Work/Research/IME Research multinet_course/Belgium Updated.xlsx", sheet = "GasNetwork")
hub_data <- read_excel("D:/NDSU/PhD Work/Research/IME Research multinet_course/Belgium Updated.xlsx", sheet = "HUB")


# Check for missing or incorrect values in PowerGrid data
powergrid_valid <- complete.cases(powergrid_data$From_Node, powergrid_data$To_Node)
powergrid_cleaned <- powergrid_data[powergrid_valid, ]

# Check for missing or incorrect values in GasNetwork data
gasnetwork_valid <- complete.cases(gasnetwork_data$From_Node, gasnetwork_data$To_Node)
gasnetwork_cleaned <- gasnetwork_data[gasnetwork_valid, ]

# Check for missing or incorrect values in HUB data
hub_valid <- complete.cases(hub_data$From_Node, hub_data$To_Node)
hub_cleaned <- hub_data[hub_valid, ]

max_node <- max(
  max(na.omit(powergrid_data$From_Node)),
  max(na.omit(powergrid_data$To_Node)),
  max(na.omit(gasnetwork_data$From_Node)),
  max(na.omit(gasnetwork_data$To_Node)),
  max(na.omit(hub_data$From_Node)),
  max(na.omit(hub_data$To_Node))
)

max_node

# Convert cleaned adjacency matrices to sparse matrix format
powergrid_sparse <- sparseMatrix(i = as.integer(powergrid_cleaned$From_Node),
                                 j = as.integer(powergrid_cleaned$To_Node),
                                 x = 1,
                                 dims = c(max_node, max_node))

gasnetwork_sparse <- sparseMatrix(i = as.integer(gasnetwork_cleaned$From_Node),
                                  j = as.integer(gasnetwork_cleaned$To_Node),
                                  x = 1,
                                  dims = c(max_node, max_node))

hub_sparse <- sparseMatrix(i = as.integer(hub_cleaned$From_Node),
                           j = as.integer(hub_cleaned$To_Node),
                           x = 1,
                           dims = c(max_node, max_node))
Layers <- length(nodeTensor)
Nodes <- max_node

# Create node tensor and layer tensor
nodeTensor <- list(powergrid_sparse, gasnetwork_sparse, hub_sparse)
layerTensor <- matrix(1, nrow = Layers, ncol = Layers)

# Calculate supra-adjacency matrix
supra_adjacency <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(nodeTensor, layerTensor, Layers, Nodes)
supra_adjacency


