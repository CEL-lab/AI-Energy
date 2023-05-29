library(muxViz)
library(openxlsx)
library(igraph)


powergrid_data <- read.xlsx("D:/NDSU/PhD Work/Research/IME Research multinet_course/Belgium Updated.xlsx", sheet = "PowerGrid")
gasnetwork_data <- read.xlsx("D:/NDSU/PhD Work/Research/IME Research multinet_course/Belgium Updated.xlsx", sheet = "GasNetwork")
hub_data <- read.xlsx("D:/NDSU/PhD Work/Research/IME Research multinet_course/Belgium Updated.xlsx", sheet = "HUB")

# Select only the "From_Node" and "To_Node" columns

powergrid_data <- powergrid_data[, c("From_Node", "To_Node")]
gasnetwork_data <- gasnetwork_data[, c("From_Node", "To_Node")]
hub_data <- hub_data[, c("From_Node", "To_Node")]


# Combine all edge data into a single data frame
mEdges <- rbind(powergrid_data, gasnetwork_data, hub_data)


# Remove rows with missing values in "From_Node" and "To_Node" columns
mEdges <- na.omit(mEdges[, c("From_Node", "To_Node")])
mEdges

# Align the rows between From_Node and layer.from
mEdges <- mEdges[!is.na(mEdges$From_Node) & !is.na(mEdges$From_Node), ]
mEdges
str(mEdges)
dim(mEdges)



# Create a new data frame with adjusted column names and assign layer numbers

# Create adjusted_mEdges dataframe with layer assignments
adjusted_mEdges <- data.frame(
  node.from = mEdges$From_Node,
  layer.from = ifelse(mEdges$From_Node %in% powergrid_data$From_Node, 1, 2),
  node.to = mEdges$To_Node,
  layer.to = ifelse(mEdges$To_Node %in% gasnetwork_data$From_Node, 2, 1),
  weight = 1
)


adjusted_mEdges

# Determine the number of layers
num_layers <- 2

# Create the list of nodes
nodes <- unique(c(mEdges$From_Node, mEdges$To_Node))

# Determine the number of nodes
num_nodes <- length(nodes)

Layers<-num_layers
Layers
num_nodes
head(adjusted_mEdges)

str(adjusted_mEdges)
summary(adjusted_mEdges)



# Build the supra-adjacency matrix
supra_adjacency_matrix <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(
  mEdges = adjusted_mEdges,
  Layers = num_layers,
  Nodes = num_nodes,
  isDirected = TRUE
)


# Create the multilayer network
network <- graph_from_adjacency_matrix(supra_adjacency_matrix, mode = "directed")

# Set the layer attribute for each node
V(network)$layer <- rep(1:num_layers, each = num_nodes)

# Set the layout algorithm
layout <- layout_with_fr(network)

# Plot the network with customized options
plot(
  network,
  layout = layout,
  vertex.size = 10,
  vertex.label = NA,
  edge.width = 0.8,
  edge.arrow.size = 0.5,
  edge.curved = 0.2,
  edge.color = "gray",
  vertex.color = c("skyblue", "lightgreen"),
  vertex.frame.color = "white",
  vertex.label.color = "black",
  main = "Multilayer Network Plot"
)
