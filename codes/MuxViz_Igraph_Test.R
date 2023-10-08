library(muxViz)
library(igraph)
library(Matrix)
library(readxl)
library(ggrepel)

# Function to process each file and return its adjacency matrix
process_file <- function(file_path, attr_name, overall_unique_nodes) {
  df <- read_excel(file_path)
  
  # Trim the spaces from the From_Node and To_Node columns
  df$From_Node <- trimws(df$From_Node)
  df$To_Node <- trimws(df$To_Node)
  
  # Aggregating the edges based on From_Node and To_Node, summing up the desired attribute
  agg_formula <- as.formula(paste(attr_name, "~ From_Node + To_Node"))
  aggregated_edges <- aggregate(agg_formula, data=df, sum)
  
  # Create an igraph object from the aggregated edge list
  g <- graph_from_data_frame(aggregated_edges, directed=TRUE, vertices=overall_unique_nodes)
  
  # Extract and return adjacency matrix
  return(as_adjacency_matrix(g, type="both", attr=attr_name))
}

# Function to extract unique nodes from a file
extract_unique_nodes <- function(file_path) {
  df <- read_excel(file_path)
  unique_nodes <- unique(c(trimws(df$From_Node), trimws(df$To_Node)))
  return(unique_nodes)
}

# File paths
file_paths <- c("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_1.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_2.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_3.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_4.xlsx",
                "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_5.xlsx")

# Extract unique nodes from each file
unique_nodes_list <- lapply(file_paths, extract_unique_nodes)

# Combine unique nodes from all files to get overall unique nodes
overall_unique_nodes <- unique(unlist(unique_nodes_list))

# Process each file for Length
adj_matrices_length <- lapply(file_paths, process_file, attr_name="Length", overall_unique_nodes=overall_unique_nodes)

# Construct the supra-adjacency matrix for Length
supra_adj_matrix_length <- do.call(bdiag, adj_matrices_length)

# Process each file for Flow
adj_matrices_flow <- lapply(file_paths, process_file, attr_name="Flow", overall_unique_nodes=overall_unique_nodes)

# Construct the supra-adjacency matrix for Flow
supra_adj_matrix_flow <- do.call(bdiag, adj_matrices_flow)

# Print the dimensions of the supra-adjacency matrices
print(dim(supra_adj_matrix_length))
print(dim(supra_adj_matrix_flow))

###### Centrality logice #########

Layers <- 5
Nodes <- nrow(supra_adj_matrix_flow) / Layers  # This assumes that all layers have the same number of nodes
Nodes


######## Flow Based ###########

AM_flow <- GetAggregateNetworkFromSupraAdjacencyMatrix(supra_adj_matrix_flow, Layers, Nodes)
TM_flow <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(supra_adj_matrix_flow, Layers, Nodes)
IAS_flow <- GetInterAssortativityTensor(supra_adj_matrix_flow, Layers, Nodes, isDirected = TRUE, Type = "OO")
AC_flow  <- GetMultiAuthCentrality(supra_adj_matrix_flow, Layers, Nodes)
MD_flow  <- GetMultiDegreeSum(supra_adj_matrix_flow, Layers, Nodes, isDirected = TRUE)
MEV_flow <- GetMultiEigenvectorCentrality(supra_adj_matrix_flow, Layers, Nodes)
MKZ_flow <- GetMultiKatzCentrality(supra_adj_matrix_flow, Layers, Nodes)
MPR_flow <- GetMultiPageRankCentrality(supra_adj_matrix_flow, Layers, Nodes)
MRW_flow <- GetMultiRWCentrality(supra_adj_matrix_flow, Layers, Nodes, Type = "classical", Method = "multilayer")
IAS_flow

######## Length Based ##########

MC_length  <- GetMultiClosenessCentrality(supra_adj_matrix_length, Layers, Nodes)
MH_length  <- GetMultiHubCentrality(supra_adj_matrix_length, Layers, Nodes)
MKC_length <- GetMultiKCoreCentrality(supra_adj_matrix_length, Layers, Nodes)
MD_length  <- GetMultiDegreeSum(supra_adj_matrix_length, Layers, Nodes, isDirected = TRUE)
TM_flow <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(supra_adj_matrix_flow, Layers, Nodes)
TM_length <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(supra_adj_matrix_length, Layers, Nodes)

############## Plotting Logic ###############

# Generalized plotting function
plot_centrality <- function(centrality_values, title, num_top_electricity = 5, num_top_gas = 8) {
  
  # Get the indices of the top nodes
  top_indices <- order(centrality_values, decreasing = TRUE)
  top_indices <- top_indices[centrality_values[top_indices] > 0]
  
  # Get the indices of the top Electricity (E) nodes
  top_electricity <- top_indices[top_indices <= 75][1:num_top_electricity]
  
  # Get the indices of the top Gas (G) nodes
  top_gas <- top_indices[top_indices > 75][1:num_top_gas]
  
  # Combine the indices for top Electricity and Gas nodes
  top_nodes <- c(top_electricity, top_gas)
  
  # Create a data frame
  plot_data <- data.frame(Index = top_nodes,
                          Values = centrality_values[top_nodes],
                          Label = ifelse(top_nodes <= 75, paste0("E", top_nodes),
                                         paste0("G", top_nodes)))
  
  # Create a new column for shapes based on condition (circles for Electricity, squares for Gas)
  plot_data$Shape <- ifelse(plot_data$Index <= 75, "E", "G")
  label_position <- ifelse(plot_data$Index %in% top_nodes, "right", "left")
  
  # Create the faceit plot
  faceit_plot <- ggplot(plot_data, aes(x = Index, y = Values, group = 1)) +
    geom_line(color = "black") +
    geom_point(aes(shape = Shape, color = Shape), size = 2) +
    geom_text_repel(aes(label = Label),
                    nudge_x = ifelse(label_position == "right", 0.3, -0.3),
                    box.padding = 0.5, point.padding = 0.5, segment.color = "transparent") +
    labs(title = title, x = "Nodes", y = "Values",
         color = "Category") +
    scale_color_manual(name = "Node Type",
                       values = c("E" = "red", "G" = "orange"),
                       labels = c("Electricity", "Gas")) +
    scale_shape_manual(name = "Node Type",
                       values = c("E" = 16, "G" = 15),
                       labels = c("Electricity", "Gas")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "right")
  
  return(faceit_plot)
}

# For the new centralities based on flow:
print(plot_centrality(MEV_flow, "MultiPlex Eigenvector Centrality"))
ggsave("MP_Eigon.png", width = 10, height = 7, units = "in")

print(plot_centrality(MKZ_flow, "MultiPlex Flow-Based Katz Centrality"))

print(plot_centrality(MPR_flow, "MultiPlex PageRank Centrality"))
ggsave("MP_PageR.png", width = 10, height = 7, units = "in")

print(plot_centrality(MRW_flow, "MultiPlex RandomWalk Centrality"))
ggsave("MP_RandomW.png", width = 10, height = 7, units = "in")

print(plot_centrality(AC_flow, "MultiPlex Flow-Based Authority Centrality"))

print(plot_centrality(MD_flow, "MultiPlex Degree Sum Centrality"))
ggsave("MP_F_Degree.png", width = 10, height = 7, units = "in")



# For the new centralities based on length:
plot_centrality(MC_length$closeness, "MultiPlex Closeness Centrality")
ggsave("MP_L_Closeness.png", width = 10, height = 7, units = "in")

print(plot_centrality(MH_length, "MultiPlex Length-Based Hub Centrality"))
print(plot_centrality(MKC_length, "MultiPlex Length-Based K-Core Centrality"))

# For the new centralities that can use either flow or length
print(plot_centrality(MD_length, "MultiPlex Degree Centrality"))
ggsave("MP_L_Degree.png", width = 10, height = 7, units = "in")

print(plot_centrality(AC_flow, "MultiPlex Flow-Based Authority Centrality"))

                                          ######################################
                                          ######## Coverage Evolution ##########                  
                                          ######################################

GetCoverageEvolutionMultilayer_modified <- function(SupraTransitionMatrix, Layers, Nodes, TimeSequence, Approximate = FALSE,
                                           Approximate.disconnected = 0) {
  
  Order <- Layers * Nodes
  SupraLaplacianMatrix <- diag(Order) - SupraTransitionMatrix
  cat(paste("  :: Eigendecomposing...", "\n"))
  
  L.eigendec <- eigen(SupraLaplacianMatrix)
  QM <- as.matrix(L.eigendec$vectors)
  
  cat(paste("  :: Computing pseudo-inverse...", "\n"))
  invQM <- ginv(QM)
  
  LM <- L.eigendec$values
  
  zero.idxs <- which(abs(Re(LM)) < 1e-12)
  #lambda2.idx <- which.min(LM[-zero.idxs])
  lambda2.idx <- which.min(Mod(LM[-zero.idxs]))
  
  cat(paste("  :: There are", length(zero.idxs), "zero eigenvalues", "\n"))
  
  lambdas.sort <- sort(LM[-zero.idxs])
  cat(paste("  :: l3 - l2 =", lambdas.sort[2] - lambdas.sort[1], "\n"))
  cat(paste(
    "  :: 90% difference expected for tau >",-log(1 - 0.9) / (lambdas.sort[2] - lambdas.sort[1]),
    "\n"
  ))
  
  Nu <- vector("list", Order)
  if (!Approximate) {
    for(l in 1:Order) {
      Nu[[l]] <- (QM[, l] %*% Matrix::t(invQM[l,])) %*% as.matrix(SupraTransitionMatrix)
    }
  } else {
    cat(paste("WARNING! Approximation might be very poor if the network is not connected.\n"))
    for(l in c(zero.idxs, lambda2.idx)) {
      Nu[[l]] <- (QM[, l] %*% Matrix::t(invQM[l,])) %*% as.matrix(SupraTransitionMatrix)
    }
  }
  
  Coverage.raw <- function(Nu, LM, Layers,Nodes, taus, Approximate, zero.idxs, lambda2.idx) {
    rho <- rep(0, length(taus))
    Order <- Layers * Nodes
    
    pb <- utils::txtProgressBar(min = 1, max = Nodes, style = 3)
    
    lapply(1:Nodes, function(i) {
      Ei <- i + Nodes * (1:Layers - 1)
      lapply(1:Nodes, function(j) {
        deltaij0 <- ifelse(i == j, 0, 1)
        exparg <- rep(0, length(taus))
        
        if (!Approximate) {
          lapply(1:Order, function(outer_l) {
            Cijl <- sum(Nu[[outer_l]][j, Ei])
            if (outer_l %in% zero.idxs) {
              exparg <<- exparg + Cijl * taus
            } else {
              exparg <<- exparg + Cijl * (1 - exp(-LM[outer_l] * taus)) / LM[outer_l]
            }
          })
        } else {
          lapply(c(zero.idxs, lambda2.idx), function(outer_l) {
            Cijl <- sum(Nu[[outer_l]][j, Ei])
            if (outer_l %in% zero.idxs) {
              exparg <<- exparg + Cijl * taus
            } else {
              exparg <<- exparg + Cijl * (1 - exp(-LM[outer_l] * taus)) / LM[outer_l]
            }
          })
        }
        rho <<- rho + exp(-exparg) * deltaij0
      })
      utils::setTxtProgressBar(pb, i)
    })
    close(pb)
    
    rho <- 1 - rho / (Nodes ^ 2)
    return(rho)
  }
  
  Coverage <- compiler::cmpfun(Coverage.raw)
  
  cat(paste("  :: Calculating coverage...", "\n"))
  
  rho <- Coverage(Nu=Nu, LM, Layers, Nodes, TimeSequence, Approximate, zero.idxs, lambda2.idx)
  rho.df <- data.frame(tau = TimeSequence, rho = rho)
  
  return(rho.df)
}

CEM <- GetCoverageEvolutionMultilayer_modified(TM_length, Layers,Nodes,1:5)
print(CEM)

