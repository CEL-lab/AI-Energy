library(readxl)
library(muxViz)
library(ggplot2)
library(openxlsx)
library(Matrix)
library(MASS)  
library(ggrepel)

# Read the Case_1 data
case1_data <- read.xlsx("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/ML Data/Default_Case.xlsx")

# Create edge list using Flow as weight
mEdges_flow <- case1_data[, c("From_Node", "From_Layer", "To_Node", "To_Layer", "Flow")]
colnames(mEdges_flow) <- c("node.from", "layer.from", "node.to", "layer.to", "weight")

# Create edge list using Length as weight
mEdges_length <- case1_data[, c("From_Node", "From_Layer", "To_Node", "To_Layer", "Length")]
colnames(mEdges_length) <- c("node.from", "layer.from", "node.to", "layer.to", "weight")

# Determine the number of layers and nodes dynamically
Layers <- max(max(case1_data$From_Layer), max(case1_data$To_Layer))
Nodes  <- max(max(case1_data$From_Node), max(case1_data$To_Node))

# Generate the supra-adjacency matrix using Flow as weight
SA_flow <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(mEdges_flow, Layers, Nodes, TRUE)

# Generate the supra-adjacency matrix using Length as weight
SA_length <- BuildSupraAdjacencyMatrixFromExtendedEdgelist(mEdges_length, Layers, Nodes, TRUE)

# Flow-based measures:
MEV_flow <- GetMultiEigenvectorCentrality(SA_flow, Layers, Nodes)
MKZ_flow <- GetMultiKatzCentrality(SA_flow, Layers, Nodes)
MPR_flow <- GetMultiPageRankCentrality(SA_flow, Layers, Nodes)
MRW_flow <- GetMultiRWCentrality(SA_flow, Layers, Nodes, Type = "classical", Method = "multilayer")

#ordered_indices <- order(MRW_flow, decreasing = TRUE)
#print(MRW_flow[ordered_indices])


# Path-based measures:
MC_length <- GetMultiClosenessCentrality(SA_length, Layers, Nodes)
MH_length <- GetMultiHubCentrality(SA_length, Layers, Nodes)
MKC_length <- GetMultiKCoreCentrality(SA_length, Layers, Nodes)


#ordered_indices <- order(MKC_length, decreasing = TRUE)
#print(MKC_length[ordered_indices])

# Measures that could potentially use either:

# Using Flow for these as an example, but you might switch to length based on specific objectives
TM_flow <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA_flow, Layers, Nodes)
TM_length <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA_length, Layers, Nodes)

IAS_flow <- GetInterAssortativityTensor(SA_flow, Layers, Nodes, isDirected = TRUE, Type = "OO")
IAS_length <- GetInterAssortativityTensor(SA_length, Layers, Nodes, isDirected = TRUE, Type = "OO")
AC_flow <- GetMultiAuthCentrality(SA_flow, Layers, Nodes)
MD_flow <- GetMultiDegreeSum(SA_flow, Layers, Nodes, isDirected = TRUE)
MD_length <- GetMultiDegreeSum(SA_length, Layers, Nodes, isDirected = TRUE)

#ordered_indices <- order(AC_flow, decreasing = TRUE)
#print(AC_flow[ordered_indices])

#### Introduce Disruption Logic ####
# Example disruption: remove a percentage of random nodes
set.seed(43) # for reproducibility
disruption_percentage <- 0.1 # 10% of nodes
num_nodes_to_remove <- floor(Nodes * disruption_percentage)

# Randomly select nodes to remove
nodes_to_remove <- sample(1:Nodes, num_nodes_to_remove)

# Set the rows and columns corresponding to the selected nodes to zero in the supra-adjacency matrices
for (node in nodes_to_remove) {
  SA_flow[node, ] <- 0
  SA_flow[, node] <- 0
  SA_length[node, ] <- 0
  SA_length[, node] <- 0
}

# Recompute the Supra-transition matrices post-disruption
TM_flow_disrupted <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA_flow, Layers, Nodes)
TM_length_disrupted <- BuildSupraTransitionMatrixFromSupraAdjacencyMatrix(SA_length, Layers, Nodes)

                                          ######################################
                                          ######## Coverage Evolution ##########                  
                                          ######################################

GetCoverageEvolutionMultilayer_modified <- function(SupraTransitionMatrix,
                                                    Layers,
                                                    Nodes,
                                                    TimeSequence,
                                                    Approximate = FALSE,
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
  
  
  Coverage.raw <- function(Nu,
                           LM,
                           Layers,
                           Nodes,
                           taus,
                           Approximate,
                           zero.idxs,
                           lambda2.idx) {
    rho <- rep(0, length(taus))
    Order <- Layers * Nodes
    
    pb <- utils::txtProgressBar(min = 1,
                                max = Nodes,
                                style = 3)
    
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
  
  rho <- Coverage(Nu=Nu,
                  LM,
                  Layers,
                  Nodes,
                  TimeSequence,
                  Approximate,
                  zero.idxs,
                  lambda2.idx)
  rho.df <- data.frame(tau = TimeSequence, rho = rho)
  
  return(rho.df)
}

### Coverage Evolution Analysis (post-disruption)

CEM <- GetCoverageEvolutionMultilayer_modified(TM_length_disrupted, Layers, Nodes, 1:20)
print(CEM)

                                          ######################################
                                          ########## Plotting Logic ############
                                          ######################################
# Generalized plotting function
plot_centrality <- function(centrality_values, title, num_top_electricity = 5, num_top_gas = 5) {
  
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
    geom_text_repel(aes(label = Label), size = 4) +
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
print(plot_centrality(MEV_flow, "Multi Layer Eigenvector Centrality"))
ggsave("ML_Eigon.png", width = 10, height = 7, units = "in")

print(plot_centrality(MKZ_flow, "Multi Layer Flow-Based Katz Centrality"))

print(plot_centrality(MPR_flow, "Multi Layer PageRank Centrality"))
ggsave("ML_PageR.png", width = 10, height = 7, units = "in")

print(plot_centrality(MRW_flow, "Multi Layer RandomWalk Centrality"))
ggsave("ML_RandomW.png", width = 10, height = 7, units = "in")

print(plot_centrality(AC_flow, "Multi Layer Flow-Based Authority Centrality"))
print(plot_centrality(MD_flow, "Multi Layer Degree Sum Centrality"))
ggsave("ML_F_Degree.png", width = 10, height = 7, units = "in")

# For the new centralities based on length:
plot_centrality(MC_length$closeness, "Multi Layer Closeness Centrality")
ggsave("ML_L_Closeness.png", width = 10, height = 7, units = "in")

print(plot_centrality(MH_length, "Multi Layer Length-Based Hub Centrality"))
print(plot_centrality(MKC_length, "Multi Layer Length-Based K-Core Centrality"))

# For the new centralities that can use either flow or length
print(plot_centrality(MD_length, "Multi Layer Length-Based Closeness Centrality"))

print(plot_centrality(TM_flow, "Multi Layer Flow-Based Closeness Centrality"))
print(plot_centrality(IAS_flow, "Multi Layer Flow-Based Closeness Centrality"))
print(plot_centrality(AC_flow, "Multi Layer Flow-Based Closeness Centrality"))

############# Plotting the coverage evolution #############

generate_coverage_plot <- function(cov_data, plot_title) {
  # Convert 'rho' to a complex type if it isn't already and extract the real part
  cov_data$Coverage <- Re(as.complex(cov_data$rho))
  
  # Adding labels to every 10th point
  cov_data$label <- ifelse(seq_along(cov_data$tau) %% 10 == 0, 
                           paste("τ=", round(cov_data$tau, 2), "\nCov=", round(cov_data$Coverage, 4)), 
                           NA)
  
  # Generate the plot using ggplot2
  ggplot(cov_data, aes(x = tau, y = Coverage)) +
    geom_line(color = "#1f78b4", size = 1.2) +  # Blue line for the plot
    geom_point(color = "#d7191c", size = 2) +   # Red points
    geom_text_repel(aes(label = label), size = 3, 
                    vjust = -2,  # Adjust this value to position labels above the points
                    color = "#333333", 
                    nudge_y = 0.02,  # Nudge upwards for clear visibility
                    segment.color = NA,  # Remove the repel lines
                    check_overlap = TRUE) +
    scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), 
                  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    scale_y_continuous(limits = c(0, max(cov_data$Coverage, na.rm = TRUE)), 
                       expand = c(0, 0)) +
    theme_light() +
    theme(panel.grid.major = element_line(color = "#cccccc"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 12),
          axis.text = element_text(color = "#333333"),
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.title = element_text(size = 14)) +
    labs(title = plot_title, x = "Diffusion Time (tau)", y = "Coverage")
}

# Prepare the coverage data for plotting
cov_data <- transform(original_cov_data, Coverage = Re(rho))

# Generate and print the coverage plot
original_cov_plot <- generate_coverage_plot(cov_data, "Multilayer Network Coverage")
print(original_cov_plot)
print(cov_data)


