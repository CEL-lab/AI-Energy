
import pymnet as pm
import pandas as pd
import matplotlib.pyplot as plt

# Read the Excel file into a Pandas DataFrame
df = pd.read_excel("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/ML Data/Default_Case.xlsx")
#stript the trailing spaces from column names
df.columns = df.columns.str.strip()

# Create a multilayer network
network = pm.MultilayerNetwork(aspects=1)

# Add nodes to the network and count electricity and gas nodes
electric_nodes_count = 0
gas_nodes_count = 0

for i in range(len(df)):
    from_node = df.loc[i, "From_Node"]
    from_layer = df.loc[i, "From_Layer"]
    to_node = df.loc[i, "To_Node"]
    to_layer = df.loc[i, "To_Layer"]
    
    network.add_node(from_node, layer=from_layer)
    network.add_node(to_node, layer=to_layer)

    if from_layer == 1:
        electric_nodes_count += 1
    else:
        gas_nodes_count += 1

# Count interlayer edges
interlayer_edges_count = 0

# Add edges to the network and count interlayer edges
for i in range(len(df)):
    from_node = df.loc[i, "From_Node"]
    to_node = df.loc[i, "To_Node"]
    from_layer = df.loc[i, "From_Layer"]
    to_layer = df.loc[i, "To_Layer"]
    
    if from_layer == 1 and to_layer == 2:
        network[from_node, to_node, from_layer, to_layer] = 1
        interlayer_edges_count += 1
    else:
        network[from_node, to_node, from_layer, to_layer] = 1

# Get the supra-adjacency matrix and node-layer pairs
#supra_adjacency, node_layer_pairs = network.get_supra_adjacency_matrix()
supra_adjacency=pm.supra_adjacency_matrix(network, includeCouplings=True)

# Print the counts
print(f"Number of Electricity nodes: {electric_nodes_count}")
print(f"Number of Gas nodes: {gas_nodes_count}")
print(f"Number of interlayer edges: {interlayer_edges_count}")
print(f"Supra Adjacency: {supra_adjacency}")
#print(f"Node Layer Pairs: {node_layer_pairs}")

# Visualize the network
#fig = pm.draw(network, show=True)
#fig = pm.draw(network, cmap="viridis", show=True)
#fig = pm.draw(network, layer_colors=["red", "blue"], show=True)

# Define the layer colors using a dictionary
layerColorDict = {
    1: "red",    # Color for layer 1 (Electricity)
    2: "orange", # Color for layer 2 (Gas)
}

# Define the layer labels using a dictionary
layerLabelDict = {
    1: "Electricity",  # Label for layer 1
    2: "Gas",          # Label for layer 2
}

# Define the interlayer edge color
interLayerEdgeColor = "green"

# Visualize the network with customized layer colors, labels, and interlayer edge color
fig = pm.draw(network,
             show=False,
             layout="spring",
             layerColorDict=layerColorDict,
             defaultLayerColor="gray",
             layerLabelDict=layerLabelDict,  # Customized layer labels
             nodeLabelRule={},
             edgeColorRule={
                 "rule": "edgeweight",
                 "colormap": "jet",
                 "scaleby": 0.1,
                 "inter": interLayerEdgeColor  # Set interlayer edge color
             },
             layergap=2.0  # Adjust the gap between layers
             )
plt.gcf().set_size_inches(7, 7)
plt.savefig("/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Journal Paper/Over leaf /ML/network_plot.pdf")  # Specify your desired path and file name
plt.show()



