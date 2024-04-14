import pandas as pd
import pymnet as pm

# Step 1: Load the Excel files

file_paths = [
    "/mmfs1/home/muhammad.kazim/Case_1.xlsx",
    "/mmfs1/home/muhammad.kazim/Case_2.xlsx",
    "/mmfs1/home/muhammad.kazim/Case_3.xlsx",
    "/mmfs1/home/muhammad.kazim/Case_4.xlsx",
    "/mmfs1/home/muhammad.kazim/Case_5.xlsx"
    
]

dfs = [pd.read_excel(fp) for fp in file_paths]

# Step 2: Aggregate the data from each DataFrame

agg_dfs = []

for df in dfs:
    # Group by From_Node and To_Node and sum the Flow values
    agg_df = df.groupby(["From_Node", "To_Node"]).agg({"Flow": "sum"}).reset_index()
    agg_dfs.append(agg_df)

# Step 3: Construct the multilayer network using pymnet

net = pm.MultilayerNetwork(aspects=1)

# For each aggregated DataFrame, create a single-layer network and add it to the multilayer network
for idx, agg_df in enumerate(agg_dfs):
    layer_name = f"Layer_{idx+1}"
    for _, row in agg_df.iterrows():
        from_node = row["From_Node"]
        to_node = row["To_Node"]
        flow = row["Flow"]
        
        # Add nodes and edges to the multilayer network
        net.add_node(from_node, layer=layer_name)
        net.add_node(to_node, layer=layer_name)
        net[from_node, to_node, layer_name, layer_name] = flow

# ... [rest of the code up to the multilayer network creation]

# Step 4: Visualize the multilayer network using pymnet with improved aesthetics

# Define the layer colors
layer_colors = {
    "Layer_1": "red",
    "Layer_2": "blue",
    "Layer_3": "green",
    "Layer_4": "purple",
    "Layer_5": "orange"
}

fig = pm.draw(
    net,
    layout="random", 
    layergap=0.6,  # Increasing the distance between layers
    defaultLayerColor="gray",
    layerColorDict=layer_colors,
    defaultEdgeWidth=0.2,  # Decreasing the edge width
    edgeWidthDict={(i, j, k, l, w): 0.2 for i, j, k, l, w in net.edges}, 
    nodeSizeRule={"rule": "scaled", "scalecoeff": 0.1},  # Increasing node size
    nodeLabelRule={},  
    edgeColorRule={"rule": "edgeweight", "colormap": "jet", "scaleby": 0.1},
    show=True
)
