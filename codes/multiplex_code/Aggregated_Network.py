import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt

# Replace with the correct path to your Excel file
file_path = '/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_1.xlsx'

# Load the Excel file
df = pd.read_excel(file_path)

# Create a graph from the dataframe
G = nx.from_pandas_edgelist(df, 'From_Node', 'To_Node')

# Plot the graph
plt.figure(figsize=(12, 8))
nx.draw(G, with_labels=True, node_color='lightblue', edge_color='gray', node_size=500)
plt.title("Network Visualization")
plt.show()
