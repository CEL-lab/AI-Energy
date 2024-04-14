import pandas as pd
import networkx as nx
import Networkx_robustness_libray_modified 
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import seaborn as sns
import plotly.graph_objects as go
import os


def get_short_file_name(file_path):
    # Extract the filename from the file path
    return file_path.split("/")[-1]

def refined_aggregate_multilayer_from_excel(file_path, attribute="Flow"):
    """Aggregate multilayer network data from an Excel file."""
    print(f"Processing file: {file_path}")
    
    # Read the excel file
    df = pd.read_excel(file_path)
    
    # Remove any trailing spaces from column names
    df.columns = [col.strip() for col in df.columns]

    # Check if the specified attribute column exists
    if attribute not in df.columns:
        print(f"'{attribute}' column missing in file: {file_path}")
        return None

    # Aggregate the data
    aggregated = df.groupby(['From_Node', 'From_Layer', 'To_Node', 'To_Layer'])[attribute].sum().reset_index()
    aggregated = aggregated.rename(columns={attribute: 'Flow'})  # Rename the attribute column to 'Flow'
    
    return aggregated


def create_graph_from_aggregated_df(aggregated_df):
    """Create a graph object from an aggregated DataFrame."""
    graph = nx.DiGraph()
    for _, row in aggregated_df.iterrows():
        from_node = row['From_Node']
        from_layer = row['From_Layer']
        to_node = row['To_Node']
        to_layer = row['To_Layer']
        weight = row['Flow']
        graph.add_edge((from_node, from_layer), (to_node, to_layer), weight=weight)
    return graph

def combine_networks(graph_list):
    """Combine a list of graph objects into a single graph."""
    combined_graph = nx.DiGraph()
    for graph in graph_list:
        combined_graph = nx.compose(combined_graph, graph)
    return combined_graph

def simulate_attack(attack, graph, attack_fraction=0.1, weight='weight'):
    """
    Simulate attack on a network
    :param attack: attack method (random, degree, betweenness, closeness, eigenvector)
    :param graph: networkx graph
    :param attack_fraction: fraction of nodes to be attacked (default: 0.1)
    :param weight: weight of edges (default: weight)
    :return: initial (float), frac (list), apl (list)
    """
    file_name = graph.name.split('/')[-1]  # Get the file name from the graph
    if attack == "random":
        attack_type = "Random"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_random_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
    elif attack == "degree":
        attack_type = "Degree"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_degree_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
    elif attack == "betweenness":
        attack_type = "Betweenness"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_betweenness_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
    elif attack == "closeness":
        attack_type = "Closeness"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_closeness_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
    elif attack == "eigenvector":
        attack_type = "Eigenvector"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_eigenvector_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
    elif attack == "k_core":
        attack_type = "K-core"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_k_core_attack(G=graph, attack_fraction=attack_fraction)
    else:
        raise ValueError("Invalid attack method. Available options are: random, degree, betweenness, closeness, eigenvector")

    attack_steps = list(range(1, len(frac) + 1))
    return attack_type, file_name, initial, frac, apl, removed_nodes, attack_steps

def plot_attack_results(frac, apl, attack_type, file_name, removed_nodes, attack_steps):
    
    # Using basic Matplotlib commands
    plt.rcParams["axes.facecolor"] = "white"
    plt.rcParams["figure.facecolor"] = "white"
    
    # Create a figure with two subplots
    fig, axes = plt.subplots(2, 1, figsize=(10, 10))
    
    # Plot fraction of nodes removed
    axes[0].plot(attack_steps, frac, color='black', label='Fraction of Nodes Remaining')
    axes[0].set_xlabel('Attack Step')
    axes[0].set_ylabel('Fraction of Nodes Remaining', fontsize=12, fontweight='bold', color='red')
    axes[0].grid(True, color='gray', linestyle='--', linewidth=0.5)  # Manual grid

    # Plot average path length after attack
    axes[1].plot(attack_steps, apl, color='black', label='Average Path Length')
    axes[1].set_xlabel('Attack Step')
    axes[1].set_ylabel('Average Path Length', fontsize=12, fontweight='bold', color='blue')
    axes[1].grid(True, color='gray', linestyle='--', linewidth=0.5)  # Manual grid

    # Iterate over attack steps, frac, and apl to add annotations
    for step, f, a in zip(attack_steps, frac, apl):
        if step <= len(removed_nodes):
            node_label = str(removed_nodes[step - 1]).split('.')[0]
            node_label = node_label.split("(")[1] if "(" in node_label else node_label

            # Determine node properties based on the label
            if int(node_label) <= 75:
                node_label = f'E{node_label}'
                node_color = 'red'
                node_marker = 'o'
            else:
                node_label = f'G{node_label}'
                node_color = 'orange'
                node_marker = 's'

            # Label the nodes on the fraction plot
            axes[0].text(step, f + 0.005, node_label, fontsize=8, ha='left', va='bottom', color='black')  
            axes[0].scatter(step, f, color=node_color, marker=node_marker, s=50, zorder=5) 

            # Label the nodes on the average path length plot, further adjusted vertical position
                # Label the nodes on the average path length plot using annotate
            axes[1].annotate(node_label, (step, a), textcoords="offset points", xytext=(0,10),  # This offsets the text by 10 points vertically
                    ha='center', 
                     fontsize=8, 
                     color='black')
            axes[1].scatter(step, a, color=node_color, marker=node_marker, s=50, zorder=5)

    # Create custom legend handles with desired marker shapes
    electricity_marker = mlines.Line2D([], [], color='red', marker='o', markersize=10, label='Electricity')
    gas_marker = mlines.Line2D([], [], color='orange', marker='s', markersize=10, label='Gas')

    # Add legend to both subplots with custom handles
    axes[0].legend(handles=[electricity_marker, gas_marker])
    axes[1].legend(handles=[electricity_marker, gas_marker])
    
    # Set the title for the entire figure and remove file extension
    short_title = file_name.replace('.xlsx', '')
    #fig.suptitle(f'{attack_type} Attack on {short_title}', fontsize=16)
    
    # Customize the gridlines
    axes[0].grid(color='gray', linestyle='--', linewidth=0.5)
    axes[1].grid(color='gray', linestyle='--', linewidth=0.5)

    sns.despine()
 
    fig.set_size_inches(10, 7)
    plt.subplots_adjust(left=0.1, right=0.9, top=0.9, bottom=0.1)  # Adjust margins to make more space for node labels
    plt.tight_layout()
    plt.show()


excel_files = [
    "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_1.xlsx",
    "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_2.xlsx",
    "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_3.xlsx",
    "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_4.xlsx",
    "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/MP Data/Case_5.xlsx"
]

if __name__ == "__main__":

    #####################################################################
    # Specify the attacks and operations you want to perform
    #target_attacks = ["random", "degree", "betweenness", "closeness", "eigenvector", "k_core"]
    target_attacks = ["random"]
    target_operations = ["molloy_reed", "critical_threshold"]
    #####################################################################


    # Aggregating the multilayer networks from all files
    graph_list = []
    for file in excel_files:
        attribute = "Length" if "closeness" in target_attacks else "Flow"
        aggregated_df = refined_aggregate_multilayer_from_excel(file, attribute=attribute)
        if aggregated_df is not None:
            graph = create_graph_from_aggregated_df(aggregated_df)
            graph_list.append(graph)

    # Combining the aggregated graphs into a single multiplex network
    multiplex_graph = combine_networks(graph_list)

    # Performing the specified attacks on the multiplex network
    for attack in target_attacks:
        attack_type, _, initial, frac, apl, removed_nodes, attack_steps = simulate_attack(attack, multiplex_graph, attack_fraction=0.1, weight='weight')
        print(f"{attack_type} Attack on entire multiplex network:")
        print(f"Initial attack size: {initial}")
        print(f"Fraction of nodes remaining: {frac}")
        print(f"Average path length after attack: {apl}")
        print(f"Nodes Removed: {removed_nodes}")
        print("\n")

        plot_attack_results(frac, apl, attack_type=attack_type, file_name="Multiplex Network", removed_nodes=removed_nodes, attack_steps=attack_steps)

    # Performing the specified operations on the multiplex network
    for operation in target_operations:
        if operation == "molloy_reed":
            molloy_reed_value = Networkx_robustness_libray_modified.molloy_reed(G=multiplex_graph)
            print(f"Molloy-Reed criterion for the entire multiplex network: {molloy_reed_value}")
        elif operation == "critical_threshold":
            critical_threshold_value = Networkx_robustness_libray_modified.critical_threshold(G=multiplex_graph)
            print(f"Critical threshold for the entire multiplex network: {critical_threshold_value}")
        print("\n")