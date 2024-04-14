import pandas as pd
import networkx as nx
#from networkx_robustness import networkx_robustness
import Networkx_robustness_libray_modified 
import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import seaborn as sns


def create_graph_from_excel(excel_file):
    # Read the Excel file into a Pandas DataFrame
    df = pd.read_excel(excel_file, engine='openpyxl')

    # Remove leading/trailing whitespaces from column names
    df.columns = df.columns.str.strip()

    # Create a directed graph using NetworkX
    graph = nx.DiGraph()

    # Iterate through the rows and add edges with weights to the graph
    for _, row in df.iterrows():
        from_node = row['From_Node']
        from_layer = row['From_Layer']
        to_node = row['To_Node']
        to_layer = row['To_Layer']
        weight = row['Flow']
        graph.add_edge((from_node, from_layer), (to_node, to_layer), weight=weight)

    return graph

def simulate_attack(attack, graph, attack_fraction=0.1, weight='weight'):
    """
    #Simulate attack on a network
    #:param attack: attack method (random, degree, betweenness, closeness, eigenvector)
    #:param graph: networkx graph
    #:param attack_fraction: fraction of nodes to be attacked (default: 0.1)
    #:param weight: weight of edges (default: weight)
    #:return: initial (float), frac (list), apl (list)
    """
    file_name = graph.name.split('/')[-1]  # Get the file name from the graph
    if attack == "random":
        attack_type = "Random"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_random_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
        attack_steps = list(range(1, len(frac) + 1))
        return attack_type, file_name, initial, frac, apl, removed_nodes, attack_steps
    
    elif attack == "degree":
        attack_type = "Degree"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_degree_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
        attack_steps = list(range(1, len(frac) + 1))
        return attack_type, file_name, initial, frac, apl, removed_nodes, attack_steps
    
    elif attack == "betweenness":
        attack_type = "Betweenness"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_betweenness_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
        attack_steps = list(range(1, len(frac) + 1))
        return attack_type, file_name, initial, frac, apl, removed_nodes, attack_steps
    
    elif attack == "closeness":
        attack_type = "Closeness"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_closeness_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
        attack_steps = list(range(1, len(frac) + 1))
        return attack_type, file_name, initial, frac, apl, removed_nodes, attack_steps
    
    elif attack == "eigenvector":
        attack_type = "Eigenvector"
        initial, frac, apl, removed_nodes = Networkx_robustness_libray_modified.simulate_eigenvector_attack(G=graph, attack_fraction=attack_fraction, weight=weight)
        attack_steps = list(range(1, len(frac) + 1))
        return attack_type, file_name, initial, frac, apl, removed_nodes, attack_steps
    
    else:
        raise ValueError("Invalid attack method. Available options are: random, degree, betweenness, closeness, eigenvector")



def plot_attack_results(frac, apl, attack_type, file_name, removed_nodes, attack_steps):
    
    sns.set_style("whitegrid")
    sns.set_palette("pastel")
    sns.set_context("paper")
    sns.set(font_scale=1.2)

    # Create a figure with two subplots
    fig, axes = plt.subplots(2, 1, figsize=(10, 8))
    
    # Plot fraction of nodes removed
    sns.lineplot(x=attack_steps, y=frac, ax=axes[0], color='black', label='Fraction of Nodes Remaining')
    axes[0].set_xlabel('Attack Step')
    axes[0].set_ylabel('Fraction of Nodes Remaining', fontsize=12, fontweight='bold', color='red')
    axes[0].grid(True)

    # Plot average path length after attack
    sns.lineplot(x=attack_steps, y=apl, ax=axes[1], color='black', label='Average Path Length')
    axes[1].set_xlabel('Attack Step')
    axes[1].set_ylabel('Average Path Length', fontsize=12, fontweight='bold', color='blue')
    axes[1].grid(True)

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
            axes[0].text(step, f + 0.02, node_label, fontsize=8, ha='left', va='bottom', color='black')  
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
    fig.suptitle(f'{attack_type} Attack on {short_title}', fontsize=16)
    
    # Customize the gridlines
    axes[0].grid(color='gray', linestyle='--', linewidth=0.5)
    axes[1].grid(color='gray', linestyle='--', linewidth=0.5)

    sns.despine()
    save_file_name = f"{attack_type}_{file_name.replace('.xlsx', '')}.png"   
    fig.set_size_inches(10, 7)
    fig.savefig(save_file_name, dpi=300, bbox_inches='tight', format='png')

    plt.tight_layout()
    plt.show()

def get_short_file_name(file_path):
    # Extract the filename from the file path
    return file_path.split("/")[-1]

# Usage example:

if __name__ == "__main__":
    excel_files = [
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_1.xlsx",
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_2.xlsx",
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_3.xlsx",
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_4.xlsx",
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/Data/Case_5.xlsx",
    ]

    # Nested dictionary to specify attacks and operations for each file
    attacks_and_operations = {
        "Case_1.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        },
        "Case_2.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        },
        "Case_3.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        },
        "Case_4.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        },
        "Case_5.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        }
    }
                            ################### Target File ##################
    target_file = "Case_1.xlsx"
    target_attacks = ["degree"]
    target_operations = ["molloy_reed"]
                            ##################################################

    for file in excel_files:
        if not target_file or get_short_file_name(target_file) == get_short_file_name(file):
            graph = create_graph_from_excel(file)

            for attack in attacks_and_operations[get_short_file_name(file)]["attacks"]:
                if not target_attacks or attack in target_attacks:
                # Perform the specified attack and get the results
                    attack_type, file_name, initial, frac, apl, removed_nodes, attack_steps = simulate_attack(attack, graph, attack_fraction=0.1, weight='weight')
                    print(f"{attack_type} Attack on {file_name}:")
                    print(f"Initial attack size: {initial}")
                    print(f"Fraction of nodes remaining: {frac}")
                    print(f"Average path length after attack: {apl}")
                    print(f"Nodes Removed: {removed_nodes}")
                    print("\n")

                    # Add code to plot the results only for the target attack
                    if attack in target_attacks:
                        # Get the shortened file name
                        short_file_name = get_short_file_name(file)
                        # Modify the plot title to include the shortened file name
                        plot_attack_results(frac, apl, attack_type=attack_type, file_name=short_file_name, removed_nodes=removed_nodes, attack_steps=attack_steps)

            for operation in attacks_and_operations[get_short_file_name(file)]["operations"]:
                if not target_operations or operation in target_operations:
                    # Perform the specified operation
                    if operation == "molloy_reed":
                        molloy_reed_value = Networkx_robustness_libray_modified.molloy_reed(G=graph)
                        print(f"Molloy-Reed criterion for {file_name}: {molloy_reed_value}")
             
                if operation == "critical_threshold":
                    critical_threshold_value = Networkx_robustness_libray_modified.critical_threshold(G=graph)
                    print(f"Critical threshold for {file_name}: {critical_threshold_value}")
            print("\n")
"""
############## Eigon Vector Networkx Code Not Important as Multiplex Network #################

        # Compute eigenvector centrality on the graph with increased iterations
        centralities = nx.eigenvector_centrality(graph, max_iter=250)
        # Print the centralities for each node
        # Print the centralities for each node in descending order
        #sorted_centralities = sorted(centralities.items(), key=lambda x: x[1], reverse=True)
        #for node, centrality in sorted_centralities:
            #print(f"Node {node}: Eigenvector Centrality = {centrality}")
        
        # Exit the script after printing the centralities
        #sys.exit()
        
        # Extract the nodes and corresponding centralities as two separate lists
        nodes = list(graph.nodes())

        # Get centrality values for each node or assign default value if not available
        default_centrality = 0.0
        centrality_values = [centralities.get(node, default_centrality) for node in nodes]

        # Convert nodes to strings for plotting as x-axis labels
        nodes = [str(node) for node in nodes]

        # Define the colors and shapes for nodes based on conditions
        colors = ['red' if idx <= 75 else 'orange' for idx in range(len(nodes))]
        node_shapes = ['o' if idx <= 75 else 's' for idx in range(len(nodes))]
        node_labels = {node: 'Electricity' if idx <= 75 else 'Gas' for idx, node in enumerate(nodes)}

        # Separate 'E' and 'G' nodes and their centrality values
        e_nodes = [node for idx, node in enumerate(nodes) if idx <= 75]
        g_nodes = [node for idx, node in enumerate(nodes) if idx > 75]
        e_centrality = [centrality_values[idx] for idx in range(len(nodes)) if idx <= 75]
        g_centrality = [centrality_values[idx] for idx in range(len(nodes)) if idx > 75]

        # Get the indices of the top 5 nodes from each category
        top_e_indices = sorted(range(len(e_centrality)), key=lambda i: e_centrality[i], reverse=True)[:5]
        top_g_indices = sorted(range(len(g_centrality)), key=lambda i: g_centrality[i], reverse=True)[:5]

        # Get the top 5 nodes and centrality values for each category
        top_e_nodes = [e_nodes[i] for i in top_e_indices]
        top_e_centrality = [e_centrality[i] for i in top_e_indices]
        top_g_nodes = [g_nodes[i] for i in top_g_indices]
        top_g_centrality = [g_centrality[i] for i in top_g_indices]
        
        # Combine the data for both categories
        combined_nodes = top_e_nodes + top_g_nodes
        combined_centrality = top_e_centrality + top_g_centrality

        # Create the plot for top 5 nodes
        plt.figure(figsize=(10, 6))
        plt.plot(combined_nodes, combined_centrality, marker='o', linestyle= '-', color='black')
        #plt.plot(combined_nodes, combined_centrality, marker='o', linestyle='-', color='black', markerfacecolor=colors, markersize=5)
        plt.scatter(top_e_nodes, top_e_centrality, c='red', marker='o', s=100, label='Top 5 Electricity')
        plt.scatter(top_g_nodes, top_g_centrality, c='orange', marker='s', s=100, label='Top 5 Gas')

        plt.xlabel('Top 5 Nodes from Electricity and Gas Categories')
        plt.ylabel('Eigenvector Centrality')
        plt.title('Top 5 Nodes by Eigenvector Centrality')
        plt.grid(True)
        plt.legend()
        #plt.xticks([])  # Remove X-axis labels
        plt.xticks(range(len(combined_nodes)), [node.split(".")[0][1:] for node in combined_nodes])        
        plt.tight_layout()
        plt.show()
 """       
################ End of EigenVector Centrality ####################
        