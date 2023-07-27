import pandas as pd
import networkx as nx
from networkx_robustness import networkx_robustness
import matplotlib.pyplot as plt

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
        weight = row['Weight']
        graph.add_edge((from_node, from_layer), (to_node, to_layer), weight=weight)

    return graph

def simulate_attack(attack, graph, attack_fraction=0.1, weight='weight'):
    """
    Simulate attack on a network
    :param attack: attack method (random, degree, betweenness, closeness, eigenvector)
    :param graph: networkx graph
    :param attack_fraction: fraction of nodes to be attacked (default: 0.1)
    :param weight: weight of edges (default: weight)
    :return: initial (float), frac (list), apl (list)
    """
    if attack == "random":
        attack_type = "Random"
        initial, frac, apl = networkx_robustness.simulate_random_attack(G=graph, attack_fraction=attack_fraction, weight=weight)

    elif attack == "degree":
        attack_type = "Degree"
        initial, frac, apl = networkx_robustness.simulate_degree_attack(G=graph, attack_fraction=attack_fraction, weight=weight)

    elif attack == "betweenness":
        attack_type = "Betweenness"
        initial, frac, apl = networkx_robustness.simulate_betweenness_attack(G=graph, attack_fraction=attack_fraction, weight=weight)

    elif attack == "closeness":
        attack_type = "Closeness"
        initial, frac, apl = networkx_robustness.simulate_closeness_attack(G=graph, attack_fraction=attack_fraction, weight=weight)

    elif attack == "eigenvector":
        attack_type = "Eigenvector"
        initial, frac, apl = networkx_robustness.simulate_eigenvector_attack(G=graph, attack_fraction=attack_fraction, weight=weight)

    else:
        raise ValueError("Invalid attack method. Available options are: random, degree, betweenness, closeness, eigenvector")

    file_name = graph.name.split('/')[-1]
    return attack_type, file_name, initial, frac, apl
    
def plot_attack_results(frac, apl, attack_type, file_name):
    # Plot fraction of nodes removed
    plt.figure(figsize=(10, 6))
    attack_steps = list(range(1, len(frac) + 1))
    plt.plot(attack_steps, frac, marker='o', linestyle='-', color='b')
    plt.xlabel('Attack Step')
    plt.ylabel('Fraction of Nodes Removed')
    plt.title(f'{attack_type} Attack on {file_name}')
    plt.grid(True)
    plt.show()

    # Plot average path length after attack
    plt.figure(figsize=(10, 6))
    plt.plot(attack_steps, apl, marker='o', linestyle='-', color='r')
    plt.xlabel('Attack Step')
    plt.ylabel('Average Path Length')
    plt.title(f'{attack_type} Attack on {file_name}')
    plt.grid(True)
    plt.show()

def get_short_file_name(file_path):
    # Extract the filename from the file path
    return file_path.split("/")[-1]

if __name__ == "__main__":
    excel_files = [
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/1.edgelist_default.xlsx",
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/2.HORTA Electric Demand OFF.xlsx",
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/3.NO166,No167 Gas Valve OFF.xlsx",
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/4.HORTA, NO166NO167 OFF.xlsx",
        "/Volumes/Data/NDSU/PhD Work/Research/IME Research/AI-Energy/5.CHAMPION, NO45NO52 OFF.xlsx",
    ]

    # Nested dictionary to specify attacks and operations for each file
    attacks_and_operations = {
        "1.edgelist_default.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        },
        "2.HORTA Electric Demand OFF.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        },
        "3.NO166,No167 Gas Valve OFF.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        },
        "4.HORTA, NO166NO167 OFF.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        },
        "5.CHAMPION, NO45NO52 OFF.xlsx": {
            "attacks": ["random", "degree", "betweenness", "closeness", "eigenvector"],
            "operations": ["molloy_reed", "critical_threshold"]
        }
    }
    # Specify the target file and attacks/operations
    target_file = "3.NO166,No167 Gas Valve OFF.xlsx"  # Leave empty to apply to all files
    target_attacks = ["random"]  # Leave empty to apply all attacks
    target_operations = ["critical_threshold"]  # Leave empty to apply all operations

    for file in excel_files:
     if not target_file or get_short_file_name(target_file) == get_short_file_name(file):
        graph = create_graph_from_excel(file)

        for attack in attacks_and_operations[get_short_file_name(file)]["attacks"]:
            if not target_attacks or attack in target_attacks:
                # Perform the specified attack and get the results
                attack_type, file_name, initial, frac, apl = simulate_attack(attack, graph, attack_fraction=0.1, weight='weight')
                print(f"{attack_type} Attack on {file_name}:")
                print(f"Initial attack size: {initial}")
                print(f"Fraction of nodes removed: {frac}")
                print(f"Average path length after attack: {apl}")
                print("\n")

                # Add code to plot the results only for the target attack
                if attack in target_attacks:
                    # Get the shortened file name
                    short_file_name = get_short_file_name(file)
                    # Modify the plot title to include the shortened file name
                    plot_attack_results(frac, apl, attack_type=attack_type, file_name=short_file_name)

        for operation in attacks_and_operations[get_short_file_name(file)]["operations"]:
            if not target_operations or operation in target_operations:
                # Perform the specified operation
                if operation == "molloy_reed":
                    molloy_reed = networkx_robustness.molloy_reed(G=graph)
                    print(f"Molloy-Reed criterion for {file_name}: {molloy_reed}")

                if operation == "critical_threshold":
                    critical_threshold = networkx_robustness.critical_threshold(G=graph)
                    print(f"Critical threshold for {file_name}: {critical_threshold}")

                print("\n")