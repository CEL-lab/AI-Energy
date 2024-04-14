import networkx as nx
import random

def simulate_random_attack(G=None, attack_fraction=0.1, weight=None):
    """
    Simulate random attack on a network
    :param G: networkx graph
    :param attack_fraction: fraction of nodes to be attacked (default: 0.1)
    :param weight: weight of edges (default: None)
    :return: initial (float), frac (list), apl (list), removed_nodes (list)
    """
    # copy the graph to avoid changing the original graph
    G = G.copy()
    # get the number of nodes
    G_nodes = G.number_of_nodes()
    # get the largest connected component of G
    if G.is_directed():
        Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
    else:
        Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
    # get the number of nodes in the largest connected component
    Gc_nodes = Gc.number_of_nodes()
    # get the initial fraction of nodes in the largest connected component
    initial = Gc_nodes / G_nodes
    # initialize lists
    frac = []
    apl = []
    removed_nodes = []  # List to store removed nodes at each iteration
    # simulate random attack
    for i in range(0, int(G_nodes * attack_fraction)):
        removed_node = random.choice(list(G.nodes()))
        G.remove_node(removed_node)
        removed_nodes.append(removed_node)
        # get the largest connected component of G
        if G.is_directed():
            Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
        else:
            Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
        # get the number of nodes in the largest connected component
        Gc_nodes = Gc.number_of_nodes()
        # get the fraction of nodes in the largest connected component
        frac.append(Gc_nodes / G_nodes)
        # get the average path length of the largest connected component
        apl.append(nx.average_shortest_path_length(Gc, weight=weight))

    return initial, frac, apl, removed_nodes

def simulate_degree_attack(G=None, attack_fraction=0.1, weight=None):
    
    # Copy the graph to avoid changing the original graph
    G = G.copy()
    # Get the number of nodes
    G_nodes = G.number_of_nodes()
    # Get the largest connected component of G
    if G.is_directed():
        Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
    else:
        Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
    # Get the number of nodes in the largest connected component
    Gc_nodes = Gc.number_of_nodes()
    # Get the initial fraction of nodes in the largest connected component
    initial = Gc_nodes / G_nodes
    # Initialize lists
    frac = []
    apl = []
    removed_nodes = []  # List to store removed nodes at each iteration
    # Get the degree of each node
    degree = nx.degree_centrality(G)
    # Sort the nodes by degree
    degree = sorted(degree, key=degree.get, reverse=True)
    # Simulate degree attack
    for i in range(0, int(G_nodes * attack_fraction)):
        # Remove the node with the highest degree and store it in removed_nodes
        removed_node = degree[i]
        G.remove_node(removed_node)
        removed_nodes.append(removed_node)
        # Get the largest connected component of G
        if G.is_directed():
            Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
        else:
            Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
        # Get the number of nodes in the largest connected component
        Gc_nodes = Gc.number_of_nodes()
        # Get the fraction of nodes in the largest connected component
        frac.append(Gc_nodes / G_nodes)
        
        # Get the average path length of the largest connected component
        apl.append(nx.average_shortest_path_length(Gc, weight=weight))
        # Sort the list of average path lengths in descending order
        apl.sort(reverse=False)  

    return initial, frac, apl, removed_nodes
    

def simulate_betweenness_attack(G=None, attack_fraction=0.1, weight=None, normalized=True, k=None, seed=None, endpoints=False):
    """
    Simulate betweenness attack on a network
    :param G: networkx graph
    :param attack_fraction: fraction of nodes to be attacked (default: 0.1)
    :param weight: weight of edges (default: None)
    :param normalized: if True, betweenness is normalized by 2/((n-1)(n-2)) for graphs, and 1/((n-1)(n-2)) for directed graphs where n is the number of nodes in G (default: True)
    :param k: use k node samples to estimate betweenness (default: None)
    :param seed: seed for random number generator (default: None)
    :param endpoints: If True include the endpoints in the shortest path counts (default: False)
    :return: initial (float), frac (list), apl (list), removed_nodes (list)
    """
    # copy the graph to avoid changing the original graph
    G = G.copy()
    # get the  number of nodes
    G_nodes = G.number_of_nodes()
    # get the largest connected component of G
    if G.is_directed():
        Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
    else:
        Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
    # get the number of nodes in the largest connected component
    Gc_nodes = Gc.number_of_nodes()
    # get the initial fraction of nodes in the largest connected component
    initial = Gc_nodes / G_nodes
    # initialize lists
    frac = []
    apl = []
    removed_nodes = []  # List to store removed nodes at each iteration
    
    # get the betweenness of each node
    betweenness = nx.betweenness_centrality(G, weight=weight, normalized=normalized, k=k, seed=seed, endpoints=endpoints)
    # sort the nodes by betweenness
    betweenness = sorted(betweenness, key=betweenness.get, reverse=True)
    # simulate betweenness attack
    for i in range(0, int(G_nodes * attack_fraction)):
        # remove the node with the highest betweenness
        removed_node = betweenness[i]
        G.remove_node(removed_node)
        removed_nodes.append(removed_node)
        # get the largest connected component of G
        if G.is_directed():
            Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
        else:
            Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
        # get the number of nodes in the largest connected component
        Gc_nodes = Gc.number_of_nodes()
        # get the fraction of nodes in the largest connected component
        frac.append(Gc_nodes / G_nodes)
        # get the average path length of the largest connected component
        apl.append(nx.average_shortest_path_length(Gc, weight=weight))

    return initial, frac, apl, removed_nodes

def simulate_closeness_attack(G=None, attack_fraction=0.1, weight=None, u=None, wf_improved=True):
    """
    Simulate closeness attack on a network
    :param G: networkx graph
    :param attack_fraction: fraction of nodes to be attacked (default: 0.1)
    :param weight: weight of edges (default: None)
    :param u: node for which closeness is to be computed (default: None)
    :param wf_improved: use of the improved algorithm to scale by the fraction of nodes reachable (default: True)
    :return: initial (float), frac (list), apl (list), removed_nodes (list)
    """
    # copy the graph to avoid changing the original graph
    G = G.copy()
    # get the number of nodes
    G_nodes = G.number_of_nodes()
    # get the largest connected component of G
    if G.is_directed():
        Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
    else:
        Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
    # get the number of nodes in the largest connected component
    Gc_nodes = Gc.number_of_nodes()
    # get the initial fraction of nodes in the largest connected component
    initial = Gc_nodes / G_nodes
    # initialize lists
    frac = []
    apl = []
    removed_nodes = []  # List to store removed nodes at each iteration
    # get the closeness of each node
    closeness = nx.closeness_centrality(G, distance=weight, u=u, wf_improved=wf_improved)
    # sort the nodes by closeness
    closeness = sorted(closeness, key=closeness.get, reverse=True)
    # simulate closeness attack
    for i in range(0, int(G_nodes * attack_fraction)):
        # remove the node with the highest closeness
        removed_node = closeness[i]
        G.remove_node(removed_node)
        removed_nodes.append(removed_node)
        # get the largest connected component of G
        if G.is_directed():
            Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
        else:
            Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
        # get the number of nodes in the largest connected component
        Gc_nodes = Gc.number_of_nodes()
        # get the fraction of nodes in the largest connected component
        frac.append(Gc_nodes / G_nodes)
        # get the average path length of the largest connected component
        apl.append(nx.average_shortest_path_length(Gc, weight=weight))

    return initial, frac, apl, removed_nodes

def simulate_eigenvector_attack(G=None, attack_fraction=0.1, weight=None, tol=1e-06, max_iter=100, nstart=None):
    """
    Simulate eigenvector attack on a network
    :param G: networkx graph
    :param attack_fraction: fraction of nodes to be attacked (default: 0.1)
    :param weight: weight of edges (default: None)
    :param tol: tolerance for the power iteration method (default: 1e-06)
    :param max_iter: maximum number of iterations for the power iteration method (default: 100)
    :param nstart: initial vector for the power iteration method (default: None)
    :return: initial (float), frac (list), apl (list), removed_nodes (list)
    """
    # copy the graph to avoid changing the original graph
    G = G.copy()
    # get the number of nodes
    G_nodes = G.number_of_nodes()
    # get the largest connected component of G
    if G.is_directed():
        Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
    else:
        Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
    # get the number of nodes in the largest connected component
    Gc_nodes = Gc.number_of_nodes()
    # get the initial fraction of nodes in the largest connected component
    initial = Gc_nodes / G_nodes
    # initialize lists
    frac = []
    apl = []
    removed_nodes = []  # List to store removed nodes at each iteration
    # get the eigenvector of each node
    eigenvector = nx.eigenvector_centrality(G, weight=weight, tol=tol, max_iter=max_iter, nstart=nstart)
    # sort the nodes by eigenvector
    eigenvector = sorted(eigenvector, key=eigenvector.get, reverse=True)
    # simulate eigenvector attack
    for i in range(0, int(G_nodes * attack_fraction)):
        # remove the node with the highest eigenvector
        removed_node = eigenvector[i]
        G.remove_node(removed_node)
        removed_nodes.append(removed_node)
        # get the largest connected component of G
        if G.is_directed():
            Gc = G.to_undirected().subgraph(sorted(nx.connected_components(G.to_undirected()), key=len, reverse=True)[0])
        else:
            Gc = G.subgraph(sorted(nx.connected_components(G), key=len, reverse=True)[0])
        # get the number of nodes in the largest connected component
        Gc_nodes = Gc.number_of_nodes()
        # get the fraction of nodes in the largest connected component
        frac.append(Gc_nodes / G_nodes)
        # get the average path length of the largest connected component
        apl.append(nx.average_shortest_path_length(Gc, weight=weight))

    return initial, frac, apl, removed_nodes


def molloy_reed(G=None):
    """
    Compute the Molloy-Reed criterion for a network
    :param G: networkx graph
    :return: Molloy-Reed criterion
    """
    # get the average squared degree
    avg_sq_degree = sum([d ** 2 for n, d in G.degree()]) / G.number_of_nodes()
    # get the average degree
    avg_degree = sum([d for n, d in G.degree()]) / G.number_of_nodes()
    # compute the Molloy-Reed criterion
    molloy_reed = avg_sq_degree/avg_degree

    return molloy_reed

def critical_threshold(G=None):
    """
    Compute the critical threshold for a network
    :param G: networkx graph
    :return: critical threshold
    """
    # get the average squared degree
    avg_sq_degree = sum([d ** 2 for n, d in G.degree()]) / G.number_of_nodes()
    # get the average degree
    avg_degree = sum([d for n, d in G.degree()]) / G.number_of_nodes()
    # compute the Molloy-Reed criterion
    molloy_reed = avg_sq_degree/avg_degree
    # compute the critical threshold
    critical_threshold = 1 - (1/(molloy_reed-1))
    return critical_threshold

def simulate_k_core_attack(G, attack_fraction=0.1):
    """
    Simulate k-core attack on a network by removing the most influential nodes
    based on their k-core values, in descending order.
    
    :param G: networkx graph
    :param attack_fraction: fraction of nodes to be attacked (default: 0.1)
    :return: initial (float), frac (list), apl (list), removed_nodes (list)
    """
    # Copy the graph to avoid changing the original graph
    G = G.copy()
    # Calculate the initial size and the largest connected component
    initial_size = len(G)
    largest_cc = max(nx.connected_components(G), key=len) if not G.is_directed() else max(nx.strongly_connected_components(G), key=len)
    initial = len(largest_cc) / initial_size

    frac = []  # Fraction of nodes in the largest connected component
    apl = []   # Average path length
    removed_nodes = []  # Track removed nodes

    # Calculate k-core for each node
    k_cores = nx.core_number(G)
    # Order nodes by their k-core value in descending order
    nodes_by_k_core = sorted(k_cores, key=k_cores.get, reverse=True)

    # Determine the number of nodes to attack
    num_attack = int(attack_fraction * initial_size)

    for i in range(num_attack):
        if nodes_by_k_core:
            # Remove the node with the highest k-core value
            node_to_remove = nodes_by_k_core.pop(0)
            G.remove_node(node_to_remove)
            removed_nodes.append(node_to_remove)

            # Recalculate the largest connected component and its size
            if G:
                largest_cc = max(nx.connected_components(G), key=len) if not G.is_directed() else max(nx.strongly_connected_components(G), key=len)
                new_size = len(largest_cc)
                frac.append(new_size / initial_size)

                # Calculate the average path length if the largest CC has more than one node
                if new_size > 1:
                    try:
                        G_sub = G.subgraph(largest_cc)
                        apl_value = nx.average_shortest_path_length(G_sub)
                        apl.append(apl_value)
                    except nx.NetworkXError:
                        apl.append(float('inf'))
                else:
                    apl.append(float('inf'))
            else:
                break

    return initial, frac, apl, removed_nodes

