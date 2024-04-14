import matplotlib.pyplot as plt

# Community data
community_data = {
    "Community 01": {"type": "gas", "total_nodes": 34, "id_range": (91, 151)},
    "Community 02": {"type": "gas", "total_nodes": 23, "id_range": (134, 214)},
    "Community 03": {"type": "gas", "total_nodes": 23, "id_range": (76, 102)},
    "Community 04": {"type": "gas", "total_nodes": 23, "id_range": (149, 218)},
    "Community 05": {"type": "electricity", "total_nodes": 22, "id_range": (1, 75)},
    "Community 06": {"type": "gas", "total_nodes": 17, "id_range": (171, 210)},
    "Community 07": {"type": "electricity", "total_nodes": 16, "id_range": (3, 74)},
    "Community 08": {"type": "electricity", "total_nodes": 15, "id_range": (5, 69)},
    "Community 09": {"type": "gas", "total_nodes": 14, "id_range": (123, 222)},
    "Community 10": {"type": "gas", "total_nodes": 13, "id_range": (129, 167)},
    "Community 11": {"type": "electricity", "total_nodes": 12, "id_range": (16, 67)},
    "Community 12": {"type": "electricity", "total_nodes": 10, "id_range": (2, 46)},
}

# Creating the bar chart
fig, ax = plt.subplots(figsize=(14, 8))

# Creating bars and annotations for each community
for i, (community, data) in enumerate(community_data.items()):
    # Determine bar color based on the type of nodes
    color = 'orange' if data["type"] == "gas" else 'red'
    bar_label = 'Gas Nodes' if data["type"] == "gas" else 'Electricity Nodes'

    # Create the bar for the community
    ax.bar(i, data["id_range"][1], width=0.4, color=color, label=bar_label if i == 0 else "")

    # Annotate the bar with the total number of nodes
    annotation = f'{data["total_nodes"]} nodes'
    ax.text(i, data["id_range"][1], annotation, ha='center', va='bottom', fontsize=8)

# Set x-axis labels with community names and node ID ranges
ax.set_xticks(range(len(community_data)))
ax.set_xticklabels([f"{community}\nID Range: {data['id_range'][0]}-{data['id_range'][1]}" for community, data in community_data.items()], rotation=45, ha='center')

# Set y-axis label
ax.set_ylabel('Node IDs')

# Add a legend for both gas and electricity
legend_labels = ['Gas Nodes', 'Electricity Nodes']
legend_colors = ['orange', 'red']
handles = [plt.Rectangle((0,0),1,1, color=legend_colors[i]) for i in range(2)]
ax.legend(handles, legend_labels)

# Show the plot
plt.tight_layout()
plt.show()
