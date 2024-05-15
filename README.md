# Multilayer Analysis of Energy Networks

## Description
This repository contains the supporting code and data for the research paper titled "Multilayer Analysis of Energy Networks." The study introduces multilayer and multiplex network analysis of Belgium's electricity and gas systems, addressing the complex interdependence inherent in such infrastructures. It evaluates resilience through random and targeted disruptions and assesses robustness using structural integrity metrics.

## Repository Structure
- `codes/`
  - `multilayer_code/` : Contains scripts for the centrality and robustness metrics applied to the multilayer network model.
  - `multiplex_code/` : Contains scripts for the centrality and robustness metrics applied to the multiplex network model.
    
- `data/` : Contains datasets used for constructing the multilayer and multiplex networks.
  - `multilayer_data/` : Contains the data for the multilayer network model.
  - `multiplex_data/` : Contains the data for the multiplex network model.
    
- `figures/` : Generated figures and visualizations from the analysis.
   - `ML/` : Contains the network and centrality metrics figures applied to the multilayer network model.
   - `MP/` : Contains the network and centrality metrics figures applied to the multiplex network model.
     
## Centrality and Robustness Metrics
Centrality metrics include Closeness, Degree, Eigenvector, PageRank, and Random Walk centrality measures. Robustness evaluations involve simulations of random and targeted attacks and analysis of network navigability through coverage evolution metrics.

## Usage
Each script in the `codes/` directory is tailored for specific network analysis as outlined in our study. Run the scripts within their respective folders to replicate the analysis or apply it to modified data for comparative studies.

## Data Description
The data folder includes adjacency matrices and necessary attributes for nodes and layers relevant to the constructed multilayer and multiplex networks, reflecting scenarios of network disruptions and operational conditions.

## Figures
Visualizations corresponding to the analyses, such as centrality distributions, robustness assessments, and attack simulations, are stored in the `figures/` directory. These illustrate key findings and support the conclusions drawn in the paper.

## Contribution
Contributions to this repository are welcome. Please fork the repository and submit a pull request with your suggested changes.

## License
The code and data are provided under an open license for academic and non-commercial use.

### Citation

Muhammad Kazim, Harun Pirim, Shuomang Shi, Di Wu, "Multilayer analysis of energy networks," *Sustainable Energy, Grids and Networks*, vol. 39, 2024, 101407, ISSN 2352-4677, [https://doi.org/10.1016/j.segan.2024.101407](https://doi.org/10.1016/j.segan.2024.101407). Available at: [https://www.sciencedirect.com/science/article/pii/S235246772400136X](https://www.sciencedirect.com/science/article/pii/S235246772400136X).

**Abstract:**
Modern interconnected and vulnerable energy infrastructures highlight the critical need for resilience and robustness to maintain the functioning of modern societies amidst growing environmental and operational challenges. This study introduces both multilayer and multiplex network analysis of Belgium’s electricity and gas systems, addressing the complex interdependence inherent in such infrastructure. The resilience is evaluated through random and targeted disruptions, while the robustness is assessed by employing the Molloy–Reed criteria for structural integrity and setting minimum operational thresholds for continuity. Multiplex layer similarities are assessed and subnetworks are identified. The coverage evolution metric quantifies the network’s adaptability under various disruption scenarios. Belgium’s energy infrastructure is used as a case study to uncover critical nodes from different centrality perspectives whose stability is essential for overall network resilience and identify vulnerabilities to disruptions, whether random or targeted. Our insights into the structural robustness, cluster cohesion, and Navigability of energy networks pave the way for more informed decisions in energy infrastructure planning and complex systems management, offering valuable lessons that can be applied globally.

**Keywords:** Multiplex networks, Multilayer networks, Centrality measures, Network robustness, Energy networks, Complex systems, Navigability


