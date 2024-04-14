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

## Citation
If you use the code or data in this repository in your work, please cite the following paper:

Kazim, M., Pirim, H., Shi, S., & Wu, D. (2024). Multilayer Analysis of Energy Networks. *Sustainable Energy, Grids and Networks*. Available at: https://www.sciencedirect.com/science/article/pii/SxxxxxxxXxxxxxxxxx

