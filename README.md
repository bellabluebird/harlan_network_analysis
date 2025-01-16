# harlan_network_analysis
this is a pipeline i designed as a part of my research fellowship to analyze, visualize, and statistically evaluate animal behavior data. this work is easily accessible, well-annotated, and can be repurposed for a variety of animal behavioral research. this builds upon the original statistical analysis performed by Dr. Santiago Menesnes in his time as my advisor.

this pipeline provides stats and visualizations to easily convey the social organization of colonies, behavioral patterns, and overall efficiency of their network structure. it is not perfect, but it's a great start for my own work in network analysis

## Files: 
### main.R:
tis is a high-level workflow manager, loading and executing various analysis functions while providing clear progress updates to the user. this is designed to be accessible to biologists and others looking to use the same analysis techniques. 
### functions_v5.
  this is the core analysis library, holding functions to perform the following tasks:  
  #### analyze_temporal_patterns
  reveals colony activity rhythms and peak interaction times, which can indicate optimal 
  foraging periods or colony-wide behavioral patterns
  #### analyze_interaction_types
  helps understand the social structure of the colony by showing which interactions are most 
  common and how they're distributed by counting interaction type, duration, and common pairs
  #### analyze_forager_patterns
  examines behavioral differences between forager and nurse ants using trip frequency analysis, food sharing patterns,
  and network position metrics
  #### analyze_ant_colony
  comprehensive analysis of the colony's social network structure using network density, reciprocity,
  path lengths, community detection, and centrality metrics
  #### compare_roles_statistically
  uses wilcoxon rank-sum tests to measure: 
    * Degree centrality (total connections)
    * In-degree (received interactions)
    * Out-degree (initiated interactions)
    * Betweenness centrality (bridge positions)
    * Closeness centrality (accessibility)
    * Eigenvector centrality (connection to important ants)
  this quantifies behavioral differences and validates classifications
  #### generate_community_stats
  analyzes strength + quantity of subgroup formations within the colony
  #### create_enhanced_visualization
  helps the user create statistically scaled network diagrams using role-based or community-based 
  coloring. it also creates temporal heat maps, and various plots to show interaction types and
  role-based differences.
  
### network_analysis_report.md:
the main output report illustrating the results of analyzing an ant colony's social network. the report reveals fascinating details about the colony's structure, including the identification of 67 ants (11 foragers and 56 nurses) and their interaction patterns. it provides valuable insights into how the colony is organized and how different types of ants interact with each other.

### role_comparison_statistics.md:
a statistical analysis report comparing the behaviors and network positions of forager and nurse ants. the purpose of this document is to provide statistical data about the significance of any observed differences between the groups; based on the  this report, we would need more data in order to prove that observed differences were significant.

### Bella_castaneus.csv:
this is the raw preliminary data file containing 1,438 observations of ant interactions. it describes the entirety of all interactions in a colony over a 20 minute observation period; more data would be neccessary to validate any conclusions drawn from this research. i produced this data through the course of my summer fellowship. 
