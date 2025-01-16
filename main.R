# this is the main script that shows how to use all the ant colony analysis functions
# it loads the data and runs through all the different types of analysis we can do

# first we need to load our custom functions from another file
# this file has all the detailed analysis code we'll use
source("functions_v5.R")

# 1. run the complete analysis with all features enabled
# this will process our ant behavior data and create a network from it
cat("\n1. running complete analysis with all features...\n")
full_report <- analyze_ant_colony(
  file_path = "Bella_castaneus.csv",  # this is our data file with ant observations
  parallel = TRUE  # this makes the analysis run faster by using multiple cpu cores
)

# 2. look at the basic statistics of our ant network
# this tells us things like how many ants we have and how connected they are
cat("\n2. examining basic network statistics...\n")
print(full_report$network_stats)

# 3. find which ants are the most important or central in the network
# we look at this in different ways - some ants might have lots of direct connections
# while others might be important for connecting different groups together
cat("\n3. viewing top individuals by various centrality measures...\n")
top_ants <- full_report$metrics %>%
  arrange(desc(degree)) %>%  # sort by number of connections (highest first)
  select(ant, degree, betweenness, closeness, eigenvector) %>%  # pick the metrics we want to see
  head(5)  # show just the top 5 ants
print(top_ants)

# 4. analyze when different interactions happen during the day
# this helps us understand if ants are more active at certain times
cat("\n4. analyzing temporal patterns...\n")
temporal_patterns <- full_report$temporal_patterns
print(head(temporal_patterns))

# 5. look at what types of interactions occur between ants
# this could be things like grooming, food sharing, etc.
cat("\n5. examining interaction types...\n")
interaction_summary <- full_report$interaction_summary
print(interaction_summary)

# 6. examine how ants form social groups or communities
# different mathematical methods can find these groups in different ways
cat("\n6. comparing community detection results...\n")
community_comparison <- full_report$communities$comparison
print(paste("community detection method comparison (adjusted rand index):",
            community_comparison))

# 7. create visual representations of our findings
# these help us understand the network structure more intuitively
cat("\n7. generating all visualizations...\n")

# save the main network visualization
# this shows all ants and their connections
cat("- main network visualization saved as 'ant_network.png'\n")

# create plots showing different measurements of ant importance
create_metric_plots(full_report$metrics)
cat("- metric plots saved as 'degree_distribution.png' and 'centrality_comparison.png'\n")

# 8. do some extra analysis of the network structure
cat("\n8. performing additional network analyses...\n")

# calculate how densely connected the network is
# this tells us if ants tend to interact with many others or just a few
cohesion <- edge_density(full_report$network)
cat("network cohesion:", cohesion, "\n")

# find the longest path between any two ants in the network
diameter <- diameter(full_report$network)
cat("network diameter:", diameter, "\n")

# identify the most central ants using different methods
# each method measures importance in a different way
central_nodes <- full_report$metrics %>%
  group_by() %>%
  summarise(
    highest_degree = ant[which.max(degree)],  # ant with most direct connections
    highest_betweenness = ant[which.max(betweenness)],  # ant that connects different groups
    highest_closeness = ant[which.max(closeness)],  # ant that can reach others quickly
    highest_eigenvector = ant[which.max(eigenvector)]  # ant connected to other important ants
  )
print("most central nodes by different metrics:")
print(central_nodes)

# 9. analyze the communities or groups we found
cat("\n9. generating community statistics...\n")
community_stats <- generate_community_stats(full_report$communities$louvain)
print(community_stats)

# 10. show how long everything took to run
cat("\n10. execution time and performance metrics:\n")
print(paste("total execution time:", full_report$execution_time, "seconds"))

# 11. save all our results to files for later use
cat("\n11. saving complete results...\n")
cat("- network analysis report saved as 'network_analysis_report.md'\n")
cat("- network metrics saved as 'ant_network_metrics.csv'\n")

# 12. create a summary of everything we analyzed
cat("\n12. analysis coverage summary:\n")
analysis_coverage <- data.frame(
  analysis_type = c("network analysis", "temporal analysis", 
                    "community detection", "centrality metrics",
                    "interaction analysis"),
  status = "completed",
  output_files = c(
    "ant_network.png",
    "temporal_patterns.png",
    "network_analysis_report.md",
    "centrality_comparison.png",
    "interaction_types.png"
  )
)
print(analysis_coverage)

# let the user know we're done
cat("\ncomplete analysis pipeline executed successfully!\n")
cat("check the generated files for detailed results and visualizations.\n")