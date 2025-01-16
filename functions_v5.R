# import required libraries for network analysis, data manipulation, and visualization
library(igraph)        # network creation and analysis
library(ggplot2)       # data visualization
library(dplyr)         # data manipulation and transformation
library(tidyr)         # data cleaning and reshaping
library(RColorBrewer)  # color palettes for visualization
library(networkD3)     # interactive network visualizations
library(lubridate)     # date and time handling
library(parallel)      # parallel processing for performance
library(scales)        # better formatting for plot scales

# analyze when interactions occur throughout the day
analyze_temporal_patterns <- function(data) {
  # create datetime objects and extract hour and day information
  data <- data %>%
    mutate(
      datetime = ymd_hms(paste(Date, Time)),
      hour = hour(datetime),
      day_of_week = wday(datetime, label = TRUE)
    )
  
  # calculate the number of interactions and unique ants per hour
  hourly_activity <- data %>%
    group_by(hour) %>%
    summarise(
      interactions = n(),
      unique_individuals = n_distinct(c(Individual, With))
    )
  
  # create a line plot showing activity patterns throughout the day
  temporal_plot <- ggplot(hourly_activity, aes(x = hour, y = interactions)) +
    geom_line(color = "blue", size = 1) +
    geom_point(aes(size = unique_individuals), alpha = 0.6) +
    theme_minimal() +
    scale_x_continuous(breaks = 0:23) +
    labs(title = "When Ants Interact Throughout the Day",
         x = "Hour of Day",
         y = "Number of Interactions",
         size = "Number of Unique Ants")
  
  # save the plot and return the hourly data
  ggsave("temporal_patterns.png", temporal_plot, width = 10, height = 6)
  return(hourly_activity)
}

# analyze different types of interactions between ants
analyze_interaction_types <- function(data) {
  # summarize the frequency and characteristics of different interaction types
  interaction_summary <- data %>%
    group_by(Event) %>%
    summarise(
      count = n(),
      unique_pairs = n_distinct(paste(Individual, With)),
      avg_duration = mean(Duration, na.rm = TRUE)
    ) %>%
    arrange(desc(count))
  
  # create a bar plot showing the frequency of different interaction types
  interaction_plot <- ggplot(interaction_summary, 
                             aes(x = reorder(Event, count), y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Types of Ant Interactions",
         x = "What the Ants Did",
         y = "How Many Times")
  
  # save the plot and return the summary data
  ggsave("interaction_types.png", interaction_plot, width = 10, height = 6)
  return(interaction_summary)
}

# analyze behavioral differences between forager and nurse ants
analyze_forager_patterns <- function(complete_data, network_data, network, foragers) {
  # Use complete data for location analysis
  complete_data <- complete_data %>%
    mutate(Location = ifelse(is.na(Location) | Location == "", "Inside", Location))
  
  forager_trips <- complete_data %>%
    filter(Individual %in% foragers) %>%
    group_by(Individual, Location) %>%
    summarise(visits = n(), .groups = 'drop') %>%
    pivot_wider(names_from = Location, 
                values_from = visits, 
                values_fill = 0) %>%
    mutate(
      percent_outside = Outside / (Outside + Inside) * 100
    )
  
  # Use network data for interaction analysis
  food_sharing <- network_data %>%
    filter(grepl("sharing|food|trophallaxis", Event, ignore.case = TRUE)) %>%
    mutate(
      is_forager = Individual %in% foragers,
      role = ifelse(is_forager, "Forager", "Nurse")
    ) %>%
    group_by(role) %>%
    summarise(
      n_individuals = n_distinct(Individual),
      total_sharing = n(),
      sharing_per_ant = total_sharing/n_individuals,
      .groups = 'drop'
    )
  
  sharing_direction <- network_data %>%
    filter(grepl("sharing|food|trophallaxis", Event, ignore.case = TRUE)) %>%
    mutate(
      giver_role = ifelse(Individual %in% foragers, "Forager", "Nurse"),
      receiver_role = ifelse(With %in% foragers, "Forager", "Nurse")
    ) %>%
    group_by(giver_role, receiver_role) %>%
    summarise(
      count = n(),
      .groups = 'drop'
    )
  
  role_metrics <- data.frame(
    ant = V(network)$name,
    role = ifelse(V(network)$name %in% foragers, "Forager", "Nurse"),
    degree = degree(network),
    betweenness = betweenness(network),
    closeness = closeness(network)
  ) %>%
    group_by(role) %>%
    summarise(
      n_ants = n(),
      avg_connections = mean(degree),
      avg_betweenness = mean(betweenness),
      avg_closeness = mean(closeness),
      .groups = 'drop'
    )
  
  return(list(
    foragers = foragers,
    forager_trips = forager_trips,
    food_sharing = food_sharing,
    sharing_direction = sharing_direction,
    role_metrics = role_metrics
  ))
}

# create and save a network visualization
create_enhanced_visualization <- function(network, metrics, foragers = NULL) {
  # create a high-resolution png file
  png("ant_network.png", width = 3000, height = 3000, res = 300)
  
  # calculate network layout with enhanced parameters for better visualization
  layout <- layout_with_fr(
    network,
    niter = 5000,           # increase iterations for better layout
    area = vcount(network)^5,  # increase area to spread nodes
    repulserad = vcount(network)^5.5,  # increase repulsion between nodes
    coolexp = 0.99,  # slower cooling for more stable layout
    start.temp = vcount(network) * 100  # higher starting temperature
  ) * 100  # additional scaling
  
  # normalize layout coordinates
  layout <- norm_coords(layout, ymin=-1, ymax=1, xmin=-1, xmax=1) * 0.8
  
  # set visual parameters based on network metrics
  V(network)$size <- sqrt(degree(network)) * 3
  E(network)$width <- log1p(E(network)$weight) * 0.5
  E(network)$arrow.size <- 0.3
  V(network)$label.dist <- 1.5
  V(network)$label.cex <- 0.8
  
  # color nodes based on forager status or community membership
  if (!is.null(foragers)) {
    vertex_colors <- ifelse(V(network)$name %in% foragers,
                            brewer.pal(9, "Set1")[1],
                            brewer.pal(9, "Set1")[2])
  } else {
    community_colors <- colorRampPalette(brewer.pal(9, "Set3"))(max(V(network)$community))
    vertex_colors <- community_colors[V(network)$community]
  }
  
  # create the network plot
  plot(network,
       layout = layout,
       vertex.label = V(network)$name,
       vertex.label.color = "black",
       vertex.frame.color = "gray",
       edge.curved = 0.2,
       vertex.color = vertex_colors,
       main = "Ant Colony Social Network")
  
  # add appropriate legend based on visualization type
  if (!is.null(foragers)) {
    legend("bottomright",
           legend = c("Forager", "Nurse"),
           fill = vertex_colors[c(TRUE, FALSE)],
           title = "Ant Jobs",
           cex = 0.8)
  } else {
    legend("bottomright",
           legend = paste("Group", 1:max(V(network)$community)),
           fill = community_colors,
           title = "Communities",
           cex = 0.8)
  }
  
  dev.off()
}

# main function to perform complete network analysis
analyze_ant_colony <- function(file_path, parallel = TRUE) {
  start_time <- Sys.time()
  
  # read the data
  data <- read.csv(file_path)
  
  # validate columns but don't filter out NA interactions yet
  required_cols <- c("Individual", "With", "Event", "Date", "Time", "Location")
  if (!all(required_cols %in% colnames(data))) {
    missing_cols <- required_cols[!required_cols %in% colnames(data)]
    stop(paste("missing these columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # First identify foragers from the complete dataset
  foragers <- data %>%
    filter(!is.na(Location) & Location == "Outside") %>%
    pull(Individual) %>%
    unique()
  
  # Now clean the data for network analysis (removing NA interactions)
  network_data <- data %>%
    filter(!is.na(With) & With != "NA") %>%
    mutate(
      Individual = as.character(Individual),
      With = as.character(With),
      Duration = as.numeric(Duration)
    ) %>%
    filter(Individual != With)  # remove self-interactions
  
  # Create edge list for network construction
  edge_list <- network_data %>%
    filter(Event %in% c("Grooming", "Grooming*") | 
             grepl("sharing", tolower(Event))) %>%
    filter(!is.na(Individual) & !is.na(With)) %>%
    group_by(Individual, With) %>%
    summarise(
      weight = n(),
      avg_duration = mean(Duration, na.rm = TRUE),
      interaction_types = list(unique(Event)),
      .groups = 'drop'
    )
  
  # Create network object from edge list
  network <- graph_from_data_frame(
    edge_list,
    directed = TRUE,
    vertices = unique(c(edge_list$Individual, edge_list$With))
  )
  
  # Calculate various network metrics for each ant
  metrics <- data.frame(
    ant = V(network)$name,
    degree = degree(network),
    degree_in = degree(network, mode = "in"),
    degree_out = degree(network, mode = "out"),
    betweenness = betweenness(network),
    closeness = closeness(network),
    eigenvector = eigen_centrality(network)$vector,
    clustering_coef = transitivity(network, type = "local", vids = V(network))
  )
  
  # Identify communities in the network
  communities_louvain <- cluster_louvain(as.undirected(network))
  V(network)$community <- membership(communities_louvain)
  
  # Perform various analyses using the complete list of foragers
  forager_analysis <- analyze_forager_patterns(data, network_data, network, foragers)
  create_enhanced_visualization(network, metrics, forager_analysis$foragers)
  temporal_patterns <- analyze_temporal_patterns(network_data)
  interaction_summary <- analyze_interaction_types(network_data)
  
  # Calculate execution time
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Calculate overall network statistics
  network_stats <- list(
    n_vertices = vcount(network),
    n_edges = ecount(network),
    density = edge_density(network),
    reciprocity = reciprocity(network),
    diameter = diameter(network),
    average_path_length = average.path.length(network),
    clustering_coefficient = transitivity(network),
    community_modularity = modularity(communities_louvain),
    execution_time = execution_time
  )
  
  # Add statistical comparison
  statistical_analysis <- compare_roles_statistically(data, network_data, network, foragers)
  
  
  # Generate and save analysis report
  generate_analysis_report(network_stats, metrics, 
                           temporal_patterns, interaction_summary,
                           forager_analysis)
  
  # Add to the return list
  results <- list(
    network = network,
    metrics = metrics,
    network_stats = network_stats,
    temporal_patterns = temporal_patterns,
    interaction_summary = interaction_summary,
    forager_analysis = forager_analysis,
    communities = list(louvain = communities_louvain),
    statistical_tests = statistical_analysis,
    execution_time = execution_time
  )
  
  return(results)
}

# function to generate a comprehensive markdown report of the analysis
generate_analysis_report <- function(network_stats, metrics, 
                                     temporal_patterns, interaction_summary,
                                     forager_analysis) {
  # compile all analysis results into a structured report
  report <- c(
    "# Ant Colony Network Analysis Report",
    "\n## Network Statistics",
    paste("* Number of ants:", network_stats$n_vertices),
    paste("* Number of interactions:", network_stats$n_edges),
    paste("* Network density:", round(network_stats$density, 3)),
    paste("* Two-way interactions:", round(network_stats$reciprocity, 3)),
    paste("* Average steps between ants:", round(network_stats$average_path_length, 3)),
    paste("* Group formation:", round(network_stats$clustering_coefficient, 3)),
    
    # section for most connected ants
    "\n## Most Connected Ants",
    "### Top 5 by Total Connections:",
    capture.output(head(arrange(metrics, desc(degree)), 5)),
    "\n### Top 5 by Network Position (Betweenness):",
    capture.output(head(arrange(metrics, desc(betweenness)), 5)),
    
    # section for forager analysis
    "\n## Forager Analysis",
    paste("* Number of Foragers:", length(forager_analysis$foragers)),
    paste("* Number of Nurses:", network_stats$n_vertices - length(forager_analysis$foragers)),
    "\n### Forager Details:",
    "Ants that go outside (with outside trip frequency):",
    capture.output(arrange(forager_analysis$forager_trips, desc(percent_outside))),
    
    # section for food sharing analysis
    "\n### Food Sharing Patterns:",
    "Overall sharing by role:",
    capture.output(forager_analysis$food_sharing),
    "\nSharing directions (who gives to who):",
    capture.output(forager_analysis$sharing_direction),
    "\n### Network Position Comparison:",
    capture.output(forager_analysis$role_metrics)
  )
  
  # save the report as a markdown file
  writeLines(report, "network_analysis_report.md")
}

# function to perform statistical tests comparing foragers and nurses
compare_roles_statistically <- function(data, network_data, network, foragers) {
  # initialize list to store all test results
  test_results <- list()
  
  # 1. prepare network metrics for all ants
  ant_metrics <- data.frame(
    ant = V(network)$name,
    role = ifelse(V(network)$name %in% foragers, "Forager", "Nurse"),
    degree = degree(network),
    degree_in = degree(network, mode = "in"),
    degree_out = degree(network, mode = "out"),
    betweenness = betweenness(network),
    closeness = closeness(network),
    eigenvector = eigen_centrality(network)$vector
  )
  
  # 2. network Metrics Tests
  ## use Wilcoxon rank-sum test (Mann-Whitney U test) as we can't assume normal distribution
  network_metrics <- c("degree", "degree_in", "degree_out", "betweenness", "closeness", "eigenvector")
  network_tests <- lapply(network_metrics, function(metric) {
    formula <- as.formula(paste(metric, "~ role"))
    test <- wilcox.test(formula, data = ant_metrics)
    data.frame(
      metric = metric,
      statistic = test$statistic,
      p_value = test$p.value,
      significant = test$p.value < 0.05
    )
  })
  test_results$network_metrics <- do.call(rbind, network_tests)
  
  # 3. behavioral tests
  ## Interaction rates
  interaction_counts <- network_data %>%
    group_by(Individual) %>%
    summarise(
      total_interactions = n(),
      .groups = 'drop'
    ) %>%
    mutate(role = ifelse(Individual %in% foragers, "Forager", "Nurse"))
  
  test_results$interaction_rate <- wilcox.test(
    total_interactions ~ role, 
    data = interaction_counts
  )
  
  ## inside vs outside time for each group
  location_proportions <- data %>%
    group_by(Individual) %>%
    summarise(
      prop_outside = mean(Location == "Outside", na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(role = ifelse(Individual %in% foragers, "Forager", "Nurse"))
  
  test_results$location <- wilcox.test(
    prop_outside ~ role,
    data = location_proportions
  )
  
  # 4. specific interaction types
  interaction_types <- network_data %>%
    group_by(Individual, Event) %>%
    summarise(
      count = n(),
      .groups = 'drop'
    ) %>%
    mutate(role = ifelse(Individual %in% foragers, "Forager", "Nurse"))
  
  # test each interaction type separately
  unique_events <- unique(interaction_types$Event)
  event_tests <- lapply(unique_events, function(event_type) {
    event_data <- interaction_types %>%
      filter(Event == event_type)
    test <- wilcox.test(count ~ role, data = event_data)
    data.frame(
      event = event_type,
      statistic = test$statistic,
      p_value = test$p.value,
      significant = test$p.value < 0.05
    )
  })
  test_results$interaction_types <- do.call(rbind, event_tests)
  
  # 5. generate summary report
  report <- c(
    "# Statistical Comparison of Foragers vs Nurses",
    "\n## Network Metrics",
    capture.output(test_results$network_metrics),
    "\n## Behavioral Differences",
    paste("Interaction Rate p-value:", round(test_results$interaction_rate$p.value, 4)),
    paste("Location Preference p-value:", round(test_results$location$p.value, 4)),
    "\n## Interaction Types",
    capture.output(test_results$interaction_types)
  )
  
  # save report
  writeLines(report, "role_comparison_statistics.md")
  
  return(test_results)
}

# function to analyze and summarize community structure
generate_community_stats <- function(communities_louvain) {
  # calculate basic community statistics
  community_stats <- data.frame(
    algorithm = "Louvain",
    num_communities = length(unique(membership(communities_louvain))),
    modularity = modularity(communities_louvain)
  )
  
  # calculate the size of each community
  community_sizes <- table(membership(communities_louvain))
  
  # print detailed community information
  cat("\nCommunity Statistics:")
  cat("\nTotal number of communities:", community_stats$num_communities)
  cat("\nModularity score:", round(community_stats$modularity, 3))
  cat("\n\nCommunity sizes:")
  print(community_sizes)
  
  return(community_stats)
}

# run the complete analysis with error handling
tryCatch({
  # attempt to run the analysis on the specified file
  report <- analyze_ant_colony("Bella_castaneus.csv", parallel = TRUE)
}, error = function(e) {
  # catch and report any errors that occur during analysis
  message("Error in analysis: ", e$message)
}, warning = function(w) {
  # catch and report any warnings that occur during analysis
  message("Warning in analysis: ", w$message)
})