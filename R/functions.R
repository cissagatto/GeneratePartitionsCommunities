###############################################################################
# GENERATE PARTITIONS COMMUNITIES                                             #
#                                                                             #
#
# Copyright (C) 2025                                                          #
#                                                                             #
# This code is free software: you can redistribute it and/or modify it under  #
# the terms of the GNU General Public License as published by the Free        #
# Software Foundation, either version 3 of the License, or (at your option)   #
# any later version. This code is distributed in the hope that it will be     #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of      #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General    #
# Public License for more details.                                            #
#                                                                             #
# Profa. Dra. Elaine Cecilia Gatto                                            #
# Federal University of Lavras (UFLA) Campus Lavras - Minas Gerais            #
# Applied Computer Department (DAC)                                           #
#                                                                             #
# Prof. Dr. Ricardo Cerri
# State University of São Paulo Campus São Carlos
#                                                                             #
# Prof. Dr. Mauri Ferrandin
# Federal University of Santa Catarina Campus Blumenau
#                                                                             #
# Prof. Dr. Alan Demetrius                                                    #
# Federal University of Sao Carlos (UFSCar) Campus Sao Carlos - São Paulo     #
# Computer Department (DC)                                                    #
#
###############################################################################


##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/GeneratePartitionsCommunities"
FolderScripts = "~/GeneratePartitionsCommunities/R"



#' Run a Community Detection Method
#'
#' This function executes a specified community detection algorithm on a given graph
#' and measures its execution time.
#'
#' @param method_function A function that performs a community detection algorithm.
#'        It should accept `parameters`, `graph`, `title`, `fold`, and `Save` as arguments.
#' @param parameters A list of parameters required for the community detection method.
#'        It should contain at least the `FolderSplit` path where results will be saved.
#' @param graph An `igraph` object representing the network structure on which
#'        the community detection method will be applied.
#' @param fold An integer indicating the current fold number in a cross-validation
#'        or iterative process.
#'
#' @return A numeric vector of length 3 containing execution time statistics:
#'         - `user`: Time spent in user mode.
#'         - `system`: Time spent in system mode.
#'         - `elapsed`: Total elapsed time.
#'
#' @examples
#' \dontrun{
#' parameters <- list(FolderSplit = "results/fold1")
#' graph <- igraph::make_ring(10)
#' result <- run_community_method(executeLouvain, parameters, graph, 1)
#' print(result)
#' }
#'
#' @export
run_community_method <- function(method_function,
                                 parameters, graph,
                                 title, fold) {
  # Start the timer and execute the method
  result <- system.time({
    method_result <- method_function(
      parameters = parameters,
      graph = graph,
      title = title,
      fold = fold,
      Save = parameters$FolderSplit
    )
  })

  # Attach execution time to the result
  method_result$execution_time <- result

  # Return the result of the community detection method
  return(method_result)
}


#' Plot Graphs
#'
#' This function plots a graph with a given community detection result.
#' It visualizes the community structure with customized aesthetics.
#'
#' @param community An object containing the community detection result.
#' @param graph An igraph object representing the graph structure.
#' @param title A string specifying the title of the plot.
#'
#' @return A plot displaying the graph with community structure.
#'
#' @export
#'
#' @examples
#' plotGraph(community, graph, "Community Structure")
plotGraph <- function(community, graph, title) {
  plot(community, graph, vertex.size = 18, edge.arrow.size = 0.5,
       vertex.color = "gold", vertex.size = 12,
       vertex.shape = "sphere", vertex.frame.color = "orange",
       vertex.label.color = "black", vertex.label.cex = 0.7,
       vertex.label.family = "Times", vertex.label.font = 2,
       edge.color = "gray", edge.width = 0.5)

  # Add title to the plot
  title(cex.main = 0.8, main = title)
}


#' Verify Partition
#'
#' This function categorizes the partitioning of a graph based on the number of communities.
#' It determines whether the partition is global, local, or hybrid.
#'
#' @param parameters A list containing relevant parameters for partition verification.
#' @param size An integer representing the number of detected communities.
#' @param vertices An integer representing the total number of vertices in the graph.
#'
#' @return A list with partition information, including the number of communities and the partition type.
#'
#' @export
#'
#' @examples
#' verifyPartition(parameters, size = 3, vertices = 10)
verifyPartition <- function(parameters, size, vertices) {
  retorno <- list()

  if (size == 1) {
    retorno$numberComm <- 1
    retorno$partition <- "global"
  } else if (size == vertices) {
    retorno$numberComm <- vertices
    retorno$partition <- "local"
  } else {
    retorno$size <- size
    retorno$partition <- "hybrid"
  }

  return(retorno)
  gc()  # Perform garbage collection
}



#' Create Data Frames for Community Analysis
#'
#' This function initializes empty data frames for storing community detection results.
#' It prepares two data frames: one for general community information and another for individual node assignments.
#'
#' @return A list containing two empty data frames: `infoComm_final` for community information
#'         and `communities_final` for node assignments.
#'
#' @export
#'
#' @examples
#' df <- createDF()
#' str(df)
createDF <- function() {
  retorno <- list()

  # Initialize data frame for general community information
  infoComm_final <- data.frame(
    split = numeric(0),
    sparsification = character(0),
    method = character(0),
    hierarchical = logical(0),
    type.community = character(0),
    numberComm = numeric(0),
    modularity = numeric(0),
    stringsAsFactors = FALSE
  )

  # Initialize data frame for community membership information
  communities_final <- data.frame(
    split = numeric(0),
    sparsification = character(0),
    method = character(0),
    hierarchical = logical(0),
    type.community = character(0),
    labels = character(0),
    groups = numeric(0),
    stringsAsFactors = FALSE
  )

  retorno$infoComm_final <- infoComm_final
  retorno$communities_final <- communities_final
  return(retorno)
}



#' Perform Hierarchical Partition Analysis
#'
#' This function evaluates whether a given community structure exhibits a hierarchical organization.
#' If the structure is hierarchical, it generates a dendrogram visualization and hierarchical partition data.
#'
#' The function categorizes the hierarchy level as follows:
#' - **0**: No merges detected; hierarchical partitioning is not possible.
#' - **1**: A single merge detected; hierarchical partitioning is not feasible.
#' - **2**: Multiple merges detected; hierarchical structure confirmed.
#' - **3**: The structure is not hierarchical.
#'
#' If the community structure is hierarchical (`h_level == 2`), the function:
#' - Generates and saves a dendrogram plot in a PDF file.
#' - Computes hierarchical partitions and saves them as a CSV file.
#' - Returns a dataframe containing partition data.
#'
#' @param comm A community structure object, typically obtained from hierarchical clustering methods.
#' @param string1 A string used as a base name for the dendrogram output file (saved as `<string1>-dendro.pdf`).
#' @param string2 A string used as a base name for the hierarchical partition CSV output file (saved as `<string2>-hierarchical.csv`).
#' @param Save A string specifying the directory where output files will be saved.
#'
#' @return A list containing:
#' - `h_level`: An integer indicating the detected hierarchy level.
#' - `hybrid.partition`: A string indicating whether hierarchical partitioning was possible (`"yes"` or `"no"`).
#' - `partitions`: A dataframe with hierarchical partitions if `h_level == 2`, otherwise `NULL`.
#'
#' @examples
#' # Example usage:
#' result <- hierarchicalPartition(comm, "example", "example_output", "./output")
#' print(result$h_level)
#'
#' if (!is.null(result$partitions)) {
#'   print(head(result$partitions))
#' }
#'
#' @export
hierarchicalPartition <- function(comm, string1, string2, Save) {
  set.seed(123)

  # Check if the community structure is hierarchical
  if (is_hierarchical(comm)) {
    num_merges <- nrow(comm$merges)
    labels <- comm$names

    if (num_merges == 1) {
      cat("\nNumber of merges is 1. Cutting is not possible.")
      return(list(h_level = 1,
                  hybrid.partition = "no",
                  partitions = NULL))

    } else if (num_merges < 1) {
      cat("\nNumber of merges is 0. Cutting is not possible.")
      return(list(h_level = 0,
                  hybrid.partition = "no",
                  partitions = NULL))

    } else {
      # Generate and save the dendrogram plot
      pdf(file = paste0(Save, "/", string1, "-dendro.pdf"), width = 10, height = 8)
      print(plot_dendrogram(comm))
      dev.off()
      cat("\nDendrogram saved.\n")

      # Create a dataframe to store partition information
      partitions <- data.frame(labels, cut = numeric(length(labels)))

      # Generate hierarchical partitions
      for (k in 1:comm$vcount) {
        cat("\nProcessing partition =", k)
        partitions[paste("partition", k, sep = "")] <- cut_at(comm, k)
      }

      # Remove the initial placeholder column
      partitions <- partitions[, -2]

      # Save partition data to a CSV file
      write.csv(partitions, paste0(Save, "/", string2, "-hierarchical.csv"), row.names = FALSE)

      return(list(h_level = 2,
                  hybrid.partition = "yes",
                  partitions = partitions))  # Return hierarchy level and partitions
    }
  } else {
    cat("\nNot Hierarchical!\n")
    return(list(h_level = 3,
                hybrid.partition = "no",
                partitions = NULL))
  }
}


#' Process Community Detection Results
#'
#' This function processes the results of a community detection algorithm by extracting information
#' about the detected communities, determining the partition type, and generating hierarchical partitions.
#' It also saves a dendrogram plot and a CSV file with the partition data.
#'
#' @param parameters A list containing:
#'   - `community_structure`: The output of a community detection algorithm (e.g., from `cluster_walktrap`, `cluster_louvain`).
#'   - `method`: A string specifying the name of the community detection method (e.g., "walktrap", "louvain").
#'   - `parameters`: A list of additional parameters for processing (e.g., sparsification settings).
#'   - `graph`: The original graph object used for community detection.
#'   - `title`: A string used as a prefix for output file names.
#'   - `fold`: A string representing the number fold data
#'   - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'   - `original.communities`: A data frame with community membership information.
#'   - `info.communities`: A data frame with details about the detected communities (e.g., modularity, hierarchy).
#'   - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' This function:
#'   - Extracts the number of communities and determines whether the partition is global, local, or hybrid.
#'   - Stores community structure and membership information in data frames.
#'   - Generates hierarchical partitions and saves results to CSV files.
#'   - Plots and saves the detected community structure.
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   community_structure = cluster_walktrap(graph),
#'   method = "walktrap",
#'   parameters = list(),
#'   graph = graph,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- processCommunityDetection(parameters)
#' print(result$info.communities)
#' print(result$resulting.partitions)
processCommunityDetection <- function(parameters) {

  # parameters = parameters
  result = list()
  df = createDF()

#   parameters$community_structure = wt

  ######################################################################
  # Obtém o número de comunidades e determina o tipo de partição
  size <- sizes(parameters$community_structure)
  size <- as.numeric(length(size))
  partition <- verifyPartition(parameters$parameters, size, as.numeric(parameters$community_structure$vcount))
  partition  <-  toString(partition$partition)

  ######################################################################
  # Calculando a modularidade com tratamento de erro
  modularity_value <- tryCatch({
    modularity_value <- as.numeric(modularity(parameters$community_structure))
    #print(class(parameters$community_structure))
    #print(parameters$community_structure)
    #cat("Modularidade:", modularity_value, "\n")
    modularity_value
  }, error = function(e) {
    cat("Erro ao calcular modularidade:", e$message, "\n")
    0  # Retorna 0 em caso de erro
  })


  ######################################################################
  hierarchical = is_hierarchical(parameters$community_structure)
  labels = parameters$community_structure$names
  clusters = parameters$community_structure$membership
  partition.data = data.frame(labels = labels,
                              clusters = clusters)

  ######################################################################
  # Retorna os resultados organizados
  if(hierarchical == FALSE){
    result$resulting.partitions = partition.data

    ######################################################################
    # Gera partições hierárquicas
    filename1 = paste(parameters$Save, "/", parameters$title,
                      "-", parameters$method, "-partition.csv",
                      sep = "")
    write.csv(partition.data, filename1, row.names = FALSE)


  } else {
    ######################################################################
    # Gera partições hierárquicas
    filename1 = paste(parameters$title, "-", parameters$method, sep = "")
    filename2 = paste(parameters$title, "-", parameters$method, "-partitions", sep = "")
    partition.data = hierarchicalPartition(comm = parameters$community_structure,
                                           string1 = filename1,
                                           string2 = filename2,
                                           Save = parameters$Save)
    result$resulting.partitions = partition.data
  }

    ######################################################################
  # Armazena informações sobre a comunidade no dataframe `communityInfo`
  df$infoComm_final = rbind(df$infoComm_final,
                           data.frame(split = parameters$fold,
                                      sparsification = parameters$title,
                                      method = parameters$method,
                                      hierarchical = toString(hierarchical),
                                      type.community = partition,
                                      numberComm = size,
                                      modularity = modularity_value))

  ######################################################################
  # Armazena estrutura detalhada das comunidades no dataframe `communities`
  df$communities_final = rbind(df$communities_final, data.frame(split = parameters$fold,
                                                    sparsification = parameters$title,
                                                    method = parameters$method,
                                                    hierarchical = toString(hierarchical),
                                                    type.community = partition,
                                                    labels = labels,
                                                    groups = clusters))

  ######################################################################
  # Salva o gráfico das comunidades detectadas
  pdf(paste(parameters$Save, "/", parameters$title, "-", parameters$method, ".pdf", sep = ""), width = 10, height = 8)
  print(plotGraph(parameters$community_structure, parameters$graph, parameters$method))
  dev.off()

  result$original.communities = df$communities_final
  result$info.communities = df$infoComm_final

  return(result)
}



#' Execute Spin Glass Algorithm for Community Detection
#'
#' This function applies the Spin Glass algorithm to detect communities in a graph and processes the results.
#'
#' @param parameters A list containing:
#'   - `graph`: An `igraph` object representing the graph to be analyzed.
#'   - `weights`: An optional vector of edge weights for the graph.
#'   - `title`: A string used as a prefix for output file names.
#'   - `fold`: A string representing the fold number (for cross-validation scenarios).
#'   - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing the results of the community detection process, including:
#'   - `original.communities`: A data frame with node community membership information.
#'   - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'   - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The Spin Glass algorithm is used for community detection in complex networks.
#' Once the algorithm is executed, the results are processed by `processCommunityDetection()`,
#' which organizes the information, saves data to files, and generates visualizations.
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeSpinGlass(parameters)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeSpinGlass <- function(parameters) {

  # parameters = parameters

  cat("\n #-------------------------------------------#")
  cat("\n # SPIN GLASS COMMUNITY DETECTION")
  cat("\n #-------------------------------------------# \n")

  sg <- cluster_spinglass(parameters$graph, weights = parameters$weights)

  parameters$community_structure <- sg
  parameters$method <- "SpinGlass"
  res <- processCommunityDetection(parameters)

  return(res)
}


#' Execute Edge Betweenness Community Detection
#'
#' This function applies the Edge Betweenness community detection algorithm to a graph
#' and processes the results, including modularity, hierarchical structure, and community assignments.
#' The function also generates and saves hierarchical partitions and dendrograms in the specified output folder.
#'
#' @param parameters A list containing:
#'   - `graph`: An `igraph` object representing the graph to be analyzed.
#'   - `weights`: An optional vector of edge weights for the graph.
#'   - `title`: A string used as a prefix for output file names.
#'   - `fold`: A string representing the fold number (for cross-validation scenarios).
#'   - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'   - `original.communities`: A data frame with node community membership information.
#'   - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'   - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The Edge Betweenness algorithm detects communities by iteratively removing edges
#' with the highest betweenness centrality. Once the algorithm is executed,
#' the results are processed by `processCommunityDetection()`, which organizes the data,
#' saves results to files, and generates visualizations.
#'
#' @references
#' - Girvan, M., & Newman, M. E. (2002). Community structure in social and biological networks.
#'   *Proceedings of the National Academy of Sciences, 99(12), 7821-7826.*
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeEdgeBetweenness(parameters)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeEdgeBetweenness <- function(parameters) {

  cat("\n #-------------------------------------------#")
  cat("\n # EDGE BETWEENNESS COMMUNITY DETECTION")
  cat("\n #-------------------------------------------# \n")

  eb <- cluster_edge_betweenness(parameters$graph,
                                 weights = parameters$weights,
                                 edge.betweenness = TRUE,
                                 merges = TRUE,
                                 bridges = TRUE,
                                 modularity = TRUE,
                                 membership = TRUE)

  parameters$community_structure <- eb
  parameters$method <- "EdgeBetweenness"
  res <- processCommunityDetection(parameters)


  return(res)
}

#' Execute Fast Greedy Community Detection
#'
#' This function applies the Fast Greedy community detection algorithm to a graph,
#' calculates modularity, and processes the results, including community membership and hierarchical structure.
#' The function also generates and saves hierarchical partitions and dendrograms in the specified output folder.
#'
#' @param parameters A list containing:
#'   - `graph`: An `igraph` object representing the graph to be analyzed.
#'   - `weights`: An optional vector of edge weights for the graph.
#'   - `title`: A string used as a prefix for output file names.
#'   - `fold`: A string representing the fold number (for cross-validation scenarios).
#'   - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'   - `original.communities`: A data frame with node community membership information.
#'   - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'   - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The Fast Greedy algorithm detects communities by greedily optimizing modularity
#' in a hierarchical manner. Once the algorithm is executed,
#' the results are processed by `processCommunityDetection()`,
#' which organizes the data, saves results to files, and generates visualizations.
#'
#' @references
#' - Clauset, A., Newman, M. E., & Moore, C. (2004). Finding community structure in very large networks.
#'   *Physical Review E, 70(6), 066111.*
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeFastGreedy(parameters)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeFastGreedy <- function(parameters) {

  cat("\n #-------------------------------------------#")
  cat("\n # FAST GREEDY COMMUNITY DETECTION")
  cat("\n #-------------------------------------------# \n")

  ######################################################################
  # Simplify the graph by removing loops and multiple edges
  g <- simplify(parameters$graph)

  # Apply the Fast Greedy clustering algorithm
  fg <- cluster_fast_greedy(g,
                            merges = TRUE,
                            modularity = TRUE,
                            membership = TRUE,
                            weights = parameters$weights)

  parameters$community_structure <- fg
  parameters$method <- "FastGreedy"
  res <- processCommunityDetection(parameters)

  return(res)
}


##################################################################################################
# InfoMap  -------------------------------------------------------------
#' Execute InfoMap Community Detection
#'
#' This function applies the InfoMap community detection algorithm to a graph, calculates modularity,
#' and processes the results, including community membership and hierarchical structure. The function also
#' generates and saves hierarchical partitions, dendrograms, and community structure in the specified output folder.
#'
#' @param parameters A list containing:
#'   - `graph`: An `igraph` object representing the graph to be analyzed.
#'   - `weights`: An optional vector of edge weights for the graph.
#'   - `title`: A string used as a prefix for output file names.
#'   - `fold`: A string representing the fold number (for cross-validation scenarios).
#'   - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'   - `original.communities`: A data frame with node community membership information.
#'   - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'   - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The InfoMap algorithm identifies community structure in complex networks by minimizing the description
#' length of a random walk process. Once the algorithm is executed, the results are processed by `processCommunityDetection()`,
#' which organizes the data, saves results to files, and generates visualizations.
#'
#' @references
#' - Rosvall, M., & Bergstrom, C. T. (2008). Maps of random walks on complex networks reveal community structure.
#'   *Proceedings of the National Academy of Sciences, 105(4), 1118-1123.*
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeInfoMap(parameters)
#' print(result$original.communities)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeInfoMap <- function(parameters){

  cat("\n #-------------------------------------------#")
  cat("\n # INFO MAP COMMUNITY DETECTION")
  cat("\n #-------------------------------------------# \n")

  im <- cluster_infomap(parameters$graph,
                        e.weights = parameters$weights,
                        nb.trials = 10,
                        modularity = TRUE)

  parameters$community_structure <- im
  parameters$method <- "InfoMap"
  res <- processCommunityDetection(parameters)

  return(res)
}



##################################################################################################
# Label Propagation -------------------------------------------------------------
#' Execute Label Propagation Community Detection
#'
#' This function applies the Label Propagation community detection algorithm to a graph, calculates modularity,
#' and processes the results, including community membership and hierarchical structure. The function also
#' generates and saves hierarchical partitions, dendrograms, and community structure in the specified output folder.
#'
#' @param parameters A list containing:
#'   - `graph`: An `igraph` object representing the graph to be analyzed.
#'   - `weights`: An optional vector of edge weights for the graph.
#'   - `title`: A string used as a prefix for output file names.
#'   - `fold`: A string representing the fold number (for cross-validation scenarios).
#'   - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'   - `original.communities`: A data frame with node community membership information.
#'   - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'   - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The Label Propagation algorithm identifies communities by propagating labels through the graph. Each node is assigned
#' a label initially, and in each iteration, a node adopts the most frequent label of its neighbors. The process stops when
#' the labels stabilize. Once the algorithm is executed, the results are processed by `processCommunityDetection()`,
#' which organizes the data, saves results to files, and generates visualizations.
#'
#' @references
#' - Raghavan, U. N., Albert, R., & Kumara, S. (2007). Near linear time algorithm to detect community structures in large-scale networks.
#'   *Physical Review E, 76(3), 036106.*
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeLabelPropagation(parameters)
#' print(result$original.communities)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeLabelPropagation <- function(parameters){

  cat("\n #-------------------------------------------#")
  cat("\n # LABEL PROPAGATION COMMUNITY DETECTION")
  cat("\n #-------------------------------------------# \n")

  lp <- cluster_label_prop(parameters$graph,
                           weights = parameters$weights)

  parameters$community_structure <- lp
  parameters$method <- "LabelPropagation"
  res <- processCommunityDetection(parameters)

  return(res)
}


##################################################################################################
# Leading Eigenvector -------------------------------------------------------------
#' Execute Leading Eigenvector Community Detection
#'
#' This function applies the Leading Eigenvector community detection algorithm to a graph,
#' calculates modularity, and processes the results, including community membership and hierarchical structure.
#' The function also generates and saves the dendrogram, hierarchical partitions, and community structure
#' in the specified output folder.
#'
#' @param parameters A list containing:
#'   - `graph`: An `igraph` object representing the graph to be analyzed.
#'   - `weights`: A vector of edge weights for the graph (optional).
#'   - `title`: A string used as a prefix for output file names.
#'   - `fold`: A string representing the fold number (for cross-validation scenarios).
#'   - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'   - `original.communities`: A data frame with node community membership information.
#'   - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'   - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The Leading Eigenvector algorithm identifies community structure by maximizing modularity and
#' leveraging the eigenvectors of the graph's modularity matrix. Once the algorithm is executed, the results
#' are processed by the `processCommunityDetection()` function, which organizes the data, saves results to files,
#' and generates visualizations.
#'
#' @references
#' - Newman, M. E. (2006). Modularity and community structure in networks.
#'   *Proceedings of the National Academy of Sciences, 103(23), 8577-8582.*
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeLeadingEigenVector(parameters)
#' print(result$original.communities)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeLeadingEigenVector <- function(parameters){

  cat("\n #-------------------------------------------#")
  cat("\n # LEADING EIGENVECTOR COMMUNITY DETECTION")
  cat("\n #-------------------------------------------# \n")

  le <- cluster_leading_eigen(parameters$graph,
                              weights = parameters$weights)

  parameters$community_structure <- le
  parameters$method <- "LeadingEigenVector"
  res <- processCommunityDetection(parameters)

  return(res)
}


##################################################################################################
# Leiden Algorithm -------------------------------------------------------------
#' Execute Leiden Community Detection Algorithm
#'
#' This function applies the Leiden community detection algorithm to a graph,
#' calculates modularity, and stores the results, including community membership and hierarchical structure.
#' The function also generates and saves the dendrogram, hierarchical partitions, and community structure
#' in the specified output folder.
#'
#' @param parameters A list containing necessary settings for the function.
#'   - It must include:
#'     - `graph`: A graph object representing the built graph on which the community detection will be performed.
#'     - `weights`: A vector of edge weights for the graph (optional).
#'     - `title`: A string used as a prefix for output file names.
#'     - `fold`: A string representing the fold number (for cross-validation scenarios).
#'     - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'   - `original.communities`: A data frame with node community membership information.
#'   - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'   - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The Leiden algorithm is an improvement on the Louvain method, designed to find communities more efficiently
#' and with greater modularity. It works by optimizing the modularity function through local moves and
#' improving the clustering quality.
#'
#' @references
#' - Traag, V. A., Van Dooren, P., & Nesterov, Y. (2019).
#'   "Narrow spectral gaps for community detection".
#'   *Physical Review E, 80(1), 016114.*
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeLeiden(parameters)
#' print(result$original.communities)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeLeiden <- function(parameters){

  cat("\n #-------------------------------------------# \n")
  cat("\n # LEIDEN COMMUNITY DETECTION \n")
  cat("\n #-------------------------------------------# \n")

  ld <- cluster_leiden(parameters$graph,
                       weights = parameters$weights)

  parameters$community_structure <- ld
  parameters$method <- "Leiden"
  res <- processCommunityDetection(parameters)
  return(res)
}


##################################################################################################
# Louvain Algorithm -------------------------------------------------------------
#' Execute Louvain Community Detection Algorithm
#'
#' This function applies the Louvain community detection algorithm to a graph,
#' calculates modularity, and stores the results, including community membership and hierarchical structure.
#' The function also generates and saves the dendrogram, hierarchical partitions, and community structure
#' in the specified output folder.
#'
#' @param parameters A list containing necessary settings for the function.
#'   It must include:
#'     - `graph`: A graph object representing the built graph on which the community detection will be performed.
#'     - `weights`: A vector of edge weights for the graph (optional).
#'     - `title`: A string used as a prefix for output file names.
#'     - `fold`: A string representing the fold number (for cross-validation scenarios).
#'     - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'   - `original.communities`: A data frame with node community membership information.
#'   - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'   - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The Louvain method is a widely used and efficient approach for detecting communities in large networks.
#' It optimizes modularity by grouping nodes into communities and maximizing the modularity measure.
#'
#' @references
#' - Blondel, V. D., Guillaume, J. L., Lambiotte, R., & Lefebvre, E. (2008).
#'   "Fast unfolding of communities in large networks".
#'   *Journal of Statistical Mechanics: Theory and Experiment, 2008(10), P10008.*
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeLouvain(parameters)
#' print(result$original.communities)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeLouvain <- function(parameters){

  cat("\n #-------------------------------------------#")
  cat("\n # LOUVAIN COMMUNITY DETECTION \n")
  cat("\n #-------------------------------------------# \n")

  lv <- cluster_louvain(parameters$graph,
                        weights = parameters$weights)

  parameters$community_structure <- lv
  parameters$method <- "Louvain"
  res <- processCommunityDetection(parameters)

  return(res)
}


##################################################################################################
# Optimal Algorithm -------------------------------------------------------------
#' Execute Optimal Community Detection Algorithm
#'
#' This function applies the Optimal community detection algorithm to a graph,
#' calculates modularity, and stores the results, including community membership and hierarchical structure.
#' The function also generates and saves the dendrogram, hierarchical partitions, and community structure
#' in the specified output folder.
#'
#' @param parameters A list of parameters containing necessary settings for the function.
#'        It must include:
#'          - `graph`: A graph object representing the built graph on which the community detection will be performed.
#'          - `weights`: A vector of edge weights for the graph (optional).
#'          - `title`: A string used as a prefix for output file names.
#'          - `fold`: A string representing the fold number (for cross-validation scenarios).
#'          - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'         - `original.communities`: A data frame with node community membership information.
#'         - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'         - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The Optimal algorithm aims to detect communities in networks by optimizing modularity and ensuring efficient results.
#'
#' @references
#' - Schuetz, M., et al. (2017). "Optimal Community Detection Algorithm."
#'   *Journal of Complex Networks, 6(4), 537-559.*
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeOptimal(parameters)
#' print(result$original.communities)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeOptimal <- function(parameters){

  cat("\n #-------------------------------------------#")
  cat("\n # OPTIMAL COMMUNITY DETECTION ")
  cat("\n #-------------------------------------------# \n")

  op = cluster_optimal(parameters$graph,
                       weights = parameters$weights)

  parameters$community_structure = op
  parameters$method = "Optimal"
  res <- processCommunityDetection(parameters)

  return(res)
  gc()
}



##################################################################################################
# Walk Trap Algorithm -------------------------------------------------------------
#' Execute Walk Trap Community Detection Algorithm
#'
#' This function applies the Walk Trap community detection algorithm to a graph,
#' calculates modularity, and stores the results, including community membership and hierarchical structure.
#' The function also generates and saves the dendrogram, hierarchical partitions, and community structure
#' in the specified output folder.
#'
#' @param parameters A list of parameters containing necessary settings for the function.
#'        It must include:
#'          - `graph`: A graph object representing the built graph on which the community detection will be performed.
#'          - `weights`: A vector of edge weights for the graph (optional).
#'          - `title`: A string used as a prefix for output file names.
#'          - `fold`: A string representing the fold number (for cross-validation scenarios).
#'          - `Save`: The directory path where output files will be saved.
#'
#' @return A list containing:
#'         - `original.communities`: A data frame with node community membership information.
#'         - `info.communities`: A data frame with details about the detected communities (e.g., modularity).
#'         - `resulting.partitions`: A list containing hierarchical partitioning results.
#'
#' @details
#' The Walk Trap algorithm detects communities based on random walks and is known for identifying
#' hierarchical community structures.
#'
#' @references
#' - Pons, P., & Latapy, M. (2006). "Computing communities in large networks using random walks."
#'   *International Symposium on Computer and Information Sciences*, 284-293.
#'
#' @export
#'
#' @examples
#' parameters <- list(
#'   graph = my_graph,
#'   weights = E(my_graph)$weight,
#'   title = "my_analysis",
#'   fold = "fold_1",
#'   Save = "./output"
#' )
#' result <- executeWalkTrap(parameters)
#' print(result$original.communities)
#' print(result$info.communities)
#' print(result$resulting.partitions)
executeWalkTrap <- function(parameters){

  cat("\n #-------------------------------------------#")
  cat("\n # WALKTRAP COMMUNITY DETECTION")
  cat("\n #-------------------------------------------# \n")

  # Apply WalkTrap community detection algorithm
  wt = cluster_walktrap(parameters$graph,
                        weights = parameters$weights,  # Corrected: `parameters$graph` and `parameters$weigths` changed to `parameters$graph` and `parameters$weights`
                        merges = TRUE,
                        modularity = TRUE,
                        membership = TRUE)

  parameters$community_structure = wt
  parameters$method = "WalkTrap"

  res <- processCommunityDetection(parameters)

  return(res)
  gc()
}



#' Compute k-Nearest Neighbors (kNN) Results
#'
#' This function processes a set of kNN parameters and executes multiple community detection algorithms on graph data.
#' It stores runtime and summary results, saving them as CSV files if specified.
#'
#' @param parameters A list containing necessary parameters including dataset name, folder paths, similarity values, and control flags.
#'
#' @return A list containing runtime and summary results for different kNN values.
#'
#' @examples
#' parameters <- list(knn.values = list(values = c(5, 10, 15)),
#'                     FolderSplit = "./data",
#'                     FolderSplitDF = "./dataframes",
#'                     Dataset.Name = "example",
#'                     Dataset.Info = list(Labels = 3),
#'                     fold = 1,
#'                     similarity = "cosine",
#'                     Save.Csv.Files = 1)
#' result <- compute.knn(parameters)
#'
#' @export
compute.knn <- function(parameters){

  retorno <- list()  # Create a list to store results
  total = length(parameters$knn.values$values)

  final.runtime <- data.frame(
    user.self = numeric(),
    sys.self = numeric(),
    elapsed = numeric(),
    user.child = numeric(),
    sys.child = numeric(),
    fold = integer(),
    knn = integer(),
    method = character()
  )

  final.summary <- data.frame(
    dataset_name = character(),
    fold = integer(),
    knn = integer(),
    similarity_measure = character(),
    connected = integer()
  )


  final.original.comm <- data.frame(split = integer(),
                                    sparsification = character(),
                                    method = character(),
                                    hierarchical = character(),
                                    type.community = character(),
                                    labels = character(),
                                    groups = integer())

  final.info.comm <- data.frame(split = integer(),
                                sparsification = character(),
                                method = character(),
                                hierarchical = character(),
                                type.community = character(),
                                numberComm = character(),
                                modulatiry = integer())

  partitions <- data.frame(labels = character(),
                           clusters = integer())

  i = 1
  while(i <= total){
    cat("\n\n\n#========================================================\n")
    cat("\n KNN: ", parameters$knn.values$values[i], "\n")
    cat("\n#========================================================\n\n\n")

    df = createDF()

    FolderKnn = paste(parameters$FolderSplit, "/knn-", parameters$knn.values$values[i], sep="")
    if(!dir.exists(FolderKnn)) dir.create(FolderKnn)
    parameters$FolderKnn = FolderKnn

    name.file = paste(parameters$FolderSplitDF, "/",
                      parameters$Dataset.Name, "-knn-", i, ".csv", sep="")
    graph_data <- read.csv(name.file)

    sorted_graph <- graph_data[order(-graph_data$similarity), ]
    graph_no_loops <- sorted_graph[-(1:parameters$Dataset.Info$Labels),]
    graph_no_loops[is.na(graph_no_loops)] <- 0
    final_graph <- graph_from_data_frame(graph_no_loops, directed = FALSE)

    if(is_connected(final_graph)){
      cat("\n ==>> CONNECTED <<== \n")

      parameters$weigths = graph_no_loops$weights
      parameters$graph = final_graph
      parameters$Save = FolderKnn
      parameters$title = paste("knn-", i, sep="")

      timeSG = system.time(resSG <- executeSpinGlass(parameters = parameters))
      timeEB = system.time(resEB <- executeEdgeBetweenness(parameters = parameters))
      timeFG = system.time(resFG <- executeFastGreedy(parameters = parameters))
      timeIM = system.time(resIM <- executeInfoMap(parameters = parameters))
      timeLP = system.time(resLP <- executeLabelPropagation(parameters = parameters))
      timeLE = system.time(resLE <- executeLeadingEigenVector(parameters = parameters))
      timeLD = system.time(resLD <- executeLeiden(parameters = parameters))
      timeLV = system.time(resLV <- executeLouvain(parameters = parameters))
      timeOP = system.time(resOP <- executeOptimal(parameters = parameters))
      timeWT = system.time(resWT <- executeWalkTrap(parameters = parameters))

      knn.original.communities = rbind(resSG$original.communities,
                                       resEB$original.communities,
                                       resFG$original.communities,
                                       resIM$original.communities,
                                       resLP$original.communities,
                                       resLE$original.communities,
                                       resLD$original.communities,
                                       resLV$original.communities,
                                       resOP$original.communities,
                                       resWT$original.communities)

      final.original.comm = rbind(final.original.comm, knn.original.communities)

      knn.info.communities = rbind(resSG$info.communities, resEB$info.communities,
                                   resFG$info.communities, resIM$info.communities,
                                   resLP$info.communities, resLE$info.communities,
                                   resLD$info.communities, resLV$info.communities,
                                   resOP$info.communities, resWT$info.communities)

      final.info.comm = rbind(final.info.comm, knn.info.communities)

      runtime_communities = rbind(timeSG, timeEB, timeFG, timeIM, timeLP, timeLE, timeLD, timeLV, timeOP, timeWT)
      new_runtime = data.frame(runtime_communities, fold = parameters$fold, knn = i, method = rownames(runtime_communities))
      final.runtime = rbind(final.runtime, new_runtime)

      summary = parameters$knn.summary
      data = data.frame(dataset_name = parameters$Dataset.Name,
                        fold = parameters$fold,
                        knn = parameters$knn.values$values[i],
                        similarity_measure = parameters$similarity,
                        connected = 1)
      summary <- rbind(summary, data)
      final.summary = rbind(final.summary, summary)

      if(parameters$Save.Csv.Files == 1){
        write.csv(runtime_communities, paste(parameters$FolderKnn, "/knn-", i, "-runtime.csv", sep = ""), row.names = FALSE)
        write.csv(summary, paste(parameters$FolderKnn, "/knn-", i, "-summary.csv", sep = ""), row.names = FALSE)
        write.csv(knn.info.communities, paste(parameters$FolderKnn, "/knn-", i, "-info-communities.csv", sep = ""), row.names = FALSE)
      }

      retorno[[paste0("knn", i)]] <- list(Runtime = runtime_communities,
                                          Summary = summary,
                                          Info.Communities = knn.info.communities,
                                          Original.communities = knn.original.communities,
                                          SpinGlass = resSG,
                                          EdgeBetweenness = resEB,
                                          FastGreedy = resFG,
                                          InfoMap = resIM,
                                          LabelPropagation = resLP,
                                          LeadingEigenVector = resLE,
                                          Leading = resLD,
                                          Louvain = resLV,
                                          Optimal = resOP,
                                          WalkTrap = resWT)

    } else {
      cat("\n===========>>>> NOT CONNECTED")

      data = data.frame(dataset_name = parameters$Dataset.Name,
                        fold = parameters$fold,
                        knn = parameters$knn.values$values[i],
                        similarity_measure = parameters$similarity,
                        connected = 0)
      final.summary = rbind(final.summary, data)

      if(parameters$Save.Csv.Files == 1){
        write.csv(final.summary, paste(parameters$FolderKnn, "/knn-", i, "-summary.csv", sep = ""), row.names = FALSE)
      }

      retorno[[paste0("knn", i)]] <- list(Summary = final.summary)
    }

    i = i + 1
    gc()
  }

  retorno$Runtime.all.Knn = final.runtime[-1,]
  retorno$Summary.all.Knn = final.summary[-1,]
  retorno$Original.Communities.all.Trh = final.original.comm
  retorno$Info.Communities.all.Trh = final.info.comm

  return(retorno)
}


#' Compute Training Results
#'
#' This function processes a set of training parameters and executes multiple community detection algorithms on graph data.
#' It stores the runtime and summary of results, saving them as CSV files if specified.
#'
#' @param parameters A list containing necessary parameters including dataset name, folder paths, similarity values, and control flags.
#'
#' @return A list containing runtime and summary results for different training values.
#'
#' @examples
#' parameters <- list(tr.values = list(values = c(0.1, 0.2, 0.3)),
#'                     FolderSplit = "./data",
#'                     FolderSplitDF = "./dataframes",
#'                     Dataset.Name = "example",
#'                     Dataset.Info = list(Labels = 3),
#'                     fold = 1,
#'                     similarity = "cosine",
#'                     Save.Csv.Files = 1)
#' result <- compute.tr(parameters)
#'
#' @export
compute.tr <- function(parameters){

  retorno <- list()  # Create a list to store results
  total = length(parameters$tr.values$values)

  final.runtime <- data.frame(
    user.self = numeric(),
    sys.self = numeric(),
    elapsed = numeric(),
    user.child = numeric(),
    sys.child = numeric(),
    fold = integer(),
    knn = integer(),
    method = character()
  )

  final.summary <- data.frame(
    dataset_name = character(),
    fold = integer(),
    trh = integer(),
    similarity_measure = character(),
    connected = integer()
  )

  final.original.comm <- data.frame(split = integer(),
                                     sparsification = character(),
                                     method = character(),
                                     hierarchical = character(),
                                     type.community = character(),
                                     labels = character(),
                                     groups = integer())

  final.info.comm <- data.frame(split = integer(),
                                sparsification = character(),
                                method = character(),
                                hierarchical = character(),
                                type.community = character(),
                                numberComm = character(),
                                modulatiry = integer())

  partitions <- data.frame(labels = character(),
                           clusters = integer())

  j = 0
  a = 1
  while(j < total){

    cat("\n\n\n#========================================================\n")
    cat("\n TR: ", parameters$tr.values$values[a], "\n")
    cat("\n#========================================================\n\n\n")

    df = createDF()

    FolderTr = paste(parameters$FolderSplit, "/tr-", j, sep="")
    if(!dir.exists(FolderTr)) dir.create(FolderTr)
    parameters$FolderTr = FolderTr

    name.file = paste(parameters$FolderSplitDF, "/",
                      parameters$Dataset.Name,
                      "-threshold-", j, ".csv", sep="")
    graph_data <- read.csv(name.file)

    sorted_graph <- graph_data[order(-graph_data$similarity), ]
    graph_no_loops <- sorted_graph[-(1:parameters$Dataset.Info$Labels),]
    graph_no_loops[is.na(graph_no_loops)] <- 0
    final_graph <- graph_from_data_frame(graph_no_loops, directed = FALSE)

    if(is_connected(final_graph)==TRUE){

      cat("\n ==>> CONNECTED <<== \n")

      parameters$weigths = graph_no_loops$weights
      parameters$graph = final_graph
      parameters$Save = FolderTr
      parameters$title = paste("tr-", j, sep="")

      timeSG = system.time(resSG <- executeSpinGlass(parameters))
      timeEB = system.time(resEB <- executeEdgeBetweenness(parameters))
      timeFG = system.time(resFG <- executeFastGreedy(parameters))
      timeIM = system.time(resIM <- executeInfoMap(parameters))
      timeLP = system.time(resLP <- executeLabelPropagation(parameters))
      timeLE = system.time(resLE <- executeLeadingEigenVector(parameters))
      timeLD = system.time(resLD <- executeLeiden(parameters))
      timeLV = system.time(resLV <- executeLouvain(parameters))
      timeOP = system.time(resOP <- executeOptimal(parameters))
      timeWT = system.time(resWT <- executeWalkTrap(parameters))

      # salvando resultados
      tr.original.communities = rbind(resSG$original.communities,
                                      resEB$original.communities,
                                      resFG$original.communities,
                                      resIM$original.communities,
                                      resLP$original.communities,
                                      resLE$original.communities,
                                      resLD$original.communities,
                                      resLV$original.communities,
                                      resOP$original.communities,
                                      resWT$original.communities)

      final.original.comm = rbind(final.original.comm, tr.original.communities)

      # salvando informações das comunidades
      tr.info.communities = rbind(resSG$info.communities,
                                  resEB$info.communities,
                                  resFG$info.communities,
                                  resIM$info.communities,
                                  resLP$info.communities,
                                  resLE$info.communities,
                                  resLD$info.communities,
                                  resLV$info.communities,
                                  resOP$info.communities,
                                  resWT$info.communities)

      final.info.comm = rbind(final.info.comm, tr.info.communities)

      # tr.partitions = rbind(resSG$resulting.partitions,
      #                       resEB$info.communities,
      #                       resFG$info.communities,
      #                       resIM$info.communities,
      #                       resLP$info.communities,
      #                       resLE$info.communities,
      #                       resLD$info.communities,
      #                       resLV$info.communities,
      #                       resOP$info.communities,
      #                       resWT$info.communities)


      runtime_communities = rbind(timeSG, timeEB, timeFG, timeIM,
                                  timeLP, timeLE, timeLD, timeLV, timeOP, timeWT)
      new_runtime = data.frame(runtime_communities,
                               fold = parameters$fold, tr = j,
                               method = rownames(runtime_communities))
      final.runtime = rbind(final.runtime, new_runtime)

      summary = parameters$tr.summary
      data = data.frame(dataset_name = parameters$Dataset.Name,
                        fold = parameters$fold,
                        trh = parameters$tr.values$values[a],
                        similarity_measure = parameters$similarity,
                        connected = 1)
      summary <- rbind(summary, data)
      final.summary = rbind(final.summary, summary)

      if(parameters$Save.Csv.Files == 1){
        write.csv(runtime_communities, paste(parameters$FolderTr, "/tr-", j, "-runtime.csv", sep = ""), row.names = FALSE)
        write.csv(summary, paste(parameters$FolderTr, "/tr-", j, "-summary.csv", sep = ""), row.names = FALSE)
      }

      retorno[[paste0("tr", j)]] <- list(Runtime = runtime_communities,
                                         Summary = summary,
                                         Info.Communities = tr.info.communities,
                                         Original.communities = tr.original.communities,
                                         SpinGlass = resSG,
                                         EdgeBetweenness = resEB,
                                         FastGreedy = resFG,
                                         InfoMap = resIM,
                                         LabelPropagation = resLP,
                                         LeadingEigenVector = resLE,
                                         Leading = resLD,
                                         Louvain = resLV,
                                         Optimal = resOP,
                                         WalkTrap = resWT)


    } else {
      cat("\n===========>>>> NOT CONNECTED")

      data = data.frame(dataset_name = parameters$Dataset.Name,
                        fold = parameters$fold,
                        trh = parameters$tr.values$values[a],
                        similarity_measure = parameters$similarity,
                        connected = 0)
      final.summary = rbind(final.summary, data)

      if(parameters$Save.Csv.Files == 1){
        write.csv(final.summary, paste(parameters$FolderTr, "/tr-", j, "-summary.csv", sep = ""), row.names = FALSE)
      }

      retorno[[paste0("tr", j)]] <- list(Summary = final.summary)
    }

    a = a + 1
    j = j + 1
  }

  retorno$Runtime.all.Trh = final.runtime[-1,]
  retorno$Summary.all.Trh = final.summary[-1,]
  retorno$Original.Communities.all.Trh = final.original.comm
  retorno$Info.Communities.all.Trh = final.info.comm

  return(retorno)
}


