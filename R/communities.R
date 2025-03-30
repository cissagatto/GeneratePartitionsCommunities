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


FolderRoot = "~/GeneratePartitionsCommunities"
FolderScripts = "~/GeneratePartitionsCommunities/R"


#' Execute community detection algorithms on multiple dataset folds
#'
#' This function iterates over multiple dataset folds and applies different community detection methods.
#' It stores execution times and summaries for each fold.
#'
#' @param parameters A list containing dataset and configuration details.
#' @return A list containing execution times, summaries, and results of multiple community detection methods.
#' @examples
#'
#' parameters <- list(
#'   Number.Folds = 5,
#'   similarity = "jaccard",
#'   Dataset.Name = "my_dataset",
#'   Dataset.Info = list(Labels = 3),
#'   Folders = list(folderCommunities = "~/Communities", folderLabelGraphs = "~/Graphs")
#' )
#'
#' results <- execute_communities(parameters)
#'
#' @export
execute.communities <- function(parameters) {


  if(parameters$sparsification==0){

    cat("\n RUNNING WHITHOUT SPARSIFICATION")

    # Create a list to store all folds
    all_results <- list()
    all_results$Runtime <- list()
    all_results$Summary <- list()
    all_results$SpinGlass <- list()
    all_results$EdgeBetweenness <- list()
    all_results$FastGreedy <- list()
    all_results$InfoMap <- list()
    all_results$LabelPropagation <- list()
    all_results$LeadingEigenVector <- list()
    all_results$Louvain <- list()
    all_results$Optimal <- list()
    all_results$WalkTrap <- list()

    #f <- 1
    #while (f <= parameters$Number.Folds) {
    cmParalel <- foreach(f = 1:parameters$Number.Folds) %dopar% {

      cat("\n-----------> Fold ", f)

      FolderRoot = "~//GeneratePartitionsCommunities"
      FolderScripts = "~//GeneratePartitionsCommunities/R"

      setwd(FolderScripts)
      source("libraries.R")

      setwd(FolderScripts)
      source("utils.R")

      setwd(FolderScripts)
      source("functions.R")

      #########################################################################
      cat("\n Store the results for the fold")
      parameters$fold = f
      name_fold <- paste0("FOLD", f)
      runtime = data.frame()
      summary <- data.frame(
        dataset_name = c(""),
        fold = c(0),
        similarity_measure = c(""),
        connected = c(0)
      )


      # Create output folder for the current fold
      cat("\nCreating FOLDER split: ")
      FolderSplit <- paste0(parameters$Folders$folderCommunities,
                            "/Split-", f)
      if (!dir.exists(FolderSplit)){dir.create(FolderSplit)}
      parameters$FolderSplit <- FolderSplit

      # Access the folder containing the dataset
      cat("\nAccessing the dataset folder")
      FolderDF <- paste0(parameters$Folders$folderLabelGraphs,
                         "/Split-", f)
      parameters$FolderSplitDF <- FolderDF

      # Load the graph file
      file_name <- paste0(FolderDF, "/", parameters$similarity,
                          "-graph-norm.csv")
      graph_data <- read.csv(file_name)
      sorted_graph <- graph_data[order(-graph_data$similarity), ]
      graph_no_loops <- sorted_graph[-(1:parameters$Dataset.Info$Labels),]
      graph_no_loops[is.na(graph_no_loops)] <- 0
      final_graph <- graph_from_data_frame(graph_no_loops,
                                           directed = FALSE)


      if (is_connected(final_graph)==TRUE) {
        cat("\n===========>>>> CONNECTED")

        summary <- rbind(summary, data.frame(
          dataset_name = parameters$Dataset.Name,
          fold = f,
          similarity_measure = parameters$similarity,
          connected = 1
        ))

        # parameters = parameters
        parameters$weigths = graph_no_loops$weights
        parameters$graph = final_graph
        parameters$title = "no-spars"
        parameters$fold = f
        parameters$Save = parameters$FolderSplit

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

        runtime_communities = rbind(timeSG,
                                    timeEB,
                                    timeFG,
                                    timeIM,
                                    timeLP,
                                    timeLE,
                                    timeLD,
                                    timeLV,
                                    timeOP,
                                    timeWT)

        runtime = runtime_communities

        write.csv(
          runtime_communities,
          paste(parameters$FolderSplit, "/",
                parameters$similarity,
                "-runtime-comm-fold-", f , ".csv", sep = ""),
          row.names = FALSE
        )


      } else  {
        cat("\n===========>>>> NOT CONNECTED")
        summary <- rbind(summary, data.frame(
          dataset_name = parameters$Dataset.Name,
          fold = f,
          similarity_measure = parameters$similarity,
          connected = 0
        ))

        #all_results$Summary[[name_fold]] <- summary
      }

      # Save summary file
      cat("\nSaving summary")
      summary <- summary[-1, ]
      write.csv(summary, paste0(parameters$FolderSplit,
                                "/fold-", f, "-",
                                parameters$similarity,
                                "-connected.csv"), row.names = FALSE)

      cat("\n retorno")
      return(list(Runtime = runtime_communities,
                  Summary = summary,
                  SpinGlass = resSG,
                  EdgeBetweenness = resEB,
                  FastGreedy = resFG,
                  InfoMap = resIM,
                  LabelPropagation = resLP,
                  LeadingEigenVector = resLE,
                  Leading = resLD,
                  Louvain = resLV,
                  Optimal = resOP,
                  WalkTrap = resWT))

      # cat("\nIncrementing f")
      # f = f + 1
    }

    #Armazenando os resultados de todos os folds
    for (i in 1:parameters$Number.Folds) {
      name_fold <- paste0("FOLD", i)
      all_results$Runtime[[name_fold]] <- cmParalel[[i]]$Runtime
      all_results$Summary[[name_fold]] <- cmParalel[[i]]$Summary
      all_results$SpinGlass[[name_fold]] <- cmParalel[[i]]$SpinGlass
      all_results$EdgeBetweenness[[name_fold]] <- cmParalel[[i]]$EdgeBetweenness
      all_results$FastGreedy[[name_fold]] <- cmParalel[[i]]$FastGreedy
      all_results$InfoMap[[name_fold]] <- cmParalel[[i]]$InfoMap
      all_results$LabelPropagation[[name_fold]] <- cmParalel[[i]]$LabelPropagation
      all_results$LeadingEigenVector[[name_fold]] <- cmParalel[[i]]$LeadingEigenVector
      all_results$Louvain[[name_fold]] <- cmParalel[[i]]$Louvain
      all_results$Optimal[[name_fold]] <- cmParalel[[i]]$Optimal
      all_results$WalkTrap[[name_fold]] <- cmParalel[[i]]$WalkTrap
    }

    cat("\nReturning results")
    return(all_results)
    gc()

  } else {

    cat("\n RUNNING WITH SPARSIFICATION")

    all_spars <- list()
    all_spars$knn <- list()
    all_spars$tr <- list()

    f = 1
    cmParalel <- foreach(f = 1:parameters$Number.Folds) %dopar% {
    # while(f <= parameters$Number.Folds){

      cat("\n----------->Fold ", f)

      ##################################################################################################
      # Configures the workspace according to the operating system                                     #
      ##################################################################################################
      FolderRoot = "~//GeneratePartitionsCommunities"
      FolderScripts = "~//GeneratePartitionsCommunities/R"

      ###############################################################################
      # Load sources                                                                #
      ###############################################################################
      cat("\nCarregando os sources\n")
      setwd(FolderScripts)
      source("libraries.R")

      setwd(FolderScripts)
      source("utils.R")

      setwd(FolderScripts)
      source("functions.R")

      setwd(FolderScripts)
      source("choose.R")

      ###############################################################################
      cat("\nCriando o FOLDER split: ")
      FolderSplit = paste(parameters$Folders$folderCommunities,
                          "/Split-", f ,sep="")
      if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}
      parameters$FolderSplit = FolderSplit

      ###############################################################################
      cat("\nAcessando o folder do data frame")
      FolderDF = paste(parameters$Folders$folderLabelGraphs,
                       "/Split-", f, sep="")
      parameters$FolderSplitDF = FolderDF

      ###############################################################################
      knn_values = read.csv(paste(parameters$FolderSplitDF ,
                                  "/spars-knn-values.csv", sep=""))
      totalKNN = nrow(knn_values)

      ###############################################################################
      threshold_values = read.csv(paste(parameters$FolderSplitDF,
                                        "/spars-tr-values.csv", sep=""))
      totalTR = nrow(threshold_values)

      ###############################################################################
      connected = c(0)
      similarity_measure = parameters$similarity
      dataset_name = parameters$Dataset.Name
      knn = c(0)
      trh = c(0)
      fold = f
      resume_knn = data.frame(dataset_name, fold, knn, similarity_measure, connected)
      resume_trh = data.frame(dataset_name, fold, trh, similarity_measure, connected)

      ###############################################################################
      # parameters = parameters
      parameters$FolderSplit = FolderSplit
      parameters$fold = f
      parameters$knn.summary = resume_knn
      parameters$tr.summary = resume_trh
      parameters$knn.values = knn_values
      parameters$tr.values = threshold_values

      ###############################################################################
      res.knn <- compute.knn(parameters)
      res.tr <- compute.tr(parameters)

      print(class(res.knn))  # Verifica o tipo de retorno
      print(class(res.tr))

      return(list(res.knn = res.knn, res.tr = res.tr))
      #f = f + 1

    } # end foreach


    #str(cmParalel)

    #Armazenando os resultados de todos os folds
    for (i in 1:parameters$Number.Folds) {
      name_fold <- paste0("FOLD", i)
      all_spars$knn[[name_fold]] <- cmParalel[[i]]$res.knn
      all_spars$tr[[name_fold]] <- cmParalel[[i]]$res.tr
    }


    cat("\nReturning results")
    return(all_spars)
    gc()

  }

  gc()
}
