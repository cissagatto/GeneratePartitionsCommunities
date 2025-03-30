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



##################################################################################################
#' Choose Hierarchical Method for Community Detection
#'
#' This function applies hierarchical community detection algorithms to multiple datasets using
#' different sparsifications (KNN and threshold methods). It organizes the data into different
#' directories, selects the best community partition based on modularity, and saves the results in CSV files.
#'
#' @param dataset_name A character string representing the name of the dataset to process.
#' @param number_folds An integer representing the number of folds to be used in cross-validation.
#' @param similarity A character string specifying the type of similarity measure used in the sparsification method
#'        (e.g., "jaccard", "cosine", etc.).
#' @param folderResults A character string specifying the path to the directory where results will be saved.
#'
#' @return This function does not return a value but saves the community detection results as CSV files
#'         in the specified output directory. These results include information about the community partition,
#'         the chosen method, modularity scores, and other relevant metadata.
#'
#' @details The function processes each fold, performs community detection using hierarchical methods, and:
#' - Creates necessary directories for storing results.
#' - Loads KNN and threshold-based sparsifications.
#' - Selects the best community partition for each sparsification using modularity as a selection criterion.
#' - Writes the results to CSV files with information about the partition, the chosen method, and modularity.
#'
#' @examples
#' \dontrun{
#' # Example of calling the chooseHierarchical function
#' chooseHierarchical(dataset_name = "Dataset1",
#'                    number_folds = 5,
#'                    similarity = "jaccard",
#'                    folderResults = "/path/to/results/")
#' }
#'
#' @import foreach
#' @importFrom dplyr filter
#' @importFrom data.table fread
#' @export
chooseHierarchical <- function(parameters, retorno){

  cat("\n==================>HIERÁRQUICO")

  f = 1
  #while(f <=  number_folds){
  chParalel <- foreach(f = 1:number_folds) %dopar% {

    ##################################################################################################
    # Configures the workspace according to the operating system                                     #
    ##################################################################################################
    FolderRoot = "~/GeneratePartitionsCommunities"
    FolderScripts = "~/GeneratePartitionsCommunities/R"

    ###############################################################################
    # Load sources                                                                #
    ###############################################################################
    cat("\nCarregando os sources\n")
    setwd(FolderScripts)
    source("libraries.R")
    source("utils.R")
    cat("\n\n------->Fold ", f)

    df_tr = createDF()
    df_knn = createDF()

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

    #########################################################
    # KNN
    i = 1
    while(i<=totalKNN){
      cat("\n\tknn", i)

      retorno$sparsification

      FolderKnn = paste(FolderSplit, "/knn-", i ,sep="")
      if(dir.exists(FolderKnn)==FALSE){dir.create(FolderKnn)}

      FolderPK = paste(FolderPartSplit, "/knn-", i ,sep="")
      if(dir.exists(FolderPK)==FALSE){dir.create(FolderPK)}

      FolderDF = paste(FolderSplit, "/knn-", i ,sep="")

      # abrindo os arquivos com as informações
      setwd(FolderDF)
      info = data.frame(read.csv(paste("split-", f, "-knn-", i, "-info.csv", sep="")))
      com = data.frame(read.csv(paste("split-", f, "-knn-", i, "-comm.csv", sep="")))

      fold = retorno$sparsification$knn[i]

      # pegando apenas os métodos hierárquicos
      info = data.frame(filter(info, hierarchical==1))

      if(nrow(info)==0){
        cat("\nNão tem [ nenhuma ] partição")

        split = f
        sparsification = paste("tr-", j, sep="")
        method = "none"
        numberComm = "none"
        modularity = 0
        hierarchical = 1
        partition = "none"

        knn_choosed =   data.frame(split, sparsification,
                                   method, numberComm, modularity,
                                   hierarchical, partition)

        df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)


      } else if(nrow(info)==1){
        cat("\nTem [ uma única ] partição")
        knn_choosed = info
        df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

      } else {
        cat("\nTem [ mais de uma ] partição")

        # qual é o maior valor de modularidade?
        maximo = info[which.max(info$modularity),]

        # encontrando outros métodos que tem exatamente o mesmo valor
        equal = data.frame(filter(info, modularity==maximo$modularity))
        n = nrow(equal)

        if(n==1){
          cat("\n\t\tn==1 ")
          # Se não houver comunidades com o mesmo valor de modularidade
          # então apenas escolha este mesmo
          knn_choosed = equal
          df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

        } else {
          cat("\nn>1")
          # Se houver mais de uma comunidade com o mesmo valor de
          # modularidade, então escolhe um apenas")
          x = round(n/2)
          knn_choosed = info[x,]
          df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)
        }
      }


      # qual é o método escolhido?
      a = toString(knn_choosed$method)

      # pegando os rótulos para criar a partição
      teste = data.frame(filter(com, method==a))

      # gather only the labels and the groups
      labels = teste$labels
      groups = teste$groups
      teste2 = data.frame(labels, groups)

      setwd(FolderPK)
      write.csv(teste2, paste("knn-", i, "-h-partition.csv", sep=""),
                row.names = FALSE)
      write.csv(knn_choosed, paste("knn-", i, "-h-choosed.csv", sep=""),
                row.names = FALSE)

      i = i + 1
      gc()
    }

    # salva o método para cada esparsificação calculada
    df_knn$infoComm_final = df_knn$infoComm_final[-1,]
    setwd(FolderPartSplit)
    write.csv(df_knn$infoComm_final, paste("fold-", f,
                                           "-knn-h-choosed.csv", sep=""),
              row.names = FALSE)


    #########################################################
    # THRESHOLD
    setwd(pasta)
    tr = data.frame(read.csv("sparsification-new-threshold.csv"))

    if(tr[1,2]=="vazio"){
      cat("\n NÃO TEM DATA FRAME")

    } else {

      n_tr = nrow(tr)
      j = 0
      while(j<n_tr){
        cat("\n\ntr", j)

        FolderTr = paste(FolderSplit, "/Tr-", j ,sep="")
        FolderPK = paste(FolderPartSplit, "/Tr-", j ,sep="")
        if(dir.exists(FolderPK)==FALSE){dir.create(FolderPK)}
        FolderDF = paste(FolderSplit, "/Tr-", j ,sep="")
        res = length(dir(FolderTr))

        if(res == 0){
          cat("\nA pasta está vazia!")
          split = f
          sparsification = paste("tr-", j, sep="")
          method = "none"
          numberComm = "none"
          modularity = 0
          hierarchical = 1
          partition = "none"

          tr_choosed =   data.frame(split, sparsification,
                                    method, numberComm, modularity,
                                    hierarchical, partition)

          df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)


        } else {

          setwd(FolderTr)
          info = data.frame(read.csv(paste("split-", f, "-tr-", j, "-info.csv", sep="")))
          com = data.frame(read.csv(paste("split-", f, "-tr-", j, "-comm.csv", sep="")))

          # separando o que é hierarquico
          info = data.frame(filter(info, hierarchical==1))

          if(nrow(info)==0){
            cat("\nNão tem nenhuma partição")

            cat("\nA pasta está vazia!")
            split = f
            sparsification = paste("tr-", j, sep="")
            method = "none"
            numberComm = "none"
            modularity = 0
            hierarchical = 1
            partition = "none"

            tr_choosed =   data.frame(split, sparsification,
                                      method, numberComm, modularity,
                                      hierarchical, partition)

            df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

          } else if(nrow(info)==0) {
            cat("\nTem [ uma única ] partição")
            tr_choosed = info
            df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

          } else {
            cat("\nTema [ mais de uma ] partição")

            # qual a maior modularidade?
            maximo = info[which.max(info$modularity),]

            # tem mais gente com esse mesmo valor?
            equal = data.frame(filter(info,modularity==maximo$modularity))
            n = nrow(equal)

            if(n==1){
              cat("\nn==1")
              tr_choosed = equal
              df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

            } else {
              cat("\nn>1")
              x = round(n/2)
              tr_choosed = info[x,]
              df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

            }

          }

          a = toString(tr_choosed$method)
          teste = data.frame(filter(com, method==a))

          # gather only the labels and the groups
          labels = teste$labels
          groups = teste$groups
          teste2 = data.frame(labels, groups)

          setwd(FolderPK)
          write.csv(teste2, paste("tr-", j, "-h-partition.csv", sep=""),
                    row.names = FALSE)
          write.csv(tr_choosed, paste("tr-", j, "-h-choosed.csv", sep=""),
                    row.names = FALSE)

        } # fim do else

        j = j + 1
        gc()
      } # fim do while

      setwd(FolderPartSplit)
      df_tr$infoComm_final = df_tr$infoComm_final[-1,]
      write.csv(df_tr$infoComm_final, paste("fold-", f, "-tr-h-choosed.csv", sep=""),
                row.names = FALSE)
      gc()
    } # fim do else

    #f = f + 1
    gc()

  } # fim do while

  gc()

} # fim da função


##################################################################################################
#' CHOOSE NON HIERARCHICAL
#'
#' This function selects non-hierarchical partitions for each dataset, based on different types of
#' sparsification and partitioning methods. It is applied to multiple datasets, divided into several
#' folds for cross-validation.
#'
#' The function performs the selection of non-hierarchical partitions, both for k-Nearest Neighbors (KNN)
#' and Thresholds, considering different conditions such as modularity values. The process includes creating
#' directories, reading and filtering data files, and generating CSV files with the selected partitions for
#' each fold.
#'
#' @param dataset_name Name of the dataset to be analyzed.
#' @param number_folds Number of folds for cross-validation.
#' @param similarity Method of similarity used in the analysis (e.g., "knn", "threshold").
#' @param folderResults Path to the folder where results will be saved.
#'
#' @return No value is returned by the function, but CSV files containing the selected partitions for each
#'         fold are saved in the directory specified in `folderResults`.
#'
#' @details
#' The function follows these steps:
#' 1. Creates specific directories to store the results.
#' 2. Reads CSV files containing information about modularity and partitions.
#' 3. Filters to select non-hierarchical and hybrid partitions.
#' 4. Selects the partitions with the highest modularity and chooses an appropriate method.
#' 5. Saves the results for each fold in CSV files, including the selected partitions.
#'
#' @examples
#' \dontrun{
#' chooseNonHierarchical("dataset1", 5, "knn", "/path/to/results")
#' }
#'
#' @seealso
#' \code{\link{chooseHierarchical}} for the function that selects hierarchical partitions.
#'
#' @import foreach
#' @importFrom dplyr filter
#' @export
chooseNonHierarchical <- function(parameters){

  cat("\n=======================>NÃO HIERÁRQUICO")

  f = 1
  #while(f <=  number_folds){
  cnhParalel <- foreach(f = 1:number_folds) %dopar% {

    ##################################################################################################
    # Configures the workspace according to the operating system                                     #
    ##################################################################################################
    FolderRoot = "~/GeneratePartitionsCommunities"
    setwd(FolderRoot)
    FolderScripts = paste(FolderRoot, "/R", sep="")

    ###############################################################################
    # Load sources                                                                #
    ###############################################################################
    setwd(FolderScripts)
    source("libraries.R")

    setwd(FolderScripts)
    source("utils.R")

    ###############################################################################
    folder = createDirs2(dataset_name, folderResults)

    createDF <- function(){

      retorno = list()

      split = c(0)
      sparsification = c("")
      method = c(0)
      numberComm = c(0)
      modularity = c(0)
      hierarchical = c(0)
      partition = c(0)

      infoComm_final = data.frame(split, sparsification, method,
                                  numberComm, modularity,
                                  hierarchical, partition)

      split = c(0)
      sparsification = c("")
      method = c(0)
      hierarchical = c(0)
      partition = c(0)
      labels = c(0)
      groups = c(0)

      communities_final = data.frame(split, sparsification,
                                     method, hierarchical, partition,
                                     labels, groups)

      retorno$infoComm_final = infoComm_final
      retorno$communities_final = communities_final
      return(retorno)
    }

    cat("\n\n============================================")
    cat("\n------->Fold ", f)

    df_tr = createDF()
    df_knn = createDF()

    folder = createDirs2(dataset_name, folderResults)

    ##################################################
    FolderSplit = paste(folder$FolderCommunities, "/Split-", f ,sep="")

    FolderPartSplit = paste(folder$FolderPartitions, "/Split-", f, sep="")
    if(dir.exists(FolderPartSplit)==FALSE){dir.create(FolderPartSplit)}

    # pasta = paste(folder$FolderDataFrame, "/", dataset_name,
    #              "/Split-", f, "/", similarity, sep="")

    pasta = paste(folder$FolderDataFrame, "/", dataset_name,
                  "/Split-", f, sep="")

    setwd(pasta)
    knn = data.frame(read.csv("sparsification-knn-values.csv"))
    n_knn = nrow(knn)

    #########################################################
    # KNN
    i = 1
    while(i<=n_knn){
      cat("\n\tknn", i)

      FolderKnn = paste(FolderSplit, "/knn-", i ,sep="")
      if(dir.exists(FolderKnn)==FALSE){dir.create(FolderKnn)}

      FolderPK = paste(FolderPartSplit, "/knn-", i ,sep="")
      if(dir.exists(FolderPK)==FALSE){dir.create(FolderPK)}

      FolderDF = paste(FolderSplit, "/knn-", i ,sep="")

      # abrindo os arquivos com as informações
      setwd(FolderDF)
      info = data.frame(read.csv(paste("split-", f, "-knn-", i, "-info.csv", sep="")))
      com = data.frame(read.csv(paste("split-", f, "-knn-", i, "-comm.csv", sep="")))

      # pegando apenas os métodos não hierárquicos
      info2 = data.frame(filter(info, hierarchical==0))

      # pegando apenas as partições hibridas
      info3 = data.frame(filter(info2, partition=="hybrid"))

      if(nrow(info3)==0){
        cat("\nNão tem [ nenhuma ] partição híbrida")
        split = f
        sparsification = paste("tr-", j, sep="")
        method = "none"
        numberComm = "none"
        modularity = 0
        hierarchical = 0
        partition = "none"

        knn_choosed = data.frame(split, sparsification,
                                 method, numberComm, modularity,
                                 hierarchical, partition)

        df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

      } else if(nrow(info3)==1) {
        cat("\nTem [ uma única ] partição híbrida")
        knn_choosed = info3
        df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

      } else {
        cat("\nTem [ mais de uma ] partição híbrida")

        # qual é o maior valor de modularidade?
        maximo = info3[which.max(info3$modularity),]

        # encontrando outros métodos que tem exatamente o mesmo valor
        equal = data.frame(filter(info3, modularity==maximo$modularity))
        n = nrow(equal)

        if(n==1){
          cat("\nn==1")
          # Se não houver comunidades com o mesmo valor de modularidade
          # então apenas escolha este mesmo
          knn_choosed = equal
          df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)

        } else {
          cat("\nn>1")
          # Se houver mais de uma comunidade com o mesmo valor de
          # modularidade, então escolhe um apenas")
          x = round(n/2)
          knn_choosed = info3[x,]
          df_knn$infoComm_final = rbind(df_knn$infoComm_final, knn_choosed)
        }

      }

      # qual é o método escolhido?
      a = toString(knn_choosed$method)

      # pegando os rótulos para criar a partição
      teste = data.frame(filter(com, method==a))

      # gather only the labels and the groups
      labels = teste$labels
      groups = teste$groups
      teste2 = data.frame(labels, groups)

      setwd(FolderPK)
      write.csv(teste2, paste("knn-", i, "-nh-partition.csv", sep=""),
                row.names = FALSE)
      write.csv(knn_choosed, paste("knn-", i, "-nh-choosed.csv", sep=""),
                row.names = FALSE)

      i = i + 1
      gc()
    }

    setwd(FolderPartSplit)
    df_knn$infoComm_final = df_knn$infoComm_final[-1,]
    write.csv(df_knn$infoComm_final, paste("fold-", f,
                                           "-knn-nh-choosed.csv", sep=""),
              row.names = FALSE)


    #########################################################
    # THRESHOLD
    ###############################################################
    #pasta = paste(folder$FolderDataFrame, "/", dataset_name,
    #              "/Split-", f, "/", similarity, sep="")

    pasta = paste(folder$FolderDataFrame, "/", dataset_name,
                  "/Split-", f, sep="")
    setwd(pasta)
    tr = data.frame(read.csv("sparsification-new-threshold.csv"))
    #cat("\nTR=", nrow(tr))

    if(tr[1,2]=="vazio"){
      cat("\nNão há Tresholds válidos!")

    } else {

      n_tr = nrow(tr)
      j = 0
      while(j<n_tr){
        cat("\n\ntr", j)

        FolderTr = paste(FolderSplit, "/Tr-", j ,sep="")
        FolderPK = paste(FolderPartSplit, "/Tr-", j ,sep="")
        if(dir.exists(FolderPK)==FALSE){dir.create(FolderPK)}
        FolderDF = paste(FolderSplit, "/Tr-", j ,sep="")
        res = length(dir(FolderTr))

        if(res == 0){
          cat("\nA pasta está vazia!")
          split = f
          sparsification = paste("tr-", j, sep="")
          method = "none"
          numberComm = "none"
          modularity = 0
          hierarchical = 0
          partition = "none"

          tr_choosed = data.frame(split, sparsification,
                                  method, numberComm, modularity,
                                  hierarchical, partition)

          df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

        } else {
          setwd(FolderTr)
          info = data.frame(read.csv(paste("split-", f, "-tr-", j, "-info.csv", sep="")))
          com = data.frame(read.csv(paste("split-", f, "-tr-", j, "-comm.csv", sep="")))

          # pegando apenas os métodos não hierárquicos
          info2 = data.frame(filter(info, hierarchical==0))

          # pegando apenas as partições hibridas
          info3 = data.frame(filter(info2, partition=="hybrid"))

          if(nrow(info3)==0){
            cat("\nNão tem [ nenhuma ] partição híbrida")
            split = f
            sparsification = paste("tr-", j, sep="")
            method = "none"
            numberComm = "none"
            modularity = 0
            hierarchical = 0
            partition = "none"

            tr_choosed = data.frame(split, sparsification,
                                    method, numberComm, modularity,
                                    hierarchical, partition)

            df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

          } else if(nrow(info3)==1){
            cat("\nTem [ uma única ] partição híbrida")
            tr_choosed = info3
            df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

          } else {
            cat("\nTem [ mais de uma ] partição híbrida")

            # qual é o maior valor de modularidade?
            maximo = info3[which.max(info3$modularity),]

            # encontrando outros métodos que tem exatamente o mesmo valor
            equal = data.frame(filter(info3, modularity==maximo$modularity))
            n = nrow(equal)

            if(n==1){
              cat("\nn==1 ")
              # Se não houver comunidades com o mesmo valor de modularidade
              # então apenas escolha este mesmo
              tr_choosed = equal
              df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

            } else {
              cat("\nn>1")
              # Se houver mais de uma comunidade com o mesmo valor de
              # modularidade, então escolhe um apenas")
              x = round(n/2)
              tr_choosed = info3[x,]
              df_tr$infoComm_final = rbind(df_tr$infoComm_final, tr_choosed)

            } # fim do if else

          } # fim do if else-if else

        } # fim do else

        # qual é o método escolhido?
        a = toString(tr_choosed$method)

        # pegando os rótulos para criar a partição
        teste = data.frame(filter(com, method==a))

        # gather only the labels and the groups
        labels = teste$labels
        groups = teste$groups
        teste2 = data.frame(labels, groups)

        setwd(FolderPK)
        write.csv(teste2, paste("tr-", j, "-nh-partition.csv", sep=""),
                  row.names = FALSE)
        write.csv(tr_choosed, paste("tr-", j, "-nh-choosed.csv", sep=""),
                  row.names = FALSE)

        j = j + 1
        gc()
      }

      setwd(FolderPartSplit)
      df_tr$infoComm_final = df_tr$infoComm_final[-1,]
      write.csv(df_tr$infoComm_final, paste("fold-", f,
                                            "-tr-nh-choosed.csv", sep=""),
                row.names = FALSE)
      gc()
    } # fim do else

    #f = f + 1
    gc()

  } # fim do while

  gc()

} # fim da função


##################################################################################################
#' MOVE FILES PARTITIONS
#'
#' This function is designed to move partition files generated during the process of partitioning datasets
#' for multiple folds in cross-validation. The function copies the relevant partition files for both
#' k-Nearest Neighbors (KNN) and Threshold methods, as well as the associated hybrid partitions, from
#' their source directories to destination directories for each fold.
#'
#' The function handles both KNN and Threshold partitioning methods. It first checks for the existence of
#' specific CSV files related to KNN and Thresholds and then copies them into the appropriate subdirectories
#' for each fold. Additionally, it creates any necessary directories if they do not already exist.
#'
#' @param dataset_name Name of the dataset to be processed.
#' @param number_folds Number of folds used in cross-validation.
#' @param folderResults Path to the folder where the results will be stored.
#'
#' @return This function does not return any value. It performs file copying operations to organize the
#'         generated partition files into the correct directories.
#'
#' @details
#' The function operates as follows:
#' 1. Creates the necessary directories for each fold.
#' 2. Copies partition files for both KNN and Threshold methods, including hierarchical and non-hierarchical partitions.
#' 3. Checks the existence of certain dataframes before proceeding with the file copying.
#' 4. Ensures that each partition type is copied to the correct fold-specific subdirectory.
#'
#' @examples
#' \dontrun{
#' moveFilesPartitions("dataset1", 5, "/path/to/results")
#' }
#'
#' @seealso
#' \code{\link{chooseNonHierarchical}} for the function that selects non-hierarchical partitions.
#'
#' @import foreach
#' @export
#############
moveFilesPartitions <- function(dataset_name, number_folds, folderResults){

  f = 1
  mfParalel <- foreach(f = 1:number_folds) %dopar% {
    #while(f<=10){

    cat("\nFold ", f)

    folder = createDirs2(dataset_name, folderResults)

    ##################################################
    FolderRepSpl = paste(folder$FolderDataFrame, "/", dataset_name,
                         "/Split-", f, sep="")
    setwd(FolderRepSpl)
    knn = data.frame(read.csv("sparsification-knn-values.csv"))
    n_knn = nrow(knn)

    tr = data.frame(read.csv("sparsification-new-threshold.csv"))
    n_tr = nrow(tr)

    ##################################################
    FolderRepSpl = paste(folder$FolderReport, "/", dataset_name,
                         "/Split-", f, sep="")

    #########################################################
    destino = paste(folder$FolderPartitions, "/", dataset_name, sep="")
    if(dir.exists(destino)==FALSE){dir.create(destino)}

    destino1 = paste(destino, "/Split-", f, sep="")
    if(dir.exists(destino1)==FALSE){dir.create(destino1)}

    comando = paste("cp -r ", paste(FolderRepSpl, "/fold-",
                                    f, "-knn-h-choosed.csv", sep="")
                    , " ", destino1, sep="")
    print(system(comando))

    comando = paste("cp -r ", paste(FolderRepSpl, "/fold-",
                                    f, "-knn-nh-choosed.csv", sep="")
                    , " ", destino1, sep="")
    print(system(comando))


    if(tr[1,2]=="vazio"){
      cat("\n não tem data frame")
    } else {
      comando = paste("cp -r ", paste(FolderRepSpl, "/fold-", f, "-tr-h-choosed.csv", sep="")
                      , " ", destino1, sep="")
      print(system(comando))

      comando = paste("cp -r ", paste(FolderRepSpl, "/fold-", f, "-tr-nh-choosed.csv", sep="")
                      , " ", destino1, sep="")
      print(system(comando))
    }


    ################################333
    # KNN
    i = 1
    while(i<=n_knn){

      cat("\nknn\t", i)

      FolderKnn = paste(FolderRepSpl, "/knn-", i ,sep="")
      setwd(FolderKnn)

      ########################################################################################################################
      #cat("\n Copy to Folder Partition")

      destino2 = paste(destino1, "/knn-", i, sep="")
      if(dir.exists(destino2)==FALSE){dir.create(destino2)}

      origem1 = paste(FolderKnn, "/knn-", i, "-h-partition.csv", sep="")
      origem2 = paste(FolderKnn, "/knn-", i, "-nh-partition.csv", sep="")
      origem3 = paste(FolderKnn, "/knn-", i, "-eb-partitions-hierarchical.csv", sep="")
      origem4 = paste(FolderKnn, "/knn-", i, "-fg-partitions-hierarchical.csv", sep="")
      origem5 = paste(FolderKnn, "/knn-", i, "-wt-partitions-hierarchical.csv", sep="")

      comando = paste("cp -r ", origem1, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem2, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem3, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem4, " ", destino2, sep="")
      print(system(comando))

      comando = paste("cp -r ", origem5, " ", destino2, sep="")
      print(system(comando))

      i = i + 1
      gc()
    } # fim do knn


    #########################################################
    # THRESHOLD
    if(tr[1,2]=="vazio"){
      cat("\n NÃO TEM DATA FRAME")
    } else {

      n_tr = nrow(tr)
      j = 0
      while(j<n_tr){

        cat("\ntr\t", j)

        FolderTr = paste(FolderRepSpl, "/Tr-", j ,sep="")
        setwd(FolderTr)

        ########################################################################################################################
        #cat("\n Copy to Folder Partition")

        destino2 = paste(destino1, "/Tr-", j, sep="")
        if(dir.exists(destino2)==FALSE){dir.create(destino2)}

        origem1 = paste(FolderTr, "/tr-", j, "-h-partition.csv", sep="")
        origem2 = paste(FolderTr, "/tr-", j, "-nh-partition.csv", sep="")
        origem3 = paste(FolderTr, "/tr-", j, "-eb-partitions-hierarchical.csv", sep="")
        origem4 = paste(FolderTr, "/tr-", j, "-fg-partitions-hierarchical.csv", sep="")
        origem5 = paste(FolderTr, "/tr-", j, "-wt-partitions-hierarchical.csv", sep="")

        comando = paste("cp -r ", origem1, " ", destino2, sep="")
        print(system(comando))

        comando = paste("cp -r ", origem2, " ", destino2, sep="")
        print(system(comando))

        comando = paste("cp -r ", origem3, " ", destino2, sep="")
        print(system(comando))

        comando = paste("cp -r ", origem4, " ", destino2, sep="")
        print(system(comando))

        comando = paste("cp -r ", origem5, " ", destino2, sep="")
        print(system(comando))

        j = j + 1
        gc()
      } # fim

    }

    gc()
  }

}

##################################################################################################
#' JOIN DATA FRAMES
#'
#' This function consolidates multiple fold-specific CSV files into two large summary data frames.
#' The function reads KNN (k-Nearest Neighbors) and Threshold (TR) CSV files for each fold and appends
#' them into one combined data frame for each type. The resulting combined data frames are saved as CSV
#' files in the specified folder.
#'
#' The function iterates over the specified number of folds (usually from 1 to 10), reading the KNN and
#' Threshold files generated for each fold, and appending them to the `resumeKNN` and `resumeTR` data frames,
#' respectively. After all folds are processed, the combined data frames are written to CSV files.
#'
#' @param dataset_name Name of the dataset to be processed.
#' @param number_folds Number of folds to process.
#' @param similarity A string indicating the similarity method used in the dataset.
#' @param folderResults Path to the folder where results will be stored.
#'
#' @return This function does not return any value. It writes two CSV files:
#'         "All-Conectado-KNN.csv" and "All-Conectado-TR.csv" containing the combined data.
#'
#' @details
#' The function performs the following steps:
#' 1. Creates necessary directories for the dataset.
#' 2. Loops through each fold, reading the KNN and Threshold CSV files.
#' 3. Appends the data from each fold to the appropriate data frames (`resumeKNN` and `resumeTR`).
#' 4. After processing all folds, writes the combined data frames to CSV files.
#'
#' @examples
#' \dontrun{
#' juntaDFs("dataset1", 10, "cosine", "/path/to/results")
#' }
#'
#' @seealso
#' \code{\link{createDirs2}} for the function that creates the necessary directories.
#'
#' @import data.table
#' @export
##
juntaDFs <- function(dataset_name, number_folds, similarity, folderResults){

  folder = createDirs2(dataset_name, folderResults)
  resumeKNN = data.frame()
  resumeTR = data.frame()

  f = 1
  while(f<=10){
    cat("\nFOLD: ", f)
    ##################################################
    FolderRepSpl = paste(folder$FolderCommunities, "/Split-", f, sep="")
    setwd(FolderRepSpl)

    nome = paste("fold-",f,"-", similarity, "-conectado-knn.csv", sep="")
    resume = data.frame(read.csv(nome))
    resumeKNN = rbind(resumeKNN, resume)

    nome = paste("fold-",f,"-", similarity, "-conectado-trh.csv", sep="")
    resume = data.frame(read.csv(nome))
    resumeTR= rbind(resumeTR, resume)


    f = f + 1
    gc()
  } # fim do while

  setwd(folder$FolderPartitions)
  write.csv(resumeTR, "All-Conectado-TR.csv", row.names = FALSE)
  write.csv(resumeKNN, "All-Conectado-KNN.csv", row.names = FALSE)

}
