###############################################################################
# #
# #
#                                                                             #
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
# Prof. Dr. Ricardo Cerri                                                     #
#                                                                             #
# Prof. Dr. Mauri Ferrandin                                                   #
#                                                                             #
# Prof. Dr. Alan Demetrius                                                    #
# Federal University of Sao Carlos (UFSCar) Campus Sao Carlos - São Paulo     #
# Computer Department (DC)                                                    # 
#
###############################################################################



##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/Generate-Partitions-Communities"
FolderScripts = "~/Generate-Partitions-Communities/R"



##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                        #
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             #
# delete: if you want, or not, to delete all folders and files generated                         #
##################################################################################################
execute <- function(parameters){

  folder = createDirs2(dataset_name, folderResults)

  FolderRoot = "~/Generate-Partitions-Communities"
  FolderScripts = paste(FolderRoot, "/R", sep="")

  if(parameters$Number.Cores  == 0){
    cat("\n\n##################################################################################################")
    cat("\n# Zero is a disallowed value for number_cores. Please choose a value greater than or equal to 1. #")
    cat("\n##################################################################################################\n\n")
  } else {
    cl <- parallel::makeCluster(parameters$Number.Cores)
    doParallel::registerDoParallel(cl)
    print(cl)

    if(parameters$Number.Cores==1){
      cat("\n\n###########################################################")
      cat("\n# RUN: Running Sequentially!                              #")
      cat("\n###########################################################\n\n")
    } else {
      cat("\n\n######################################################################")
      cat("\n# RUN: Running in parallel with ", parameters$Number.Cores, " cores! #")
      cat("\n######################################################################\n\n")
    }
  }

  retorno = list()

  setwd(FolderScripts)
  source("libraries.R")

  setwd(FolderScripts)
  source("utils.R")

  setwd(FolderScripts)
  source("communities.R")

  setwd(FolderScripts)
  source("functions.R")


  cat("\n\n########################################################")
    cat("\n# Run: Comunidades                                     #")
    cat("\n########################################################\n\n")
  timeC = system.time(res1 <- communities(ds, dataset_name,
                                          number_dataset,
                                          number_folds,
                                          number_cores,
                                          similarity,
                                          folderResults))



  cat("\n\n#########################################################")
  cat("\n# Run: Choose Hierarchical                                #")
  timeChooseH = system.time(res2 <- chooseHierarchical(dataset_name,
                                                       number_folds,
                                                       similarity,
                                                       folderResults))


  cat("\n\n###################################################")
  cat("\n# Run: Choose Non Hierarchical                             #")
  timeChooseNH = system.time(res3 <- chooseNonHierarchical(dataset_name,
                                                           number_folds,
                                                           similarity,
                                                           folderResults))


  cat("\n\n########################################################")
  cat("\n# Run: Organizing results                                 #")
  timeJunta = system.time(res4 <- juntaDFs(dataset_name,
                                           number_folds,
                                           similarity,
                                           folderResults))


  #cat("\n\n################################################################################################")
  #cat("\n# Run:                                                                         #")
  #timePart = system.time(res5 <- moveFilesPartitions(dataset_name, number_folds, FolderResults))
  #cat("\n##################################################################################################\n\n")

  cat("\n\n###########################################################")
  cat("\n#Stop Parallel")
  on.exit(stopCluster(cl))
  cat("\n###########################################################")

  cat("\nsalva")
  setwd(folder$FolderPartitions)
  tempoFinal = rbind(timeC, timeChooseH, timeChooseNH, timeJunta)
  write.csv(tempoFinal, "runtime-2.csv")
  print(tempoFinal)

  cat("\n#############################################################")
  cat("\n#END                                                                                             #")
  cat("\n###########################################################")
  cat("\n\n\n\n")

  cat("\n\n###########################################################")
  cat("\n#Stop Parallel")
  on.exit(stopCluster(cl))
  cat("\n###########################################################\n\n")

  gc()
}


