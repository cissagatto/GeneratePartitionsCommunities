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
FolderRoot = "~/Generate-Partitions-Communities"
FolderScripts = "~/Generate-Partitions-Communities/R"


#################################################################
# MAIN FUNCTIOn
#################################################################
execute.run <- function(parameters) {
  
  FolderRoot = "~/Generate-Partitions-Communities"
  FolderScripts = "~/Generate-Partitions-Communities/R"
  
  setwd(FolderScripts)
  source("libraries.R")
  
  setwd(FolderScripts)
  source("utils.R")
  
  setwd(FolderScripts)
  source("communities.R")
  
  setwd(FolderScripts)
  source("functions.R")
  
  setwd(FolderScripts)
  source("choose.R")
  
  retorno = list()
  
  if (parameters$Number.Cores  == 0) {
    cat("\n\n##################################################################################################")
    cat(  "\n# Zero is a disallowed value for number_cores. Please choose a value greater than or equal to 1. #")
    cat("\n##################################################################################################\n\n")
    
  } else {
    
    cl <- parallel::makeCluster(parameters$Number.Cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if (parameters$Number.Cores == 1) {
      cat("\n\n###########################################################")
      cat("\n# RUN: Running Sequentially!                              #")
      cat("\n###########################################################\n\n")
    } else {
      cat(
        "\n\n######################################################################"
      )
      cat("\n# RUN: Running in parallel with ",
          parameters$Number.Cores,
          " cores! #")
      cat(
        "\n######################################################################\n\n"
      )
    }
  }
  
  if(parameters$sparsification == 0){
    
    
    cat("\n\n########################################################")
    cat("  \n# Run: Comunidades                                     #")
    cat("  \n########################################################\n\n")
    timeComm = system.time(res.comm <- execute.communities(parameters))
    return(res.comm)
    # res.comm$Runtime
    # res.comm$Summary
    # res.comm$SpinGlass$FOLD1
    
    
  } else {
    
    cat("\n\n########################################################")
    cat("  \n# Run: sparsification                                  #")
    cat("  \n########################################################\n\n")
    # debug(execute.communities)
    timeSpars = system.time(res.spars <- execute.communities(parameters))
    
    # res.spars$knn$FOLD1$knn1
    # res.spars$tr$FOLD1
    retorno$sparsification = res.spars
    
    
    #cat("\n\n#########################################################")
    #cat("\n# Run: Choose Hierarchical                                #")
    #timeChooseH = system.time(res2 <- chooseHierarchical(parameters))
    
    #cat("\n\n###################################################")
    #cat("\n# Run: Choose Non Hierarchical                             #")
    #timeChooseNH = system.time(res3 <- chooseNonHierarchical(parameters))
    
    #cat("\n\n########################################################")
    #cat("\n# Run: Organizing results                                 #")
    #timeJunta = system.time(res4 <- juntaDFs(parameters))
    
    return(retorno)
  }
  
  cat("\n######################################################################")
  cat("\n# END RUN.R                                                          #")
  cat("\n######################################################################")
  cat("\n\n\n\n")
  
  
} # fim da função



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
