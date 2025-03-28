rm(list=ls())  # Remove all object7s from the environment

cat("\n\n###################################################")
cat(  "\n# START GENERATING PARTITIONS COMMUNITIES         #")
cat(  "\n###################################################\n\n")

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


#' @title Generate Partitions Communities
#' @description This script is responsible for generating partitioned communities using various algorithms.
#' It sets up the environment, loads necessary datasets, processes configurations, and executes the community 
#' detection pipeline.
#' 
#' @details 
#' The script follows these steps:
#' 1. Sets the working directory and loads necessary libraries.
#' 2. Reads datasets and configuration files.
#' 3. Processes datasets and similarity matrices.
#' 4. Executes the graph construction process.
#' 5. Saves runtime and cleans up temporary files.
#' 6. Stores results locally or in the cloud.
#'
#' @author 
#' - Profa. Dra. Elaine Cecilia Gatto (UFLA)
#' - Prof. Dr. Ricardo Cerri (USP)
#' - Prof. Dr. Mauri Ferrandin (UFSC)
#' - Prof. Dr. Alan Demetrius (UFSCar)
#'
#' @copyright 2025
#' @license GNU General Public License v3.0
#' @seealso \code{\link{execute.run}}, \code{\link{save_runtime}}, \code{\link{clean_up}}
#' 
#' @examples 
#' # Run the script:
#' source("generate_partitions_communities.R")
#'


# Setting up the working environment
cat("\n\n#######################################################")
cat("\n# BDFG: SET WORK SPACE                                #")
cat("\n#######################################################\n\n")

FolderRoot = "~/Generate-Partitions-Communities"
FolderScripts = "~/Generate-Partitions-Communities/R"

setwd(FolderScripts)  # Set working directory to script folder
source("libraries.R") # Load required libraries

setwd(FolderScripts)
source("utils.R")  # Load utility functions

setwd(FolderScripts)
source("run.R")  # Load execution functions

# Configuring options
cat("\n\n######################################################")
cat("\n# GPC: OPTIONS CONFIGURATIONS                        #")
cat("\n######################################################\n\n")
set_options()

# Loading datasets
cat("\n\n########################################################")
cat("\n# GPC: READ DATASETS                                   #")
cat("\n########################################################\n\n")
datasets <- load_datasets()

# Getting command-line arguments
cat("\n\n########################################################")
cat("\n# GPC: GET THE ARGUMENTS COMMAND LINE                  #")
cat("\n########################################################\n\n")
args <- commandArgs(TRUE)

# Read configuration file
config_file <- args[1]
# home/cissagatto/Generate-Partitions-Communities/config-files/
# config_file = "~/Generate-Partitions-Communities/config-files/j-emotions.csv"
parameters <- read_config_file(config_file, datasets)

# Ensure the results folder exists
create_folder(parameters$Folder.Results)

# Get necessary directories for the process
cat("\n######################")
cat("\n# GPC: GET DIRECTORIES #")
cat("\n######################\n")
directories <- get_directories(parameters)
parameters$Folders <- directories

# Checking if the dataset tar.gz file exists
cat("\n####################################################################")
cat("\n# GPC: CHECKING THE DATASET TAR.GZ FILE                           #")
cat("\n####################################################################\n\n")
process_dataset(datasets, parameters)

# Checking if the similarity matrices tar.gz file exists
cat("\n####################################################################")
cat("\n# GPC: CHECKING THE SIMILARITIES TAR.GZ FILE                      #")
cat("\n####################################################################\n\n")
process_label_graphs(datasets, parameters)

# Set and retrieve parameters
set.parameters(parameters)
get.parameters()

# Executing the graph construction process
cat("\n\n################################################################")
cat("\n# GPC: EXECUTE                                                 #")
cat("\n################################################################\n\n")
timeGPC <- system.time(resGPC <- execute.run(parameters))

# resGPC$knn$FOLD1$knn1$SpinGlass$resulting.partitions
# resGPC$tr$FOLD2$tr3$EdgeBetweenness$resulting.partitions$partitions

# Save runtime data
cat("\n\n####################################################")
cat("\n# GPC: SAVE RUNTIME                               #")
cat("\n####################################################\n\n")
save_runtime(timeGPC, parameters)

# Save results to the appropriate storage (cloud or local machine)
cat("\n\n###################################################################")
cat("\n# GPC: SAVE TO APPROPRIATE LOCATION (CLOUD OR MACHINE)          #")
cat("\n##################################################################\n\n")
if(parameters$Save.Csv.Files==1){
  save_locally(parameters)  
}


# End process
cat("\n\n####################################################")
cat("\n# GPC: END                                         #")
cat("\n####################################################\n")

rm(list = ls())  # Clean up all objects from the environment

##################################################################################################
# For any errors, please contact: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
