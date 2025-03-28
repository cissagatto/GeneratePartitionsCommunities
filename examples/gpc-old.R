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

cat("\n##########################################################################")
cat("\n# START!                                                              #")
cat("\n#######################################################################")
cat("\n\n\n\n")

##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
FolderRoot = "~/Generate-Partitions-Communities"
FolderScripts = "~/Generate-Partitions-Communities/R"


cat("\n\n##############################################################")
cat("\n# Generate Partitions communities LOAD SOURCES                                    #")
cat("\n##############################################################\n\n")
setwd(FolderScripts)
source("libraries.R")

setwd(FolderScripts)
source("utils.R")

setwd(FolderScripts)
source("run.R")


cat("\n\n##############################################################")
cat("\n# OPTIONS CONFIGURATIONS                          #")
cat("\n##############################################################\n\n")
options(java.parameters = "-Xmx64g")
options(show.error.messages = TRUE)
options(scipen=20)


cat("\n\n##############################################################")
cat("\n# Generate Partitions communities READ DATASETS                                   #")
cat("\n##############################################################\n\n")
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets-original.csv"))


cat("\n\n##############################################################")
cat("\n# Generate Partitions communities GET THE ARGUMENTS COMMAND LINE                  #")
cat("\n##############################################################\n\n")
args <- commandArgs(TRUE)



#############################################################################
# FIRST ARGUMENT: getting specific dataset information being processed      #
# from csv file                                                             #
#############################################################################

# /home/biomal/Generate-Partitions-Communities/Datasets/GpositiveGO.tar.gz
# config_file = paste(FolderRoot, "/config-files/jaccard/j-GpositiveGO.csv", sep="")


config_file <- args[1]



if(file.exists(config_file)==FALSE){
  cat("\n################################################################")
  cat("\n# Missing Config File! Verify the following path:              #")
  cat("\n# ", config_file, "                                            #")
  cat("\n################################################################\n\n")
  break
} else {
  cat("\n########################################")
  cat("\n# Properly loaded configuration file!  #")
  cat("\n########################################\n\n")
}


cat("\n########################################")
cat("\n# PARAMETERS READ                    #\n")
config = data.frame(read.csv(config_file))
print(config)
cat("\n########################################\n\n")


parameters = list()

# DATASET_PATH
dataset_path = toString(config$Value[1])
dataset_path = str_remove(dataset_path, pattern = " ")
parameters$Path.Dataset = dataset_path

# TEMPORARTY_PATH
folderResults = toString(config$Value[2])
folderResults = str_remove(folderResults, pattern = " ")
parameters$Folder.Results = folderResults

# PARTITIONS_PATH
DataFrameGraphs_Path = toString(config$Value[3])
DataFrameGraphs_Path = str_remove(DataFrameGraphs_Path, pattern = " ")
parameters$DataFrameGraphs_Path = DataFrameGraphs_Path

# SIMILARITY
similarity = toString(config$Value[4])
similarity = str_remove(similarity, pattern = " ")
parameters$Similarity = similarity

# DATASET_NAME
dataset_name = toString(config$Value[5])
dataset_name = str_remove(dataset_name, pattern = " ")
parameters$Dataset.Name = dataset_name

# DATASET_NAME
number_dataset = as.numeric(config$Value[6])
parameters$Number.Dataset = number_dataset

# NUMBER_FOLDS
number_folds = as.numeric(config$Value[7])
parameters$Number.Folds = number_folds

# NUMBER_CORES
number_cores = as.numeric(config$Value[8])
parameters$Number.Cores = number_cores

# DATASET_INFO
ds = datasets[number_dataset,]
parameters$Dataset.Info = ds


###############################################################################
# Creating temporary processing folder                                        #
###############################################################################
if(dir.exists(folderResults) == FALSE) {dir.create(folderResults)}


##################################################################################################
# GET THE DIRECTORIES                                                                            #
##################################################################################################
folder = createDirs2(dataset_name, folderResults)
cat("\n")


###############################################################################
# Copying datasets from ROOT folder on server                                 #
###############################################################################

cat("\n####################################################################")
cat("\n# Checking the dataset tar.gz file                                 #")
cat("\n####################################################################\n\n")
str00 = paste(dataset_path, "/", dataset_name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){

  cat("\n######################################################################")
  cat("\n# The tar.gz file for the dataset to be processed does not exist!    #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str00, "                                  #")
  cat("\n######################################################################\n\n")
  break

} else {

  # COPIANDO
  str01 = paste("cp ", str00, " ", folder$FolderDatasets , sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }

  # DESCOMPACTANDO
  str02 = paste("tar xzf ", folder$FolderDatasets, "/", ds$Name,
                ".tar.gz -C ", folder$FolderDatasets, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }

  # str29 = paste("cp -r ", folder$FolderDatasets, "/", ds$Name,
  #               "/CrossValidation/* ", folder$folderResults,
  #               "/datasets/CrossValidation/", sep="")
  # res=system(str29)
  #if(res!=0){break}else{cat("\ncopiou")}

  # str30 = paste("cp -r ",folder$FolderDatasets, "/", ds$Name,
  #               "/LabelSpace/* ", folder$folderResults,
  #               "/datasets/LabelSpace/", sep="")
  # res=system(str30)
  #if(res!=0){break}else{cat("\ncopiou")}

  # str31 = paste("cp -r ", folder$FolderDatasets, "/", ds$Name,
  #               "/NamesLabels/* ", folder$folderResults,
  #               "/datasets/NamesLabels/", sep="")
  # res=system(str31)
  #if(res!=0){break}else{cat("\ncopiou")}

  # str32 = paste("rm -r ", folder$folderResults,
  #               "/datasets/", ds$Name, sep="")
  # print(system(str32))
  #if(res!=0){break}else{cat("\napagou")}

  #APAGANDO
  str03 = paste("rm ", folder$FolderDatasets, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)

  cat("\n####################################################################")
  cat("\n# tar.gz file of the DATASET loaded correctly!                     #")
  cat("\n####################################################################\n\n")


}


###############################################################################
# Copying DATA FRAMES from ROOT folder on server                               #
###############################################################################

cat("\n####################################################################")
cat("\n# Checking the PARTITIONS tar.gz file                              #")
cat("\n####################################################################\n\n")
str01 = paste(DataFrameGraphs_Path, "/", ds$Name,".tar.gz", sep = "")
str01 = str_remove(str01, pattern = " ")
print(str01)

if(file.exists(str01)==FALSE){

  cat("\n######################################################################")
  cat("\n# The tar.gz file for the partitions to be processed does not exist!    #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str01, "                                  #")
  cat("\n######################################################################\n\n")
  break

} else {

  cat("\n####################################################################")
  cat("\n# tar.gz file of the PARTITION loaded correctly!                   #")
  cat("\n####################################################################\n\n")

  # COPIANDO
  str02 = paste("cp ", str01, " ", folder$FolderDataFrame , sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }

  # DESCOMPACTANDO
  str02 = paste("tar xzf ", folder$FolderDataFrame, "/", ds$Name,
                ".tar.gz -C ", folder$FolderDataFrame, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }

  #APAGANDO
  str03 = paste("rm ", folder$FolderDataFrame, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }

  # str31 = paste("cp -r ", folder$FolderDataFrame, "/", ds$Name,
  #               "/Partitions/* ", folder$folderResults,
  #               "/Partitions/", sep="")
  # res=system(str31)
  #
  # str31 = paste("cp -r ", folder$folderPartitions, "/", ds$Name,
  #               "/Communities/* ", folder$folderResults,
  #               "/Communities/", sep="")
  # res=system(str31)


}



###########################################################################
cat("\nSTART")
timeBTC = system.time(res <- execute(ds, dataset_name, number_dataset,
                                     number_folds, number_cores,
                                     similarity, FolderResults))


###############################################################################
result_set <- t(data.matrix(timeBTC))
setwd(folder$FolderPartitions)
write.csv(result_set, "Runtime.csv")
print(timeBTC)
cat("\n")


###############################################################################
#cat("\n------------------>>>>>>>>>>> Copy to google drive")
#cat("\ncopiando as partições")
#origem1 = paste(folder$FolderPartitions)
#destino1 = paste("cloud:[2022]ResultadosExperimentos/Communities/Partitions/Jaccard/", dataset_name, "/Partitions", sep="")
#comando1 = paste("rclone copy ", origem1, " ", destino1, sep="")
#cat("\n", comando1, "\n")
#a = print(system(comando1))
#a = as.numeric(a)
#if(a != 0){
#  stop("Erro RCLONE")
#  quit("yes")
#}

#cat("\ncopiando as comunidades")
#origem2 = paste(folder$FolderCommunities)
#destino2 = paste("cloud:[2022]ResultadosExperimentos/Communities/Partitions/Jaccard/", dataset_name, "/Communities", sep="")
#comando2 = paste("rclone copy ", origem2, " ", destino2, sep="")
#cat("\n", comando2, "\n")
#b = print(system(comando2))
#b = as.numeric(b)
#if(b != 0){
#  stop("Erro RCLONE")
#  quit("yes")
#}


print(system(paste("rm -r ", folder$FolderResults, "/Datasets", sep="")))
print(system(paste("rm -r ", folder$FolderResults, "/DataFrames", sep="")))


cat("\n\n###################################################################")
cat("\n# ====> GPC: COMPRESS RESULTS                                    #")
cat("\n#####################################################################\n\n")
str3 = paste("tar -zcf ", folder$FolderResults, "/",
             dataset_name, "-", similarity, "-results-gpc.tar.gz ",
             folder$FolderResults, "/", sep="")
print(system(str3))


cat("\n\n###################################################################")
cat("\n# ====> GPC: COPY TO HOME                                     #")
cat("\n#####################################################################\n\n")

str0 = "~/Generate-Partitions-Communities/Reports/"
if(dir.exists(str0)==FALSE){dir.create(str0)}

str1 = paste(str0, ds$Name, sep="")
if(dir.exists(str1)==FALSE){dir.create(str1)}

str2 = paste(str1, "/", similarity, sep="")
if(dir.exists(str2)==FALSE){dir.create(str2)}

str3 = paste(folder$FolderResults, "/",
             dataset_name, "-", similarity,
             "-results-gpc.tar.gz ", sep="")

str4 = paste("cp ", str3, " ", str2, sep="")
print(system(str4))


##############################################################################
cat("\nDelete Folder Results (dev, scracth, lustre?)")
str3 = paste("rm -r ", folderResults, sep="")
print(system(str3))


###########################################################################
rm(list = ls())
gc()


