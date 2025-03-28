rm(list = ls())


FolderRoot = "~/Generate-Partitions-Communities"
FolderScripts = "~/Generate-Partitions-Communities/R"


###############################################################################
# LOAD LIBRARY/PACKAGE                                                        #
###############################################################################
setwd(FolderScripts)
source("libraries.R")


###############################################################################
# READING DATASET INFORMATION FROM DATASETS-ORIGINAL.CSV                      #
###############################################################################
setwd(FolderRoot)
datasets = data.frame(read.csv("datasets-original.csv"))
n = nrow(datasets)


###############################################################################
# CREATING FOLDER TO SAVE CONFIG FILES                                        #
###############################################################################
FolderCF = paste(FolderRoot, "/config-files", sep = "")
if (dir.exists(FolderCF) == FALSE) {dir.create(FolderCF)}

similarity = c("jaccard", "rogers")
sim = c("j", "ro")

s = 1
while(s<=length(similarity)){

  FolderS = paste(FolderCF, "/", similarity[s], sep = "")
  if (dir.exists(FolderS) == FALSE) {dir.create(FolderS)}

  d = 1
  while (d <= n) {

    ds = datasets[d, ]

    cat("\n\n#===============================================")
    cat("\n# Similarity \t", similarity[s])
    cat("\n# Dataset \t", ds$Name)
    cat("\n#===============================================")

    name = paste(sim[s], "-", ds$Name, sep = "")

    file_name = paste(FolderCF, "/", name, ".csv", sep = "")

    output.file <- file(file_name, "wb")

    write("Config, Value", file = output.file, append = TRUE)

    write("Dataset_Path, ~/Generate-Partitions-Communities/dataset",
          file = output.file, append = TRUE)

    folder_name = paste("/tmp/", name, sep = "")

    str1 = paste("Temporary_Path, ", folder_name, sep = "")
    write(str1, file = output.file, append = TRUE)

    str2 = paste("Graph_Path, ~/Generate-Partitions-Communities/Label-Graphs/", sep = "")
    write(str2, file = output.file, append = TRUE)

    str4 = paste("Similarity, ", similarity[s], sep = "")
    write(str4, file = output.file, append = TRUE)
    
    str4 = paste("Sparsification, 1, ", similarity[s], sep = "")
    write(str4, file = output.file, append = TRUE)

    str3 = paste("Dataset_Name, ", ds$Name, sep = "")
    write(str3, file = output.file, append = TRUE)

    str2 = paste("Number_Dataset, ", ds$Id, sep = "")
    write(str2, file = output.file, append = TRUE)

    write("Number_Folds, 10", file = output.file, append = TRUE)
    write("Number_Cores, 1", file = output.file, append = TRUE)
    write("R_clone, 0", file = output.file, append = TRUE)
    write("Save_csv_files, 1", file = output.file, append = TRUE)
    
    close(output.file)
    d = d + 1
    gc()
  }

  s = s + 1
  gc()
}

rm(list = ls())


